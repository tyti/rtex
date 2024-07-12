use super::conditional::{conditional, terminate_current_conditional_and_skip_to_fi};
use super::macro_expand::macro_expand;
use super::{Scanner, TokenSourceType};
use crate::command::{Command, ExpandableCommand, MarkCommand, UnexpandableCommand};
use crate::eqtb::{ControlSequence, ControlSequenceId, Eqtb, IntegerVariable};
use crate::error::overflow;
use crate::logger::Logger;
use crate::print::Printer;
use crate::token::Token;
use crate::token_lists::{conv_toks, ins_the_toks};

/// Expands the current command.
/// NOTE: Expects that the command code is larger than MAX_COMMAND
/// and less than DONT_EXPAND.
/// See 366. and 367.
pub fn expand(
    expandable_command: ExpandableCommand,
    token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    if eqtb.integer(IntegerVariable::TracingCommands) > 1 {
        if let ExpandableCommand::Macro(_) | ExpandableCommand::EndTemplate = expandable_command {
            // These commands are not traced.
        } else {
            show_expandable(&expandable_command, scanner, eqtb, logger);
        }
    }
    match expandable_command {
        ExpandableCommand::Undefined => complain_about_undefined_macro(scanner, eqtb, logger),
        ExpandableCommand::Macro(macro_call) => {
            macro_expand(macro_call, token, scanner, eqtb, logger)
        }
        ExpandableCommand::ExpandAfter => expand_token_after_next_token(scanner, eqtb, logger),
        ExpandableCommand::NoExpand => scanner.suppress_expansion_of_next_token(eqtb, logger),
        ExpandableCommand::IfTest(if_test) => conditional(if_test, scanner, eqtb, logger),
        ExpandableCommand::FiOrElse(fi_or_else) => {
            terminate_current_conditional_and_skip_to_fi(fi_or_else, token, scanner, eqtb, logger)
        }
        ExpandableCommand::CsName => manufacture_control_sequence_name(scanner, eqtb, logger),
        ExpandableCommand::Convert(convert_command) => {
            conv_toks(convert_command, scanner, eqtb, logger)
        }
        ExpandableCommand::The => ins_the_toks(scanner, eqtb, logger),
        ExpandableCommand::Mark(mark_command) => {
            insert_appropriate_mark_text_into_scanner(mark_command, scanner, eqtb, logger)
        }
        ExpandableCommand::EndTemplate => {
            insert_token_containing_frozen_endv(scanner, eqtb, logger)
        }
        ExpandableCommand::Input => initiate_input_from_file(token, scanner, eqtb, logger),
        ExpandableCommand::EndInput => scanner.end_input_from_current_file(),
    }
}

/// Expand the next token after expanding the one following it.
/// See 368.
fn expand_token_after_next_token(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) {
    // Get the first token and save it.
    let first_token = scanner.get_token(eqtb, logger);
    // Get the second token.
    let (second_command, second_token) = scanner.get_command_and_token(eqtb, logger);
    // Expand the second token if appropriate.
    match second_command {
        Command::Expandable(expandable_command) => {
            expand(expandable_command, second_token, scanner, eqtb, logger)
        }
        Command::Unexpandable(_) => scanner.back_input(second_token, eqtb, logger),
    }
    // Push the first, unexpanded token in front again.
    scanner.back_input(first_token, eqtb, logger);
}

/// See 370.
fn complain_about_undefined_macro(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) {
    logger.print_err("Undefined control sequence");
    let help = &[
        "The control sequence at the end of the top line",
        "of your error message was never \\def'ed. If you have",
        "misspelled it (e.g., `\\hobx'), type `I' and the correct",
        "spelling (e.g., `I\\hbox'). Otherwise just continue,",
        "and I'll forget about whatever was undefined.",
    ];
    logger.error(help, scanner, eqtb)
}

/// Reads a control sequence name enclosed by \csname and \endcsname
/// in and replaces it by the corresponding control sequence token.
/// See 372.
fn manufacture_control_sequence_name(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) {
    // Collect the token in `name`.
    let mut cs_name = Vec::new();
    // Consume all char tokens after expansion.
    let (finishing_command, finishing_token) = loop {
        let (command, token) = get_x_token(scanner, eqtb, logger);
        match token {
            Token::LeftBrace(c)
            | Token::RightBrace(c)
            | Token::MathShift(c)
            | Token::TabMark(c)
            | Token::MacParam(c)
            | Token::SuperMark(c)
            | Token::SubMark(c)
            | Token::Spacer(c)
            | Token::Letter(c)
            | Token::OtherChar(c) => {
                cs_name.push(c);
            }
            token @ Token::CSToken { .. } => break (command, token),
            Token::Null => {
                panic!("Should not appear here")
            }
        }
    };
    // Complain if the ending control sequence is not endcsname.
    if let UnexpandableCommand::EndCsName = finishing_command {
        // Do nothing.
    } else {
        complain_about_missing_endcsname(finishing_token, scanner, eqtb, logger);
    }
    // Look up the name and return the corresponding control sequence.
    let Ok(cs) = eqtb.lookup_or_create(&cs_name) else {
        overflow(
            "hash size",
            ControlSequenceId::MAX as usize,
            &scanner.input_stack,
            eqtb,
            logger,
        );
    };

    // An undefined csname is set to be equivalent to \relax
    if let Command::Expandable(ExpandableCommand::Undefined) = eqtb.control_sequences.get(cs) {
        let command = Command::Unexpandable(UnexpandableCommand::Relax { no_expand: false });
        eqtb.cs_define(cs, command, false);
    }
    let token = Token::CSToken { cs };
    scanner.back_input(token, eqtb, logger);
}

/// See 373.
fn complain_about_missing_endcsname(
    token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    logger.print_err("Missing ");
    logger.print_esc_str(b"endcsname");
    logger.print_str(" inserted");
    let help = &[
        "The control sequence marked <to be read again> should",
        "not appear between \\csname and \\endcsname.",
    ];
    scanner.back_error(token, help, eqtb, logger)
}

/// See 375.
fn insert_token_containing_frozen_endv(
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let token = Token::CSToken {
        cs: ControlSequence::FrozenEndv,
    };
    scanner.back_input(token, eqtb, logger)
}

/// See 378.
fn initiate_input_from_file(
    token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    // Don't allow an \input command while scanning a file name.
    if scanner.name_in_progress {
        scanner.insert_relax(token, eqtb, logger)
    } else {
        scanner.start_input(eqtb, logger)
    }
}

/// See 380.
pub fn get_x_token(
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> (UnexpandableCommand, Token) {
    loop {
        let (command, token) = scanner.get_next(false, eqtb, logger);
        match command {
            Command::Unexpandable(unexpandable_command) => return (unexpandable_command, token),
            Command::Expandable(ExpandableCommand::Macro(macro_call)) => {
                macro_expand(macro_call, token, scanner, eqtb, logger)
            }
            Command::Expandable(ExpandableCommand::EndTemplate) => {
                let command = UnexpandableCommand::Endv;
                let token = Token::CSToken {
                    cs: ControlSequence::FrozenEndv,
                };
                return (command, token);
            }
            Command::Expandable(expandable_command) => {
                expand(expandable_command, token, scanner, eqtb, logger);
            }
        }
    }
}

/// Keeps expanding as long as possible and returns the first unexpandable token.
/// See 381.
pub fn x_token(
    mut command: Command,
    mut token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> (UnexpandableCommand, Token) {
    loop {
        match command {
            Command::Unexpandable(unexpandable_command) => return (unexpandable_command, token),
            Command::Expandable(expandable_command) => {
                expand(expandable_command, token, scanner, eqtb, logger);
                (command, token) = scanner.get_next(false, eqtb, logger);
            }
        }
    }
}

/// See 386.
fn insert_appropriate_mark_text_into_scanner(
    mark_command: MarkCommand,
    scanner: &mut Scanner,
    eqtb: &Eqtb,
    logger: &mut Logger,
) {
    let mark = eqtb.marks.get(mark_command);
    if let Some(token_list) = mark {
        scanner.input_stack.begin_token_list(
            token_list.clone(),
            TokenSourceType::MarkText,
            eqtb,
            logger,
        );
    }
}

/// Prints the given command code and character together with the mode.
/// See 299.
fn show_expandable(
    expandable_command: &ExpandableCommand,
    scanner: &Scanner,
    eqtb: &Eqtb,
    logger: &mut Logger,
) {
    logger.begin_diagnostic(eqtb.tracing_online());
    logger.print_nl_str("{");
    if scanner.scanning_write_tokens {
        if logger.shown_mode.is_some() {
            logger.print_str("no mode: ");
            logger.shown_mode = None;
        }
    } else if Some(eqtb.mode()) != logger.shown_mode {
        eqtb.mode().display(logger);
        logger.print_str(": ");
        logger.shown_mode = Some(eqtb.mode());
    }
    expandable_command.display(logger);
    logger.print_char(b'}');
    logger.end_diagnostic(false);
}
