use super::{LongState, Scanner, ScannerStatus, Token};

use crate::command::MacroCall;
use crate::eqtb::{ControlSequence, Eqtb, IntegerVariable};
use crate::logger::Logger;
use crate::macros::{macro_show, Macro, ParamToken};
use crate::print::Printer;
use crate::token_lists::{show_token_list, RcTokenList};

use std::rc::Rc;

enum MacroCallError {
    PrefixMismatch,
    IllegalPar,
}

/// See 389.
pub fn macro_expand(
    macro_call: MacroCall,
    token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let save_scanner_status =
        std::mem::replace(&mut scanner.scanner_status, ScannerStatus::Matching);
    let save_warning_index = scanner.warning_index;

    let Token::CSToken { cs } = token else {
        panic!("Impossible");
    };
    scanner.warning_index = cs;
    let is_long_call = macro_call.long;
    let macro_def = macro_call.macro_def;
    let mut pstack = Vec::new();
    if eqtb.integer(IntegerVariable::TracingMacros) > 0 {
        show_text_of_macro_being_expanded(cs, &macro_def, eqtb, logger);
    }
    // If the macro expects some form of parameter.
    if macro_def.parameter_text[0] != ParamToken::End {
        match scan_parameters(
            &macro_def.parameter_text,
            &mut pstack,
            cs,
            is_long_call,
            scanner,
            eqtb,
            logger,
        ) {
            Ok(_) => {}
            Err(_) => {
                scanner.scanner_status = save_scanner_status;
                scanner.warning_index = save_warning_index;
                return;
            }
        }
    }
    feed_macro_body_and_parameters_to_scanner(cs, macro_def, pstack, scanner, eqtb, logger);
    scanner.scanner_status = save_scanner_status;
    scanner.warning_index = save_warning_index;
}

/// Pushes the parameters onto the parameter stack and the body onto the input stack.
/// See 390.
fn feed_macro_body_and_parameters_to_scanner(
    cs: ControlSequence,
    macro_def: Rc<Macro>,
    parameters: Vec<RcTokenList>,
    scanner: &mut Scanner,
    eqtb: &Eqtb,
    logger: &mut Logger,
) {
    // Remove finished token lists from stack to avoid stack overflow for tail-calling
    // macros.
    scanner.input_stack.remove_finished_token_lists();

    scanner
        .input_stack
        .begin_macro_call(cs, macro_def, parameters, eqtb, logger);
}

/// Return Err for goto exit, Ok(end_token_position) for continue.
/// `cur_token` points to the first token of the parameter text,
/// `pstack` is empty when this function is called.
/// See 391.
fn scan_parameters(
    parameter_text: &[ParamToken],
    pstack: &mut Vec<RcTokenList>,
    cs: ControlSequence,
    is_long_call: bool,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Result<(), MacroCallError> {
    // We no longer need to worry about detecing an
    // outer macro, so we only care about whether it is long or not.
    scanner.long_state = if is_long_call {
        LongState::LongCall
    } else {
        LongState::Call
    };

    // Scan a potential prefix delimiter.
    let mut pos = if let ParamToken::Match(_) = parameter_text[0] {
        0
    } else {
        scan_prefix(parameter_text, scanner, eqtb, logger)?
    };

    // Match the input tokens to the tokens in the macro parameter list.
    while let ParamToken::Match(match_chr) = parameter_text[pos] {
        pos += 1;
        let m = scan_a_parameter(parameter_text, &mut pos, cs, scanner, eqtb, logger)?;
        tidy_up_parameter_just_scanned(m, pstack, match_chr, scanner, eqtb, logger);

        // If we reached the end-match token, we are done.
        if parameter_text[pos] == ParamToken::End {
            break;
        }
    }
    Ok(())
}

/// This function is called to scan a leading delimiter.
/// It returns the position of the first match token or an Error in case that the prefixes did not
/// match.
/// See 392.
fn scan_prefix(
    parameter_text: &[ParamToken],
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Result<usize, MacroCallError> {
    scanner.argument = Vec::new();
    let mut pos = 0;
    let mut input_token = scanner.get_token(eqtb, logger);
    while ParamToken::Normal(input_token) == parameter_text[pos] {
        pos += 1;
        // The prefix has been completely matched.
        if let ParamToken::Match(_) | ParamToken::End = parameter_text[pos] {
            // If the last matching character has been a left brace, we must
            // have the case of a macro where parameter list ends with #{.
            // The left brace here is consumed, so it should not affect the
            // align state.
            if input_token.is_left_brace() {
                scanner.align_state -= 1;
            }
            return Ok(pos);
        }

        // Try to match next pair.
        input_token = scanner.get_token(eqtb, logger);
    }
    // We were not able to completely match the prefix.
    Err(report_improper_use_of_the_macro(scanner, eqtb, logger))
}

/// When this function is called, pos points to the token in the macro token list
/// right after the current match token.
/// Returns an Err for goto exit or Ok for continue.
/// See 392. and 394.
fn scan_a_parameter(
    parameter_text: &[ParamToken],
    pos: &mut usize,
    cs: ControlSequence,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Result<usize, MacroCallError> {
    scanner.argument = Vec::new();
    let delimiter_start = *pos;
    let mut m = 0;
    loop {
        let input_token = scanner.get_token(eqtb, logger);
        // This means that we have a match on the delimiter.
        if ParamToken::Normal(input_token) == parameter_text[*pos] {
            *pos += 1;
            if let ParamToken::Match(_) | ParamToken::End = parameter_text[*pos] {
                // If the last matching character has been a left brace,
                // we must have the case of a macro where the parameter list ends
                // with #{. The left brace here is consumed, so it should not affect
                // the align state.
                if input_token.is_left_brace() {
                    scanner.align_state -= 1;
                }
                break;
            }
            continue;
        }
        // If we have matched at least one token of the delimiter or we are scanning the prefix.
        if delimiter_start != *pos {
            match contribute_recently_matched_tokens_to_current_parameter(
                parameter_text,
                input_token,
                delimiter_start,
                pos,
                &mut m,
                scanner,
            ) {
                false => continue,
                true => {}
            }
        }
        // When we reach this point, we know that the current token does not belong to the
        // delimiter.
        if input_token == eqtb.par_token && scanner.long_state != LongState::LongCall {
            return Err(report_runaway_argument(
                input_token,
                0,
                scanner,
                eqtb,
                logger,
            ));
        }
        if input_token.is_left_brace() {
            contribute_entire_group_to_current_parameter(input_token, scanner, eqtb, logger)?;
        } else if input_token.is_right_brace() {
            report_extra_right_brace(input_token, cs, scanner, eqtb, logger)?;
            continue;
        // If input_token is not a brace.
        } else if !store_current_token(parameter_text, input_token, *pos, scanner) {
            continue;
        }
        m += 1;
        let token = parameter_text[*pos];
        if let ParamToken::Match(_) | ParamToken::End = token {
            break;
        }
    }
    Ok(m)
}

/// Store the current token in the current argument unless it is a space token that would make
/// up an entire undelimited argument.
/// See 393.
fn store_current_token(
    parameter_text: &[ParamToken],
    input_token: Token,
    pos: usize,
    scanner: &mut Scanner,
) -> bool {
    // Ignore spaces that precede a match.
    // NOTE: While it is strange that this does not test for all types of space tokens, it's how
    // TeX82 does it.
    if input_token == Token::SPACE_TOKEN {
        if let ParamToken::Match(_) | ParamToken::End = parameter_text[pos] {
            return false;
        }
    }
    scanner.argument.push(input_token);
    true
}

/// See 395.
fn report_extra_right_brace(
    right_brace_token: Token,
    cs: ControlSequence,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Result<(), MacroCallError> {
    scanner.back_input(right_brace_token, eqtb, logger);
    logger.print_err("Argument of ");
    cs.sprint_cs(eqtb, logger);
    logger.print_str(" has an extra }");
    let help = &[
        "I've run across a `}' that doesn't seem to match anything.",
        "For example, `\\def\\a#1{...}' and `\\a}' would produce",
        "this error. If you simply proceed now, the `\\par' that",
        "I've just inserted will cause me to report a runaway",
        "argument that might be the root of the problem. But if",
        "your `}' was spurious, just type `2' and it will go away.",
    ];
    scanner.align_state += 1;
    scanner.long_state = LongState::Call;
    scanner.ins_error(eqtb.par_token, help, eqtb, logger);
    Ok(())
}

/// See 396.
fn report_runaway_argument(
    token: Token,
    unbalance: i32,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> MacroCallError {
    if scanner.long_state == LongState::Call {
        scanner.runaway(eqtb, logger);
        logger.print_err("Paragraph ended before ");
        scanner.warning_index.sprint_cs(eqtb, logger);
        logger.print_str(" was complete");
        let help = &[
            "I suspect you've forgotten a `}', causing me to apply this",
            "control sequence to too much text. How can we recover?",
            "My plan is to forget the whole thing and hope for the best.",
        ];
        scanner.back_error(token, help, eqtb, logger);
    }
    scanner.align_state -= unbalance;
    MacroCallError::IllegalPar
}

/// Returns false if we found a shifted match that we want to extend (goto continue),
/// or true if no match is possible with what we have scanned so far.
/// NOTE: \par tokens could be silently added to the argument here but its not
///       worth it to watch out for that case.
/// See 397.
fn contribute_recently_matched_tokens_to_current_parameter(
    parameter_text: &[ParamToken],
    input_token: Token,
    delim_pos: usize,
    pos: &mut usize,
    m: &mut usize,
    scanner: &mut Scanner,
) -> bool {
    // NOTE The scanned tokens correspond to the tokens in parameter_text[delim_pos..pos] plus
    // input_token. We also know that input_token != parameter_text[pos].

    // This denotes the index of the last input token for which we already know that it
    // does not match with the delimiter.
    let mut t = delim_pos;
    while t < *pos {
        let ParamToken::Normal(nonmatching_token) = parameter_text[t] else {
            panic!("It can only be a normal Token");
        };
        // Add the non-matching token to the parameter.
        scanner.argument.push(nonmatching_token);
        *m += 1;
        // Attempt to match from the token following the t token.
        // `u` corresponds to what input, `v` corresponds to the token being matched
        // against.
        let mut u = t + 1;
        let mut v = delim_pos;
        loop {
            // If we reached the end of the previous longest match.
            if u == *pos {
                // If this match attempt ends here as well.
                if ParamToken::Normal(input_token) != parameter_text[v] {
                    break;
                // If this match still fits we need to move out to retrieve
                // more input tokens.
                } else {
                    *pos = v + 1;
                    return false;
                }
            }
            // If the input token corresponging to `u` does not match with `v`,
            // this matching attempt is over.
            if parameter_text[u] != parameter_text[v] {
                break;
            }
            // Move forwards in both lists.
            u += 1;
            v += 1;
        }
        // The matching attempt failed, so we move on to the next token.
        t += 1;
    }
    // Start matching at the start again.
    *pos = delim_pos;
    true
}

/// See 398.
fn report_improper_use_of_the_macro(
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> MacroCallError {
    logger.print_err("Use of ");
    scanner.warning_index.sprint_cs(eqtb, logger);
    logger.print_str(" doesn't match its definition");
    let help = &[
        "If you say, e.g., `\\def\\a1{...}', then you must always",
        "put `1' after `\\a', since control sequence names are",
        "made up of letters only. The macro here has not been",
        "followed by the required stuff, so I'm ignoring it.",
    ];
    logger.error(help, scanner, eqtb);
    MacroCallError::PrefixMismatch
}

/// Adds an entire group to the currently scanned parameter.
/// Return an error if an illegal \par token appears.
/// See 399.
fn contribute_entire_group_to_current_parameter(
    mut input_token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Result<(), MacroCallError> {
    let mut unbalance = 1;
    loop {
        scanner.argument.push(input_token);
        input_token = scanner.get_token(eqtb, logger);
        if input_token == eqtb.par_token && scanner.long_state != LongState::LongCall {
            return Err(report_runaway_argument(
                input_token,
                unbalance,
                scanner,
                eqtb,
                logger,
            ));
        }
        if input_token.is_left_brace() {
            unbalance += 1;
        } else if input_token.is_right_brace() {
            unbalance -= 1;
            if unbalance == 0 {
                break;
            }
        }
    }
    scanner.argument.push(input_token);
    Ok(())
}

/// See 400.
fn tidy_up_parameter_just_scanned(
    m: usize,
    pstack: &mut Vec<RcTokenList>,
    match_chr: u8,
    scanner: &mut Scanner,
    eqtb: &Eqtb,
    logger: &mut Logger,
) {
    let mut parameter = std::mem::take(&mut scanner.argument);
    // If a single group has been scanned, we want to strip the enclosing braces.
    if m == 1
        && (parameter
            .last()
            .expect("We just checked that at least one token was contributed")
            .is_right_brace())
    {
        parameter.remove(0);
        parameter.pop();
    }
    pstack.push(std::rc::Rc::new(parameter));
    if eqtb.integer(IntegerVariable::TracingMacros) > 0 {
        logger.begin_diagnostic(eqtb.tracing_online());
        logger.print_nl(match_chr);
        logger.print_int(pstack.len() as i32);
        logger.print_str("<-");
        let list = &pstack[pstack.len() - 1];
        show_token_list(list, 1000, logger, eqtb);
        logger.end_diagnostic(false);
    }
}

/// See 401.
fn show_text_of_macro_being_expanded(
    cs: ControlSequence,
    macro_def: &Macro,
    eqtb: &Eqtb,
    logger: &mut Logger,
) {
    logger.begin_diagnostic(eqtb.tracing_online());
    logger.print_ln();
    cs.print_cs(eqtb, logger);
    macro_show(macro_def, logger, eqtb);
    logger.end_diagnostic(false);
}
