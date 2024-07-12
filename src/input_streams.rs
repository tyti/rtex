use crate::eqtb::{ControlSequence, Eqtb};
use crate::error::fatal_error;
use crate::input::{Scanner, ScannerStatus};
use crate::logger::{InteractionMode, Logger};
use crate::macros::{Macro, MacroToken, ParamToken};
use crate::print::Printer;
use crate::read_line;
use crate::token::Token;

/// Reads a line from stream `n` as token list intended to be stored in control sequence
/// `target_cs`. Returns pointer to reference count.
/// See 482.
pub fn read_toks(
    stream_number: i32,
    target_cs: ControlSequence,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Macro {
    scanner.scanner_status = ScannerStatus::Defining;
    scanner.warning_index = target_cs;
    scanner.macro_def = Macro::default();
    scanner.macro_def.parameter_text.push(ParamToken::End);
    // For a non-negative stream_number, a prompt will be given to the user in
    // case that terminal input is used.
    let mut prompt_for_input = stream_number >= 0;
    // Only stream numbers 0 to 15 correspond to a real stream. All other correspond to the
    // terminal.
    let stream_number = if stream_number < 0 || stream_number > 15 {
        None
    } else {
        Some(stream_number as u8)
    };
    let saved_align_state = scanner.align_state;
    scanner.align_state = 1_000_000;
    // Keep reading lines from the stream until the braces are balanced.
    loop {
        input_and_store_tokens_from_next_line_of_file(
            stream_number,
            prompt_for_input,
            target_cs,
            scanner,
            eqtb,
            logger,
        );
        if scanner.align_state == 1_000_000 {
            break;
        }
        // We do not want a prompt again for multi-line inputs.
        prompt_for_input = false;
    }
    scanner.align_state = saved_align_state;
    scanner.scanner_status = ScannerStatus::Normal;
    std::mem::take(&mut scanner.macro_def)
}

/// See 483. and 486.
fn input_and_store_tokens_from_next_line_of_file(
    stream_number: Option<u8>,
    prompt_for_input: bool,
    target_cs: ControlSequence,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let stream_file = match stream_number {
        None => None,
        Some(n) => scanner.read_file[n as usize].as_mut().map(|file| (file, n)),
    };

    // Get the next line from either terminal or file.
    let (mut line, file_ended) = match stream_file {
        None => {
            let line =
                input_for_read_from_terminal(prompt_for_input, target_cs, scanner, eqtb, logger);
            (line, false)
        }
        Some((read_file, n)) => {
            let mut line = Vec::new();
            match read_line(read_file, &mut line) {
                // The file has been exhaused.
                false => {
                    scanner.read_file[n as usize] = None;
                    // A closing file is implicitly assumed to provide an empty line.
                    (Vec::new(), true)
                }
                true => (line, false),
            }
        }
    };

    if !eqtb.end_line_char_inactive() {
        line.push(eqtb.end_line_char() as u8);
    }
    scanner
        .input_stack
        .input_from_stream_line(stream_number, line, eqtb, logger);

    if file_ended && scanner.align_state != 1_000_000 {
        scanner.runaway(eqtb, logger);
        logger.print_err("File ended within ");
        logger.print_esc_str(b"read");
        let help = &["This \\read has unbalanced braces."];
        scanner.align_state = 1_000_000;
        logger.error(help, scanner, eqtb);
    }

    loop {
        let token = scanner.get_token(eqtb, logger);
        if token == Token::Null {
            break;
        }
        // If the line contains a closing brace without a matching preceding opening brace,
        // the rest of the line including the brace is silently ignored.
        if scanner.align_state < 1_000_000 {
            loop {
                let token = scanner.get_token(eqtb, logger);
                if token == Token::Null {
                    break;
                }
            }
            scanner.align_state = 1_000_000;
            break;
        }
        scanner
            .macro_def
            .replacement_text
            .push(MacroToken::Normal(token));
    }
    scanner.input_stack.pop_input();
}

// 484.
/// See 484.
fn input_for_read_from_terminal(
    prompt_for_input: bool,
    target_cs: ControlSequence,
    scanner: &Scanner,
    eqtb: &Eqtb,
    logger: &mut Logger,
) -> Vec<u8> {
    if logger.interaction == InteractionMode::Scroll
        || logger.interaction == InteractionMode::ErrorStop
    {
        // We print a prompt only for the first line.
        if !prompt_for_input {
            logger.prompt_input("", &scanner.input_stack, eqtb)
        // We do print a prompt.
        } else {
            logger.print_ln();
            target_cs.sprint_cs(eqtb, logger);
            logger.prompt_input("=", &scanner.input_stack, eqtb)
        }
    } else {
        fatal_error(
            "*** (cannot \\read from terminal in nonstop modes)",
            &scanner.input_stack,
            eqtb,
            logger,
        );
    }
}
