//! # An implementation of TeX82 in Rust
mod alignment;
mod box_building;
mod command;
mod dimension;
mod dvi;
mod eqtb;
mod error;
mod fonts;
mod format;
mod glue;
mod horizontal_mode;
mod hyphenation;
mod input;
mod input_streams;
mod integer;
mod line_breaking;
mod logger;
mod macros;
mod main_control;
mod main_loop;
mod math;
mod math_mode;
mod mode_independent;
mod nodes;
mod output;
mod packaging;
mod page_breaking;
mod print;
mod scaled;
mod scan_boxes;
mod scan_internal;
mod semantic_nest;
mod token;
mod token_lists;
mod vertical_mode;
mod vsplit;
mod write_streams;

use alignment::AlignState;
use eqtb::{CatCode, Eqtb, IntegerVariable};
use format::{
    load_hyphenator_and_eqtb_from_default_format_file,
    load_hyphenator_and_eqtb_from_specified_format_file, store_fmt_file,
};
use hyphenation::Hyphenator;
use input::input_stack::INPUT_STACK_SIZE;
use input::Scanner;
use logger::{wlog, wlog_ln, History, InteractionMode, Logger, BANNER};
use main_control::main_control;
use output::Output;
use page_breaking::PageBuilder;
use print::Printer;
use semantic_nest::SemanticState;

use std::ffi::OsString;
use std::fs::File;
use std::fs::OpenOptions;
use std::io::{BufRead, Write};
use std::os::unix::ffi::OsStringExt;
use std::path::Path;

// We use a global flag for now to indicate whether this version is INITEX.
const INIT: bool = true;

/// This is the entry point into the TeX program.
/// See 1332.
pub fn tex_main() -> Result<(), ()> {
    if INIT {
        println!("{BANNER} (INITEX)");
    } else {
        println!("{BANNER} (no format preloaded)");
    }

    // Start the input
    let (format_name, mut first_line, first_non_space_pos) = read_format_name_and_first_line();

    let (mut logger, mut hyphenator, mut eqtb) = if let Some(format_name) = format_name {
        load_hyphenator_and_eqtb_from_specified_format_file(format_name)?
    // If no format has been loaded.
    } else if !INIT {
        load_hyphenator_and_eqtb_from_default_format_file()?
    } else {
        let mut eqtb = Box::new(Eqtb::new());
        eqtb.init_prim();
        let hyphenator = Hyphenator::new();
        let logger = Logger::new(" (INITEX)".to_string(), InteractionMode::ErrorStop);
        (logger, hyphenator, eqtb)
    };

    // We set history to this value before initialization
    // and set it to spotless once initialization succeeded.
    logger.history = History::ErrorMessageIssued;

    // If the remaining input line is non-empty and does not start with an escape
    // sequence, use the first word as input file. This should behave the same
    // as having an implicit "\input " at the beginning of the line.
    let start_with_input = first_non_space_pos < first_line.len()
        && eqtb.cat_code(first_line[first_non_space_pos]) != CatCode::Escape;

    // Initialize global variables and data structures.
    let mut output = Output::new();
    let mut page_builder = PageBuilder::new();
    let mut nest = SemanticState::new();

    // Here we add an end_line_char to the current line if it is defined.
    if !eqtb.end_line_char_inactive() {
        first_line.push(eqtb.end_line_char() as u8);
    }

    eqtb.fix_date_and_time();
    logger.initialize_print_selector_based_on_interaction();

    // From 331.
    // Initialize Scanner stuff
    let mut scanner = Scanner::new(first_line, first_non_space_pos);

    if start_with_input {
        scanner.start_input(&mut eqtb, &mut logger);
    }

    // Now we are done with the initialization and can get started for real.
    logger.history = History::Spotless;

    let mut align_state = AlignState::new();

    let dumping = main_control(
        &mut align_state,
        &mut hyphenator,
        &mut page_builder,
        &mut output,
        &mut nest,
        &mut scanner,
        &mut eqtb,
        &mut logger,
    );

    // Now we are done and only need to clean-up after ourselves.
    final_cleanup(dumping, &hyphenator, &mut scanner, &mut eqtb, &mut logger);
    close_files_and_terminate(0, &hyphenator, &nest, output, &scanner, &eqtb, &mut logger);
}

/// Read the first line, and look for a format name.
/// See 1337.
fn read_format_name_and_first_line() -> (Option<OsString>, Vec<u8>, usize) {
    // From 331.
    // Get the first line.
    let (first_line, first_non_space_pos) = get_first_input_line();

    let mut pos = first_non_space_pos;
    // If line starts with '&', retrieve the format file name.
    let format_name = if first_line[first_non_space_pos] == b'&' {
        // Jump over the ampersand.
        pos += 1;
        // Determine the format file string.
        let start = pos;
        while pos < first_line.len() && first_line[pos] != b' ' {
            pos += 1;
        }
        let end = pos;
        // Move forward to the next non-space character
        while pos < first_line.len() && first_line[pos] == b' ' {
            pos += 1;
        }
        Some(OsString::from_vec(first_line[start..end].to_vec()))
    } else {
        None
    };
    (format_name, first_line, pos)
}

/// Gets the first input line and first non-space index.
///
/// If there have been command line arguments, those are taken as first input line.
/// Otherwise the user is asked to input a line not only containing spaces until
/// the user complies or closes stdin (in which case we shut down).
/// See 37.
pub fn get_first_input_line() -> (Vec<u8>, usize) {
    // If we have command line arguments, we take those as the first line of input instead of
    // interactively asking the user for input.
    let args = std::env::args();
    if args.len() > 1 {
        // NOTE We do not check for null bytes and new lines.
        let input_line: Vec<u8> = args
            .skip(1)
            .map(|arg| arg.to_string())
            .collect::<Vec<_>>()
            .join(" ")
            .bytes()
            .collect();
        // Find first non-space character position.
        let first_non_space = input_line.iter().position(|&x| x != b' ');

        // If this is not empty or only-whitespace use it.
        if let Some(pos) = first_non_space {
            return (input_line, pos);
        }
    }

    loop {
        print!("**");
        std::io::stdout().flush().unwrap();
        let mut line = Vec::new();
        if !read_line(&mut std::io::stdin().lock(), &mut line) {
            println!();
            print!("! End of file on the terminal... why?");
            std::process::exit(1);
        }

        // Find first non-space character position.
        let first_non_space = line.iter().position(|&x| x != b' ');

        // Return if there is some non-space input.
        if let Some(pos) = first_non_space {
            return (line, pos);
        }

        // Otherwise ask again for input.
        println!("Please type the name of your input file.");
    }
}

/// See 1335.
fn final_cleanup(
    dumping: bool,
    hyphenator: &Hyphenator,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    if !dumping {
        logger.newline_char = None;
    }
    if logger.job_name.is_none() {
        logger.open_log_file(&scanner.input_stack, eqtb);
    }

    scanner.input_stack.shutdown(logger);

    if eqtb.cur_level > 0 {
        logger.print_nl_str("(");
        logger.print_esc_str(b"end occurred ");
        logger.print_str("inside a group at level ");
        logger.print_int(eqtb.cur_level as i32);
        logger.print_char(b')');
    }

    scanner.clear_cond_stack(logger);

    if logger.history != History::Spotless {
        if logger.history == History::WarningIssued
            || logger.interaction != InteractionMode::ErrorStop
        {
            if logger.terminal_logging {
                // Temporarily disable logging to file
                let log_file = std::mem::take(&mut logger.log_file);
                logger.print_nl_str("(see the transcript file for additional information)");
                logger.log_file = log_file;
            }
        }
    }
    if dumping {
        if INIT {
            store_fmt_file(hyphenator, &scanner.input_stack, eqtb, logger);
        } else {
            logger.print_nl_str("(\\dump is performed only by INITEX)");
        }
    }
}

/// See 1333. and 1378.
fn close_files_and_terminate(
    exit_status: i32,
    hyphenator: &Hyphenator,
    nest: &SemanticState,
    mut output: Output,
    scanner: &Scanner,
    eqtb: &Eqtb,
    logger: &mut Logger,
) -> ! {
    for k in 0..=15 {
        output.write_files[k] = None;
    }
    logger.newline_char = None;
    if cfg!(feature = "stats") {
        if eqtb.integer(IntegerVariable::TracingStats) > 0 {
            output_statistics_about_this_job(hyphenator, nest, scanner, eqtb, logger);
        }
    }
    output.finish_dvi_file(logger);
    logger.shutdown();
    std::process::exit(exit_status);
}

/// NOTE This is not particularly meaningful information anymore.
/// See 1334.
fn output_statistics_about_this_job(
    hyphenator: &Hyphenator,
    nest: &SemanticState,
    scanner: &Scanner,
    eqtb: &Eqtb,
    logger: &mut Logger,
) {
    if let Some(log_file) = &mut logger.log_file {
        wlog_ln(log_file, b" ");
        wlog_ln(log_file, b"Here is how much of TeX's memory you used:");
        wlog(log_file, b" 0 strings out of 0");
        wlog_ln(log_file, b" 0 string_characters out of 0");
        wlog_ln(log_file, b" 0 words of memory out of 0");
        wlog_ln(
            log_file,
            format!(
                " {} multiletter control sequences out of {}",
                eqtb.control_sequences.cs_count, 0
            )
            .as_bytes(),
        );
        wlog(log_file, b" 0  words out of font info for 0 fonts");
        wlog_ln(log_file, b", out of 0 for 0");
        wlog(
            log_file,
            format!(" {} hyphenation exception", hyphenator.exceptions.len()).as_bytes(),
        );
        if hyphenator.exceptions.len() != 1 {
            wlog(log_file, b"s");
        }
        wlog_ln(log_file, format!(" out of {}", 0).as_bytes());
        wlog_ln(
            log_file,
            format!(
                " {}i,{}n,0p,0b,{}s stack positions out of {}i,{}n,0p,0b,{}s",
                scanner.input_stack.max_in_stack,
                nest.max_nest_stack,
                eqtb.max_save_stack + 6,
                INPUT_STACK_SIZE,
                0,
                0
            )
            .as_bytes(),
        );
    }
}

///////////////////////////////////////////////////////////

/// This should do the same rounding as the Pascal round function used in TeX82.
/// To the best of our current knowledge this means rounding to the nearest integer,
/// in case of ties rounding away from zero.
/// See 3.
fn round(x: f64) -> i32 {
    if x > i32::MAX as f64 {
        i32::MAX
    // We don't take the absolute minimum here to ensure we can negate the result safely.
    } else if x < (i32::MIN + 1) as f64 {
        i32::MIN + 1
    } else {
        x.round() as i32
    }
}

/// See 1091.
fn norm_min(h: i32) -> usize {
    if h <= 0 {
        1
    } else if h >= 63 {
        63
    } else {
        h as usize
    }
}

/// Attempts to open the file corresponding to `path` as read-only.
/// See 27.
fn open_in(path: &Path) -> Result<File, std::io::Error> {
    if !path.is_file() {
        return Err(std::io::Error::other("Not a file"));
    }
    OpenOptions::new().read(true).open(path)
}

/// Attempts to open the file corresponding to `path` as output file.
/// The file is always created and truncated if it exists already.
/// See 27.
fn open_out(path: &Path) -> Result<File, std::io::Error> {
    OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(path)
}

/// Reads a line from the given input source into the given string.
///
/// Reads until it encounters a `'\n'`. Removes a trailing
/// `'\n'` and trailing spaces. Returns false if the source has been exhausted.
///
/// Note: This replaces the function "input_ln" from TeX82 which worked with
/// the global buffer instead of returning a string.
///
/// See 31.
fn read_line(f: &mut impl BufRead, line: &mut Vec<u8>) -> bool {
    let bytes_read = f.read_until(b'\n', line).expect("IO error");
    if bytes_read == 0 {
        return false;
    }
    if let Some(b'\n') = line.last() {
        line.pop();
    }
    while let Some(b' ') = line.last() {
        line.pop();
    }
    true
}
