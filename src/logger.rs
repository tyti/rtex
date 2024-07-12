use crate::eqtb::{Eqtb, FontIndex, IntegerVariable, TokenListVariable};
use crate::error::{fatal_error, jump_out};
use crate::format::{Dumpable, FormatError, FORMAT_EXTENSION};
use crate::input::{InputStack, Scanner};
use crate::print::pseudo::PseudoPrinter;
use crate::print::{cannot_be_printed, to_hex_char, Printer, MAX_PRINT_LINE};
use crate::semantic_nest::Mode;
use crate::token_lists::token_show;
use crate::{open_out, read_line};

use std::ffi::OsString;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};

pub const BANNER: &str = "This is rtex, Version 3.141592653";

/// See 54.
pub struct Logger {
    pub interaction: InteractionMode,
    pub terminal_logging: bool,
    pub term_offset: usize,
    pub log_file: Option<BufWriter<File>>,
    pub file_offset: usize,
    log_name: Option<Vec<u8>>,
    pub job_name: Option<OsString>,
    pub format_ident: String,
    tally: usize,

    // These are two copies of the the current values of the newline and escape chars
    // that are stored in the Eqtb.
    // NOTE: It is imporant that these are kept in sync.
    pub newline_char: Option<u8>,
    pub escape_char: Option<u8>,

    // Error handling
    /// This is set to false during calls to `error` in `get_next` to avoid recursion.
    /// See 76.
    pub deletions_allowed: bool,
    /// Used to disallow \setbox assignments after a closing \halign in display math
    /// or right after an \accent command.
    pub set_box_allowed: bool,
    /// Records the error/warning state of the current run.
    pub history: History,
    /// The number of errors that have happened without interaction
    /// in the current paragraph.
    pub error_count: i32,
    pub use_err_help: bool,

    /// See 96.
    pub interrupt: i32,
    pub ok_to_interrupt: bool,

    old_setting: bool,

    /// None indicates that the last shown mode was during the scanning of write tokens.
    /// See 213.
    pub shown_mode: Option<Mode>,

    /// Used to keep track of the current font across several calls to `short_display`.
    /// See 173.
    pub font_in_short_display: FontIndex,

    pub pack_begin_line: i32,

    /// See 1281.
    pub long_help_seen: bool,
}

impl Logger {
    pub const fn new(format_ident: String, interaction: InteractionMode) -> Self {
        Self {
            interaction,
            terminal_logging: true,
            term_offset: 0,
            file_offset: 0,
            log_file: None,
            log_name: None,
            job_name: None,
            format_ident,
            tally: 0,

            // See 240.
            newline_char: Some(0),
            escape_char: Some(b'\\'),

            deletions_allowed: true,
            set_box_allowed: true,
            history: History::Spotless,
            error_count: 0,
            use_err_help: false,

            interrupt: 0,
            ok_to_interrupt: true,

            // We set it to some value but it should not matter which.
            old_setting: false,
            shown_mode: None,

            font_in_short_display: 0,
            pack_begin_line: 0,

            long_help_seen: false,
        }
    }

    /// See 34.
    pub fn update_terminal(&self) {
        std::io::stdout().flush().unwrap();
    }

    /// Prints a char as it is.
    /// See 58.
    fn print_raw_char(&mut self, c: u8) {
        if self.terminal_logging {
            std::io::stdout()
                .write_all(&[c])
                .expect("Failed writing to stdout");
            self.term_offset += 1;
            if self.term_offset == MAX_PRINT_LINE {
                println!();
                self.term_offset = 0;
            }
        }
        if let Some(log_file) = &mut self.log_file {
            wlog(log_file, std::slice::from_ref(&c));
            self.file_offset += 1;
            if self.file_offset == MAX_PRINT_LINE {
                wlog_cr(log_file);
                self.file_offset = 0;
            }
        }
        self.tally += 1;
    }

    /// Prints a newline if we are not at the beginning of a new line already.
    fn ensure_newline(&mut self) {
        if ((self.term_offset > 0) && (self.terminal_logging))
            || ((self.file_offset > 0) && (self.log_file.is_some()))
        {
            self.print_ln();
        }
    }

    pub fn print_nl(&mut self, c: u8) -> &mut Self {
        self.ensure_newline();
        self.print(c);
        self
    }

    pub fn print_nl_str(&mut self, s: &str) -> &mut Self {
        self.ensure_newline();
        self.print_str(s);
        self
    }

    /// See 73.
    pub fn print_err(&mut self, s: &str) -> &mut Self {
        self.print_nl_str("! ");
        self.print_str(s);
        self
    }

    /// Create a Pseudo printer for the current Logger.
    pub fn create_pseudo_printer(&self) -> PseudoPrinter {
        PseudoPrinter::new(self.escape_char)
    }

    /// Prints the passed string to the Logger and gets a new line from
    /// the terminal.
    /// See 71.
    pub fn prompt_input(&mut self, s: &str, input_stack: &InputStack, eqtb: &Eqtb) -> Vec<u8> {
        self.print_str(s);
        self.term_input(input_stack, eqtb)
    }

    /// Reads a line from stdin and returns it unmodified.
    /// See 71.
    pub fn term_input(&mut self, input_stack: &InputStack, eqtb: &Eqtb) -> Vec<u8> {
        self.update_terminal();
        let mut line = Vec::new();
        if !read_line(&mut std::io::stdin().lock(), &mut line) {
            fatal_error("End of file on the terminal!", input_stack, eqtb, self);
        }
        self.term_offset = 0;

        // Echo the input to the log file if possible.
        self.terminal_logging = false;
        if !line.is_empty() {
            for &c in &line {
                self.print(c);
            }
        }
        self.print_ln();
        self.terminal_logging = true;
        line
    }
}

/// The possible interaction settings. These determine how
/// TeX will react when encountering errors.
/// See 73.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InteractionMode {
    /// Omits all stops and omits terminal output
    Batch = 0,
    /// Omits all stops
    Nonstop = 1,
    /// Omits error stops
    Scroll = 2,
    /// Stops at every opportunity to interact
    ErrorStop = 3,
}

impl InteractionMode {
    pub fn as_str(&self) -> &[u8] {
        match self {
            Self::Batch => b"batchmode",
            Self::Nonstop => b"nonstopmode",
            Self::Scroll => b"scrollmode",
            Self::ErrorStop => b"errorstopmode",
        }
    }
}

impl Printer for Logger {
    /// See 57.
    fn print_ln(&mut self) {
        if self.terminal_logging {
            println!();
            self.term_offset = 0;
        }
        if let Some(log_file) = &mut self.log_file {
            wlog_cr(log_file);
            self.file_offset = 0;
        }
    }

    /// Prints the character as-is unless it is the designated internal newline character in which
    /// case it print a new line.
    /// See 58.
    fn print_char(&mut self, c: u8) {
        match self.newline_char {
            Some(nl) if c == nl => {
                self.print_ln();
            }
            _ => {
                self.print_raw_char(c);
            }
        }
    }

    /// Prints a single character, escaped if deemed necessary.
    ///
    /// For printable ASCII values this is just the corresponding character. For non-printable
    /// characters ASCII characters (value less than 128), the character is escaped by first
    /// printing two `^` characters and then the character plus 64 if the character value is below
    /// 64, or minus 64 if the character value is above 64.
    ///
    /// Thus Carriage Return, with a value of 13, is represented as `^^M` while Delete, with a
    /// value of 127, is represented as `^^?`.
    ///
    /// Non-ASCII bytes, i.e. with a value larger or equal to 128, are represented by two `^`
    /// characters followed by the hex respresentation of the value in number and lowercase
    /// letters.
    ///
    /// Thus the byte 255 is represented as `^^ff` and the byte 128 is represented as `^^80`.
    /// See 59. and 48.
    fn print(&mut self, c: u8) {
        match self.newline_char {
            Some(nl) if c == nl => {
                self.print_ln();
            }
            _ => {
                if cannot_be_printed(c) {
                    self.print_raw_char(b'^');
                    self.print_raw_char(b'^');
                    if c < 64 {
                        self.print_raw_char(c + 64);
                    } else if c < 128 {
                        self.print_raw_char(c - 64);
                    } else {
                        self.print_raw_char(to_hex_char(c / 16));
                        self.print_raw_char(to_hex_char(c % 16));
                    }
                } else {
                    self.print_raw_char(c);
                }
            }
        }
    }

    /// Prints a string. The string is expected to be printable.
    /// See 59.
    fn print_str(&mut self, s: &str) {
        let s = s.as_bytes();
        for c in s {
            self.print_char(*c);
        }
    }

    /// See 243.
    fn current_escape_character(&self) -> Option<u8> {
        self.escape_char
    }

    /// Get the number of printed characters since the start or the last reset.
    fn get_tally(&self) -> usize {
        self.tally
    }

    /// Set the count of characters that have been printed so far back to zero.
    fn reset_tally(&mut self) {
        self.tally = 0;
    }
}

/// Represents the worst kind of error encountered so far.
/// See 76.
#[derive(PartialEq)]
pub enum History {
    /// Nothing bad has happened.
    Spotless,
    /// [`begin_diagnostic`] has been called
    WarningIssued,
    /// [`error`] has been called
    ErrorMessageIssued,
}
impl Logger {
    /// See 75.
    pub fn initialize_print_selector_based_on_interaction(&mut self) {
        self.terminal_logging = self.interaction != InteractionMode::Batch;
    }

    /// See 82.
    pub fn error(&mut self, help: &[&str], scanner: &mut Scanner, eqtb: &mut Eqtb) {
        self.history = History::ErrorMessageIssued;
        self.print_char(b'.');
        scanner.input_stack.show_context(self, eqtb);
        if self.interaction == InteractionMode::ErrorStop {
            self.get_users_advice(help, scanner, eqtb);
        } else {
            // Otherwise increase the error count.
            self.error_count += 1;
            if self.error_count == 100 {
                self.print_nl_str("(That makes 100 errors; please try again.)");
                jump_out(self);
            }
            self.put_help_message_on_transcript_file(help, eqtb);
        }
    }

    /// Ask the user for a single letter input to determine
    /// next step.
    /// See 83.
    fn get_users_advice(&mut self, mut help: &[&str], scanner: &mut Scanner, eqtb: &mut Eqtb) {
        loop {
            // We need to jump out if the interaction level was
            // changed in the loop.
            if self.interaction != InteractionMode::ErrorStop {
                return;
            }
            scanner.input_stack.clear_for_error_prompt();
            self.print_ln();
            let line = self.prompt_input("? ", &scanner.input_stack, eqtb);
            let mut c = match line.first() {
                None => return,
                Some(&c) => c,
            };
            c.make_ascii_uppercase();
            if self.interpret_code(c, &line[1..], &mut help, scanner, eqtb) {
                return;
            }
        }
    }

    /// We return false for continue and true for return
    /// See 84.
    fn interpret_code(
        &mut self,
        c: u8,
        line: &[u8],
        help: &mut &[&str],
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
    ) -> bool {
        match c {
            b'0'..=b'9' => {
                if self.deletions_allowed {
                    self.delete_tokens(c, line, help, scanner, eqtb);
                    return false;
                }
            }
            b'H' => {
                self.print_help_info(help, eqtb);
                return false;
            }
            b'I' => {
                self.introduce_new_material(line, &mut scanner.input_stack, eqtb);
                return true;
            }
            b'Q' | b'R' | b'S' => {
                self.change_interaction_level(c);
                return true;
            }
            b'X' => {
                jump_out(self);
            }
            _ => {}
        }
        self.print_menu_of_available_options();
        false
    }

    /// See 85.
    fn print_menu_of_available_options(&mut self) {
        self.print_str("Type <return> to proceed, S to scroll future error messages,");
        self.print_nl_str("R to run without stopping, Q to run quietly,");
        self.print_nl_str("I to insert something, ");
        if self.deletions_allowed {
            self.print_nl_str("1 or ... or 9 to ignore the next 1 to 9 tokens of input,");
        }
        self.print_nl_str("H for help, X to quit.");
    }

    /// See 86.
    fn change_interaction_level(&mut self, c: u8) {
        self.error_count = 0;
        self.interaction = match c {
            b'Q' => InteractionMode::Batch,
            b'R' => InteractionMode::Nonstop,
            b'S' => InteractionMode::Scroll,
            _ => panic!("There are no other options"),
        };
        self.print_str("OK, entering ");
        match c {
            b'Q' => {
                self.print_esc_str(b"batchmode");
                self.terminal_logging = false;
            }
            b'R' => {
                self.print_esc_str(b"nonstopmode");
            }
            b'S' => {
                self.print_esc_str(b"scrollmode");
            }
            _ => panic!("There are no other options"),
        };
        self.print_str("...");
        self.print_ln();
        self.update_terminal();
    }

    /// See 87.
    fn introduce_new_material(&mut self, line: &[u8], input_stack: &mut InputStack, eqtb: &Eqtb) {
        let line = if !line.is_empty() {
            line.to_vec()
        } else {
            self.prompt_input("insert>", input_stack, eqtb)
        };
        // Note: there is no end_line_char added to the line.

        input_stack.input_from_terminal_insert(line, eqtb, self);
    }

    /// Delete the number of tokens as indicated by the
    /// by `c` and potentially the first character in `line`.
    /// See 88.
    fn delete_tokens(
        &mut self,
        c: u8,
        remaining_line: &[u8],
        help: &mut &[&str],
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
    ) {
        let save_align_state = scanner.align_state;
        scanner.align_state = 1_000_000;
        self.ok_to_interrupt = false;
        let tokens_to_delete = match remaining_line.first() {
            // If there are two ascii digits.
            Some(&c2) if c2.is_ascii_digit() => (c - b'0') * 10 + (c2 - b'0'),
            // If there is only one ascii digit.
            _ => c - b'0',
        };
        for _ in 0..tokens_to_delete {
            let _ = scanner.get_token(eqtb, self);
        }
        scanner.align_state = save_align_state;
        self.ok_to_interrupt = true;
        *help = &[
            "I have just deleted some text, as you asked.",
            "You can now delete more, or insert, or whatever.",
        ];
        scanner.input_stack.show_context(self, eqtb);
    }

    /// See 89.
    fn print_help_info(&mut self, help: &mut &[&str], eqtb: &Eqtb) {
        if self.use_err_help {
            self.give_err_help(eqtb);
            self.use_err_help = false;
        } else {
            for line in *help {
                self.print_str(line);
                self.print_ln();
            }
        }
        *help = &[
            "Sorry, I already gave what help I could...",
            "Maybe you should try asking a human?",
            "An error might have occurred before I noticed any problems.",
            "``If all else fails, read the instructions.''",
        ];
    }

    /// See 1284.
    fn give_err_help(&mut self, eqtb: &Eqtb) {
        let err_help = match eqtb.token_lists.get(TokenListVariable::ErrHelp) {
            Some(rc_token_list) => rc_token_list,
            None => panic!("Impossible"),
        };
        token_show(err_help, self, eqtb);
    }

    /// See 90.
    pub fn put_help_message_on_transcript_file(&mut self, help: &[&str], eqtb: &Eqtb) {
        if self.interaction != InteractionMode::Batch {
            self.terminal_logging = false;
        }
        if self.use_err_help {
            self.print_ln();
            self.give_err_help(eqtb);
        } else {
            for line in help {
                self.print_nl_str(line);
            }
        }
        self.print_ln();
        if self.interaction != InteractionMode::Batch {
            self.terminal_logging = true;
        }
        self.print_ln();
    }

    /// See 91.
    pub fn int_error(&mut self, n: i32, help: &[&str], scanner: &mut Scanner, eqtb: &mut Eqtb) {
        self.print_str(" (");
        self.print_int(n);
        self.print_char(b')');
        self.error(help, scanner, eqtb)
    }

    /// See 96.
    pub fn check_interrupt(&mut self, scanner: &mut Scanner, eqtb: &mut Eqtb) {
        if self.interrupt != 0 {
            self.pause_for_instructions(scanner, eqtb);
        }
    }

    /// See 98.
    fn pause_for_instructions(&mut self, scanner: &mut Scanner, eqtb: &mut Eqtb) {
        if self.ok_to_interrupt {
            self.interaction = InteractionMode::ErrorStop;
            self.terminal_logging = true;
            self.print_err("Interruption");
            let help = &[
                "You rang?",
                "Try to insert some instructions for me (e.g., `I\\showlists'),",
                "unless you just want to quit by typing `X'.",
            ];
            self.deletions_allowed = false;
            self.error(help, scanner, eqtb);
            self.deletions_allowed = true;
            self.interrupt = 0;
        }
    }

    /// Between this and `end_diagnostic` only logging should happen.
    /// See 245.
    pub fn begin_diagnostic(&mut self, tracing_online: i32) {
        self.old_setting = self.terminal_logging;
        if (tracing_online <= 0) && self.terminal_logging && self.log_file.is_some() {
            self.terminal_logging = false;
            if let History::Spotless = self.history {
                self.history = History::WarningIssued;
            }
        }
    }

    /// See 245.
    pub fn end_diagnostic(&mut self, blank_line: bool) {
        self.print_nl_str("");
        if blank_line {
            self.print_ln();
        }
        self.terminal_logging = self.old_setting;
    }

    /// See 518.
    pub fn print_file_name(&mut self, path: &Path) {
        self.slow_print_str(path.to_str().unwrap().as_bytes());
    }

    /// Get path from the user.
    /// Stops the program if the user aborts input.
    /// See 530.
    pub fn prompt_file_name(
        &mut self,
        path: &Path,
        s: &str,
        ext: &str,
        input_stack: &InputStack,
        eqtb: &Eqtb,
    ) -> PathBuf {
        if s == "input file name" {
            self.print_err("I can't find file `");
        } else {
            self.print_err("I can't write on file `");
        }
        self.print_file_name(path);
        self.print_str("'.");
        if ext == "tex" {
            input_stack.show_context(self, eqtb);
        }
        self.print_nl_str("Please type another ");
        self.print_str(s);
        if self.interaction == InteractionMode::Batch
            || self.interaction == InteractionMode::Nonstop
        {
            fatal_error(
                "*** (job aborted, file error in nonstop mode)",
                input_stack,
                eqtb,
                self,
            );
        }
        let line = self.prompt_input(": ", input_stack, eqtb);
        let mut new_path = self.scan_file_name_in_line(&line);
        if new_path.file_name().is_none() {
            new_path = path.to_path_buf();
        } else if new_path.extension().is_none() {
            new_path.set_extension(ext);
        }
        new_path
    }

    /// We decoupled this from the more generic `prompt_file_name`.
    /// See 530.
    pub fn prompt_format_file_name(
        &mut self,
        path: &Path,
        input_stack: &InputStack,
        eqtb: &Eqtb,
    ) -> PathBuf {
        self.print_err("I can't write on file `");
        self.print_file_name(path);
        self.print_str("'.");
        self.print_nl_str("Please type another format file name");
        if self.interaction == InteractionMode::Batch
            || self.interaction == InteractionMode::Nonstop
        {
            fatal_error(
                "*** (job aborted, file error in nonstop mode)",
                input_stack,
                eqtb,
                self,
            );
        }
        let line = self.prompt_input(": ", input_stack, eqtb);
        let mut new_path = self.scan_file_name_in_line(&line);
        if new_path.file_name().is_none() {
            new_path = path.to_path_buf();
        } else if new_path.extension().is_none() {
            new_path.set_extension(FORMAT_EXTENSION);
        }
        new_path
    }

    /// Creates a path from the given line. Ignores leading spaces
    /// and stops at first space or end of line.
    /// See 531.
    fn scan_file_name_in_line(&mut self, line: &[u8]) -> PathBuf {
        let mut k = 0;
        while k < line.len() && line[k] == b' ' {
            k += 1;
        }
        let first = k;
        loop {
            if k == line.len() {
                break;
            }
            if line[k] == b' ' {
                break;
            }
            k += 1;
        }
        PathBuf::from(std::str::from_utf8(&line[first..k]).unwrap())
    }

    /// See 534.
    pub fn open_log_file(&mut self, input_stack: &InputStack, eqtb: &Eqtb) {
        let save_terminal_logging = self.terminal_logging;
        let job_name = match &self.job_name {
            Some(name) => name.clone(),
            None => {
                let name = OsString::from("texput");
                self.job_name = Some(name.clone());
                name
            }
        };
        let mut path = PathBuf::from(&job_name);
        path.set_extension("log");
        let mut log_file = loop {
            match open_out(&path) {
                Ok(file) => {
                    break BufWriter::new(file);
                }
                Err(_) => {
                    // Attempt to get a different log file name.
                    // See 535.
                    self.terminal_logging = true;
                    path = self.prompt_file_name(
                        &path,
                        "transcript file name",
                        "log",
                        input_stack,
                        eqtb,
                    )
                }
            }
        };
        self.log_name = Some(path.as_os_str().as_encoded_bytes().to_vec());
        self.terminal_logging = false;
        self.print_banner_line_including_date_and_time(&mut log_file, eqtb);
        self.log_file = Some(log_file);
        self.print_ln();
        self.print_str("**");

        let first_line = &input_stack.first_line;
        // Decide whether to include the last character of the first input line.
        let mut end = first_line.len();
        if let Some(&c) = first_line.last() {
            if c as i32 == eqtb.end_line_char() {
                end = first_line.len() - 1;
            }
        }
        // Print the first input line into the log file.
        for &c in first_line[..end].iter() {
            self.print(c);
        }

        self.print_ln();
        self.terminal_logging = save_terminal_logging;
    }

    /// See 536.
    fn print_banner_line_including_date_and_time(&self, log_file: &mut impl Write, eqtb: &Eqtb) {
        // NOTE These should really use system time.
        let months = [
            "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC",
        ];
        let mon = eqtb.integer(IntegerVariable::Month) as usize;
        write!(
            log_file,
            "{BANNER}{format}  {day} {month} {year} {hour:02}:{minute:02}",
            format = self.format_ident,
            day = eqtb.integer(IntegerVariable::Day),
            month = months[mon - 1],
            year = eqtb.integer(IntegerVariable::Year),
            hour = eqtb.integer(IntegerVariable::Time) / 60 % 60,
            minute = eqtb.integer(IntegerVariable::Time) % 60,
        )
        .expect("Error writing to log file");
    }

    /// Shut down the Logger.
    /// See 1333.
    pub fn shutdown(&mut self) {
        if let Some(log_name) = self.log_name.clone() {
            let save_terminal_logging = self.terminal_logging;
            self.terminal_logging = false;
            self.print_ln();
            self.terminal_logging = save_terminal_logging;
            self.log_file = None;
            if self.terminal_logging {
                self.print_nl_str("Transcript written on ");
                self.slow_print_str(&log_name);
                self.print_char(b'.');
            }
        }
    }

    /// See 1265 and 75.
    pub fn new_interaction(&mut self, interaction: InteractionMode) {
        self.print_ln();
        self.interaction = interaction;
        self.terminal_logging = self.interaction != InteractionMode::Batch;
    }
}

/// See 56.
pub fn wlog(log_file: &mut impl Write, s: &[u8]) {
    log_file.write_all(s).expect("Failed writing to log file");
}

/// See 56.
pub fn wlog_ln(log_file: &mut impl Write, s: &[u8]) {
    log_file.write_all(s).unwrap();
    writeln!(log_file).expect("Failed writing to log file");
}

/// See 56.
fn wlog_cr(log_file: &mut impl Write) {
    writeln!(log_file).expect("Failed writing to log file");
}

impl Dumpable for InteractionMode {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::Batch => writeln!(target, "BatchMode")?,
            Self::Nonstop => writeln!(target, "NonstopMode")?,
            Self::Scroll => writeln!(target, "ScrollMode")?,
            Self::ErrorStop => writeln!(target, "ErrorStopMode")?,
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "BatchMode" => Ok(Self::Batch),
            "NonstopMode" => Ok(Self::Nonstop),
            "ScrollMode" => Ok(Self::Scroll),
            "ErrorStopMode" => Ok(Self::ErrorStop),
            _ => Err(FormatError::ParseError),
        }
    }
}
