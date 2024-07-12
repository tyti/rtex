use super::line_lexer::LineLexer;
use super::macro_reader::MacroReader;
use super::token_source::{AlignCommand, TokenListReader, TokenSourceType};
use crate::eqtb::{ControlSequence, ControlSequenceId, Eqtb, IntegerVariable};
use crate::error::{fatal_error, overflow};
use crate::logger::{InteractionMode, Logger};
use crate::macros::{show_macro_pseudo, Macro, MacroToken};
use crate::print::{Printer, ERROR_LINE, HALF_ERROR_LINE, MAX_PRINT_LINE};
use crate::read_line;
use crate::token::Token;
use crate::token_lists::{show_token_list_pseudo, RcTokenList};

use std::fs::File;
use std::io::BufReader;
use std::path::Path;
use std::rc::Rc;

// Maximum number of simultaneous input sources
pub const INPUT_STACK_SIZE: usize = 200;

pub struct InputStack {
    /// The current input source.
    cur_source: InputSource,
    /// The stack of all saved input sources.
    /// See 301.
    stack: Vec<InputSource>,
    pub max_in_stack: usize,
    /// Holds the line number of the most recent input file or zero for the terminal input.
    line_number: usize,
    /// Stores the line number of all but the most recent input file (and the terminal input).
    line_number_stack: Vec<usize>,
    /// This is set by an \endinput command and means the current line is
    /// the last line that will be fetched from the current file.
    /// See 361.
    force_eof: bool,
    /// Keep the first input line around for when the log file is ready.
    pub first_line: Vec<u8>,
}

/// An input source. It can either provide characters or tokens
/// depending on the type.
#[derive(Debug)]
pub enum InputSource {
    /// A [`TextSource`].
    TextSource {
        source_type: SourceType,
        lexer: LineLexer,
    },
    /// A called macro.
    MacroCall { reader: MacroReader },
    /// A [TokenSource].
    TokenSource {
        source_type: TokenSourceType,
        reader: TokenListReader,
    },
    /// An expansion-suppressed token.
    DontExpand {
        cs: ControlSequence,
        has_been_read: bool,
    },
}

/// Indicates the type of [`TextSource`].
/// See 303--305.
#[derive(Debug)]
pub enum SourceType {
    /// An input file.
    File { file: Box<BufReader<File>> },
    /// An inserted line from the terminal.
    TerminalInsert,
    /// The terminal base.
    TerminalBase,
    /// An input stream.
    Stream(Option<u8>),
}

impl InputStack {
    /// See 331.
    #[inline]
    pub fn new(first_line: Vec<u8>, first_non_space_pos: usize) -> Self {
        Self {
            cur_source: InputSource::TextSource {
                lexer: LineLexer::new_with_pos(first_line.clone(), first_non_space_pos),
                source_type: SourceType::TerminalBase,
            },
            stack: Vec::new(),
            max_in_stack: 0,
            line_number: 0,
            line_number_stack: Vec::new(),
            force_eof: false,
            first_line,
        }
    }

    /// Returns a reference to the currently used input source.
    pub fn current_source(&self) -> &InputSource {
        &self.cur_source
    }

    /// Returns a mutable reference to the currently used input source.
    fn current_source_mut(&mut self) -> &mut InputSource {
        &mut self.cur_source
    }

    /// Returns an Iterator over references to the sources going from
    /// the current source to the base terminal.
    fn source_iter(&self) -> impl Iterator<Item = &InputSource> {
        std::iter::once(&self.cur_source).chain(self.stack.iter().rev())
    }

    /// Push a new [`InputSource`] on the input stack.
    /// See 321.
    pub fn push_input(&mut self, new_input_source: InputSource, eqtb: &Eqtb, logger: &mut Logger) {
        let old_input_source = std::mem::replace(&mut self.cur_source, new_input_source);
        if self.stack.len() > self.max_in_stack {
            self.max_in_stack = self.stack.len();
            if self.stack.len() == INPUT_STACK_SIZE {
                overflow("input stack size", INPUT_STACK_SIZE, self, eqtb, logger);
            }
        }
        self.stack.push(old_input_source);
    }

    /// Pop the top of the input stack into the current input state.
    /// See 322.
    pub fn pop_input(&mut self) {
        let prev_source = self.stack.pop().expect("`pop_input` called on empty stack");
        self.cur_source = prev_source;
    }

    /// See 323.
    pub fn begin_token_list(
        &mut self,
        rc_token_list: RcTokenList,
        source_type: TokenSourceType,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        if eqtb.integer(IntegerVariable::TracingMacros) > 1 {
            source_type.print_named_token_list_trace(&rc_token_list, eqtb, logger);
        }

        let reader = TokenListReader {
            token_list: rc_token_list,
            pos: 0,
        };

        self.push_input(
            InputSource::TokenSource {
                reader,
                source_type,
            },
            eqtb,
            logger,
        )
    }

    /// See 323.
    pub fn begin_macro_call(
        &mut self,
        cs: ControlSequence,
        macro_def: Rc<Macro>,
        parameters: Vec<RcTokenList>,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        let reader = MacroReader {
            cs,
            macro_def,
            parameters,
            pos: 0,
        };

        self.push_input(InputSource::MacroCall { reader }, eqtb, logger)
    }

    /// Add an insert line to the top of the current input stack.
    /// See 87.
    pub fn input_from_terminal_insert(&mut self, line: Vec<u8>, eqtb: &Eqtb, logger: &mut Logger) {
        // Start the LineLexer in Midline state so that leading whitespace is not ignored.
        let lexer = LineLexer::new_midline(line);
        let input = InputSource::TextSource {
            lexer,
            source_type: SourceType::TerminalInsert,
        };
        self.push_input(input, eqtb, logger)
    }

    /// Add a line from a stream to the top of the current input stack.
    /// See 483.
    pub fn input_from_stream_line(
        &mut self,
        stream_number: Option<u8>,
        line: Vec<u8>,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        let input = InputSource::TextSource {
            lexer: LineLexer::new(line),
            source_type: SourceType::Stream(stream_number),
        };
        self.push_input(input, eqtb, logger)
    }

    /// Add a file to the top of the current input stack.
    /// See 483., 537. and 538.
    pub fn input_from_file(
        &mut self,
        path: &Path,
        file: File,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        if logger.term_offset + path.to_str().unwrap().len() > MAX_PRINT_LINE - 2 {
            logger.print_ln();
        } else if logger.term_offset > 0 || logger.file_offset > 0 {
            logger.print_char(b' ');
        }
        logger.print_char(b'(');
        logger.slow_print_str(path.to_str().unwrap().as_bytes());
        logger.update_terminal();

        self.line_number_stack.push(self.line_number);
        self.line_number = 1;
        eqtb.line_number = self.line_number;
        let mut buf_reader = BufReader::new(file);
        let mut first_line = Vec::new();
        if !read_line(&mut buf_reader, &mut first_line) {
            // An empty file is considered to contain one empty line.
            // So we do nothing.
        };
        self.firm_up_the_line(&mut first_line, eqtb, logger);
        let input = InputSource::TextSource {
            lexer: LineLexer::new(first_line),
            source_type: SourceType::File {
                file: Box::new(buf_reader),
            },
        };
        self.push_input(input, eqtb, logger)
    }

    /// See 341., 343., 360. and 362.
    pub fn get_next(
        &mut self,
        allow_new_cs: bool,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> NextResult {
        // We start over if an input source has been exhausted or if we need to expand a parameter
        loop {
            let cat_code = |c| eqtb.cat_code(c);

            // We distinguish here between text and token sources.
            let force_eof = self.force_eof;
            let token = match self.current_source_mut() {
                InputSource::TextSource { lexer, source_type } => {
                    match lexer.scan_next_token(&cat_code) {
                        Ok(Some(lexer_token)) => {
                            let Ok(token) = lexer_token.to_token(allow_new_cs, eqtb) else {
                                overflow(
                                    "hash size",
                                    ControlSequenceId::MAX as usize,
                                    self,
                                    eqtb,
                                    logger,
                                );
                            };
                            token
                        }
                        Ok(None) => match source_type {
                            SourceType::File { ref mut file, .. } => {
                                // If we are allowed to get more lines from the file.
                                if !force_eof {
                                    let mut line = Vec::new();
                                    if read_line(file, &mut line) {
                                        self.firm_up_the_line(&mut line, eqtb, logger);
                                        let InputSource::TextSource { lexer, .. } =
                                            self.current_source_mut()
                                        else {
                                            panic!("We just checked");
                                        };
                                        *lexer = LineLexer::new(line);
                                        self.line_number += 1;
                                        eqtb.line_number = self.line_number;

                                        // Continue with the new line.
                                        return NextResult::LineEnded;
                                    }
                                }
                                // Close the current file.
                                logger.print_char(b')');
                                logger.update_terminal();
                                self.force_eof = false;
                                self.pop_input();
                                self.line_number = self.line_number_stack.pop().unwrap();
                                eqtb.line_number = self.line_number;
                                return NextResult::FileEnded;
                            }
                            SourceType::TerminalInsert => {
                                self.pop_input();
                                continue;
                            }
                            SourceType::Stream(_) => return NextResult::StreamLineEnded,
                            SourceType::TerminalBase => {
                                if logger.log_file.is_none() {
                                    logger.open_log_file(self, eqtb);
                                }

                                // Ask for more input if interaction level allows.
                                if logger.interaction == InteractionMode::ErrorStop
                                    || logger.interaction == InteractionMode::Scroll
                                {
                                    let InputSource::TextSource { lexer, .. } =
                                        self.current_source_mut()
                                    else {
                                        panic!("Impossible");
                                    };
                                    // Depending on whether or not an end_line_char has been added,
                                    // an empty line can have length zero or one.
                                    let empty_line_length =
                                        if eqtb.end_line_char_inactive() { 0 } else { 1 };
                                    // If the line is empty.
                                    if lexer.line_len() == empty_line_length {
                                        logger
                                            .print_nl_str("(Please type a command or say `\\end`)");
                                    }
                                    logger.print_ln();
                                    let mut line = logger.prompt_input("*", self, eqtb);
                                    if !eqtb.end_line_char_inactive() {
                                        line.push(eqtb.end_line_char() as u8);
                                    }
                                    let InputSource::TextSource { lexer, .. } =
                                        self.current_source_mut()
                                    else {
                                        panic!("Impossible");
                                    };
                                    *lexer = LineLexer::new(line);
                                    return NextResult::LineEnded;
                                } else {
                                    fatal_error(
                                        "*** (job_aborted, no legal \\end found)",
                                        self,
                                        eqtb,
                                        logger,
                                    );
                                }
                            }
                        },
                        Err(()) => {
                            return NextResult::InvalidChar;
                        }
                    }
                }
                InputSource::MacroCall { reader } => match reader.get_next_token() {
                    None => {
                        return NextResult::TokenListEnded;
                    }
                    Some(MacroToken::Normal(token)) => token,
                    Some(MacroToken::OutParam(number)) => {
                        let parameter = reader.parameters[number as usize - 1].clone();
                        self.insert_macro_parameter(parameter, eqtb, logger);
                        continue;
                    }
                },
                InputSource::TokenSource { reader, .. } => match reader.get_next_token() {
                    None => {
                        return NextResult::TokenListEnded;
                    }
                    Some(token) => token,
                },
                InputSource::DontExpand { cs, has_been_read } => {
                    if *has_been_read {
                        // Do the same thing that would happen when a BackedUp list ends.
                        return NextResult::TokenListEnded;
                    } else {
                        // Remember to mark it as read.
                        *has_been_read = true;
                        return NextResult::DontExpand(*cs);
                    }
                }
            };

            return NextResult::Token(token);
        }
    }

    /// See 359.
    fn insert_macro_parameter(&mut self, parameter: RcTokenList, eqtb: &Eqtb, logger: &mut Logger) {
        // NOTE To avoid calling begin_token_list here as it is in the parent
        // module, we just copy the logic here.
        let reader = TokenListReader {
            token_list: parameter,
            pos: 0,
        };
        let input = InputSource::TokenSource {
            reader,
            source_type: TokenSourceType::Parameter,
        };
        self.push_input(input, eqtb, logger)
    }

    /// When \\pausing is positive, every line of an input file is echoed to
    /// the user and she has a chance to replace it, or accept it by pressing
    /// return.
    /// See 363.
    fn firm_up_the_line(&self, line: &mut Vec<u8>, eqtb: &Eqtb, logger: &mut Logger) {
        if eqtb.pausing() > 0 {
            if logger.interaction == InteractionMode::Scroll
                || logger.interaction == InteractionMode::ErrorStop
            {
                logger.print_ln();
                if !line.is_empty() {
                    for c in &*line {
                        logger.print(*c);
                    }
                }
                let user_line = logger.prompt_input("=>", self, eqtb);
                // If the user input is not empty, substitute the current line with
                // the user input.
                if !user_line.is_empty() {
                    *line = user_line;
                }
            }
        }
        if !eqtb.end_line_char_inactive() {
            line.push(eqtb.end_line_char() as u8);
        }
    }

    /// Ends the input from the current input file by not reading
    /// in any new lines.
    pub fn end_input_from_current_file(&mut self) {
        self.force_eof = true;
    }

    /// Puts the given Token list back on the input stack.
    /// See 323.
    pub fn back_list(&mut self, token_list: Vec<Token>, eqtb: &Eqtb, logger: &mut Logger) {
        let rc_token_list = std::rc::Rc::new(token_list);
        let reader = TokenListReader {
            token_list: rc_token_list,
            pos: 0,
        };
        self.push_input(
            InputSource::TokenSource {
                reader,
                source_type: TokenSourceType::BackedUp,
            },
            eqtb,
            logger,
        )
    }

    /// Backs the current token up unless the current input is a stream.
    /// See 337.
    pub fn back_up_outer_control_sequence(
        &mut self,
        current_token: Token,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        match self.current_source() {
            InputSource::TextSource {
                source_type: SourceType::Stream(_),
                ..
            } => {}
            _ => {
                self.back_list(vec![current_token], eqtb, logger);
            }
        }
    }

    /// Remove finished terminal insert sources from the input stack.
    /// See 330.
    pub fn clear_for_error_prompt(&mut self) {
        while let InputSource::TextSource {
            lexer,
            source_type: SourceType::TerminalInsert,
        } = self.current_source()
        {
            if !self.stack.is_empty() && lexer.is_finished() {
                self.pop_input();
            } else {
                break;
            }
        }
    }

    /// Remove finished token lists from stack but not VTemplates as they trigger an alignment
    /// action. We also leave UTemplates as they are (unlike TeX82).
    /// See 390.
    pub fn remove_finished_token_lists(&mut self) {
        loop {
            match self.current_source() {
                InputSource::TextSource { .. } => break,
                InputSource::MacroCall { reader } => {
                    if reader.pos < reader.macro_def.replacement_text.len() {
                        break;
                    }
                    self.pop_input();
                }
                InputSource::TokenSource {
                    reader,
                    source_type,
                } => {
                    if reader.pos < reader.token_list.len() {
                        break;
                    }
                    if let TokenSourceType::VTemplate { .. } | TokenSourceType::UTemplate =
                        source_type
                    {
                        break;
                    }
                    self.pop_input();
                }
                InputSource::DontExpand { has_been_read, .. } => {
                    if *has_been_read {
                        self.pop_input();
                    } else {
                        break;
                    }
                }
            }
        }
    }

    /// Finds the command that ended the cell that just finished.
    /// Returns a fatal error should it detect improperly interwoven alignments.
    /// See 1131.
    pub fn find_ending_command_of_cell(&self, eqtb: &Eqtb, logger: &mut Logger) -> AlignCommand {
        let error_string = "(interwoven alignment preambles are not allowed)";
        // Go through the stack to find the VTemplate that just ended.
        // If any of the encountered input sources, including the VTemplate, are not completeley read
        // (and thus are not ready to be deleted), or are TextSources, abort.
        let ending_command = {
            let mut sources = self.source_iter();
            loop {
                match sources.next() {
                    Some(InputSource::TokenSource {
                        source_type,
                        reader,
                    }) => {
                        if reader.pos < reader.token_list.len() {
                            fatal_error(error_string, self, eqtb, logger);
                        }
                        if let &TokenSourceType::VTemplate { ending_command } = source_type {
                            break ending_command;
                        }
                    }
                    Some(InputSource::MacroCall { reader }) => {
                        if reader.pos < reader.macro_def.replacement_text.len() {
                            fatal_error(error_string, self, eqtb, logger);
                        }
                    }
                    Some(InputSource::DontExpand { has_been_read, .. }) => {
                        if !*has_been_read {
                            fatal_error(error_string, self, eqtb, logger);
                        }
                    }
                    Some(InputSource::TextSource { .. }) | None => {
                        fatal_error(error_string, self, eqtb, logger);
                    }
                }
            }
        };
        ending_command
    }

    /// Test heuristically whether the output routine has finished.
    /// See 1026.
    pub fn is_output_text_finished(&self) -> bool {
        // We deviate a bit from TeX82's treatment by not allowing depleted NoExpands.
        match self.current_source() {
            // We expect to just have read the closing brace of the output routine.
            // The current source should therefore be a depleted OutputText or a depleted BackedUp.
            InputSource::TokenSource {
                source_type: TokenSourceType::OutputText | TokenSourceType::BackedUp,
                reader,
            } if reader.pos >= reader.token_list.len() => true,
            // Otherwise something is out of balance with the output routine.
            _ => false,
        }
    }

    /// If we have a token list as input source, we jump to its end.
    /// Note: This is a slightly simplified version of what TeX82 does but should do the job.
    /// See 1027.
    pub fn deplete_current_token_list(&mut self) {
        // Deplete the current token source.
        let InputSource::TokenSource { reader, .. } = self.current_source_mut() else {
            return;
        };
        reader.pos = reader.token_list.len();
    }

    /// Shows an input stack trace down to the first TextSource.
    /// See 311.
    pub fn show_context(&self, logger: &mut Logger, eqtb: &Eqtb) {
        let mut sources = self.source_iter();
        // Always display the current source.
        let cur_source = sources.next().expect("There is always at least one source");
        self.display_current_context(cur_source, logger, eqtb);

        // If the current source is already the terminal or a file, we stop here.
        if let InputSource::TextSource {
            source_type: SourceType::File { .. } | SourceType::TerminalBase,
            ..
        } = cur_source
        {
            return;
        }

        // This is the number of intermediate levels displayed so far.
        let mut nn = 0;

        for source in sources {
            // We stop once we reach a file source or the terminal.
            if let InputSource::TextSource {
                source_type: SourceType::File { .. } | SourceType::TerminalBase,
                ..
            } = source
            {
                self.display_current_context(source, logger, eqtb);
                return;
            // If we have not yet displayed too many intermediate levels.
            } else if nn < eqtb.error_context_lines() {
                // HIST: The following check was originally in 312.
                // Jump over backed-up token list that have been read already.
                if let InputSource::TokenSource {
                    source_type: TokenSourceType::BackedUp,
                    reader,
                } = source
                {
                    if reader.pos >= reader.token_list.len() {
                        continue;
                    }
                }
                self.display_current_context(source, logger, eqtb);
                nn += 1;
            // If we have just displayed too many intermediate levels.
            } else if nn == eqtb.error_context_lines() {
                logger.print_nl_str("...");
                nn += 1;
            }
        }
    }

    /// HIST: The top check was moved to 311.
    /// See 312. and 318.
    fn display_current_context(
        &self,
        input_source: &InputSource,
        logger: &mut Logger,
        eqtb: &Eqtb,
    ) {
        let location_len;
        logger.reset_tally();
        let mut pseudo_printer = logger.create_pseudo_printer();
        match input_source {
            InputSource::TextSource {
                ref source_type,
                ref lexer,
            } => {
                let location = self.source_location_to_string(source_type);
                logger.print_nl_str(&location);
                location_len = logger.get_tally();

                // From 318.
                let (first, second) = lexer.get_read_and_unread_parts_of_line(eqtb.end_line_char());

                for &c in first {
                    pseudo_printer.print(c);
                }
                pseudo_printer.switch_to_unread_part();
                for &c in second {
                    pseudo_printer.print(c);
                }
            }
            InputSource::MacroCall { reader } => {
                logger.print_ln();
                reader.cs.print_cs(eqtb, logger);
                location_len = logger.get_tally();
                show_macro_pseudo(&reader.macro_def, reader.pos, &mut pseudo_printer, eqtb);
            }
            InputSource::TokenSource {
                source_type,
                reader,
            } => {
                Self::print_type_of_token_list(source_type, reader, logger);
                location_len = logger.get_tally();
                show_token_list_pseudo(&reader.token_list, reader.pos, &mut pseudo_printer, eqtb);
            }
            &InputSource::DontExpand { cs, has_been_read } => {
                // We need to ensure here that this produces the same output as a the backup list
                // in the original implementation would have produced if we want to have full
                // backwards compatibility.
                if has_been_read {
                    logger.print_nl_str("<recently read> ");
                } else {
                    logger.print_nl_str("<to be read again> ");
                }
                location_len = logger.get_tally();

                if has_been_read {
                    pseudo_printer.print_esc_str(b"notexpanded: ");
                    cs.print_cs(eqtb, &mut pseudo_printer);
                    pseudo_printer.switch_to_unread_part();
                } else {
                    pseudo_printer.switch_to_unread_part();
                    pseudo_printer.print_esc_str(b"notexpanded: ");
                    cs.print_cs(eqtb, &mut pseudo_printer);
                }
            }
        }
        let (read_part, unread_part) = pseudo_printer.into_parts();
        let (line_one, line_two) = Self::print_two_lines_using_tricky_pseudoprinted_information(
            location_len,
            read_part,
            unread_part,
        );
        for c in line_one {
            logger.print_char(c);
        }
        logger.print_ln();
        for c in line_two {
            logger.print_char(c);
        }
    }

    /// Return an indication of what the current input is and the line number
    /// in case of file inputs.
    /// See 313.
    fn source_location_to_string(&self, source_type: &SourceType) -> String {
        match source_type {
            SourceType::TerminalBase => "<*> ".to_string(),
            SourceType::TerminalInsert => {
                // NOTE The double space is how the original implementation does it.
                "<insert>  ".to_string()
            }
            SourceType::Stream(n) => {
                if let Some(n) = n {
                    format!("<read {}> ", n)
                } else {
                    "<read *>".to_string()
                }
            }
            SourceType::File { .. } => {
                format!("l.{} ", self.line_number)
            }
        }
    }

    /// See 314.
    fn print_type_of_token_list(
        source_type: &TokenSourceType,
        reader: &TokenListReader,
        logger: &mut Logger,
    ) {
        use TokenSourceType::*;
        let s = match source_type {
            Parameter => "<argument> ",
            UTemplate | VTemplate { .. } => "<template> ",
            BackedUp => {
                if reader.pos >= reader.token_list.len() {
                    "<recently read> "
                } else {
                    "<to be read again> "
                }
            }
            Inserted => "<inserted text> ",
            OutputText => "<output> ",
            EveryParText => "<everypar> ",
            EveryMathText => "<everymath> ",
            EveryDisplayText => "<everydisplay> ",
            EveryHboxText => "<everyhbox> ",
            EveryVboxText => "<everyvbox> ",
            EveryJobText => "<everyjob> ",
            EveryCrText => "<everycr> ",
            MarkText => "<mark> ",
            WriteText => "<write> ",
        };
        logger.print_nl_str(s);
    }

    /// `l` is the number of characters already printed at the beginning of the
    /// first line that are thus not available for the pseudo-printed context.
    /// See 317.
    pub fn print_two_lines_using_tricky_pseudoprinted_information(
        location_len: usize,
        read_part: Vec<u8>,
        unread_part: Vec<u8>,
    ) -> (Vec<u8>, Vec<u8>) {
        let mut line_one = Vec::new();
        if location_len + read_part.len() <= HALF_ERROR_LINE {
            line_one.extend_from_slice(&read_part);
        } else {
            line_one.extend_from_slice(b"...");
            let target_len = HALF_ERROR_LINE.saturating_sub(location_len + 3);
            let start = read_part.len().saturating_sub(target_len);
            line_one.extend_from_slice(&read_part[start..]);
        }

        let first_line_len = location_len + line_one.len();
        // Start the second line and fill it until the breaking point with spaces.
        let mut line_two = vec![b' '; first_line_len];
        if first_line_len + unread_part.len() <= ERROR_LINE {
            line_two.extend_from_slice(&unread_part);
        } else {
            let target_len = ERROR_LINE.saturating_sub(first_line_len + 3);
            line_two.extend_from_slice(&unread_part[..target_len]);
            line_two.extend_from_slice(b"...");
        }
        (line_one, line_two)
    }

    /// Close all input sources apart from the base terminal.
    /// See 1335.
    pub fn shutdown(&mut self, logger: &mut Logger) {
        while !self.stack.is_empty() {
            // Print a closing parenthesis for every input file we close.
            if let InputSource::TextSource {
                source_type: SourceType::File { .. },
                ..
            } = self.cur_source
            {
                logger.print_str(" )");
            }
            self.pop_input();
        }
    }
}

pub enum NextResult {
    Token(Token),
    DontExpand(ControlSequence),
    LineEnded,
    FileEnded,
    StreamLineEnded,
    TokenListEnded,
    InvalidChar,
}
