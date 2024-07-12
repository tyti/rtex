mod conditional;
pub mod expansion;
pub mod input_stack;
pub mod line_lexer;
mod macro_expand;
mod macro_reader;
mod scan_tokens;
mod scanning;
pub mod token_source;

use crate::command::{
    Command, ExpandableCommand, IfTest, MacroCall, MathCommand, UnexpandableCommand,
};
use crate::eqtb::{ControlSequence, Eqtb, RegisterIndex};
use crate::error::fatal_error;
use crate::integer::{Integer, IntegerExt};
use crate::logger::Logger;
use crate::macros::{show_macro_def, Macro};
use crate::open_in;
use crate::print::{Printer, ERROR_LINE};
use crate::token::Token;
use crate::token_lists::{show_token_list, RcTokenList};

use conditional::{ConditionState, IfLimit};
pub use input_stack::InputStack;
use input_stack::{InputSource, NextResult};
use token_source::{AlignCommand, TokenListReader, TokenSourceType};

use std::fs::File;
use std::io::BufReader;
use std::path::{Path, PathBuf};

/// Default fallback path for finding input files.
/// TeX82 sets this to "TeXinput:".
const TEX_AREA: &str = "TeXinput";

pub struct Scanner {
    pub input_stack: InputStack,
    /// The balancedness of open/closing braces. See 309.
    /// It is set to the following values:
    ///  1_000_000  as the initial value,
    ///  -1_000_000 when an alignment is started,
    ///  -1_000_000 when a preamble is read,
    ///  1_000_000  at the start of each utemplate,
    ///  0          when a utemplate has been completely read,
    ///  1_000_000  when a vtemplate is pushed on the stack,
    pub align_state: i32,

    /// The status is used to help diagnosing runaway groups and arguments.
    pub scanner_status: ScannerStatus,
    /// A pointer to a control sequence used in warnings.
    pub warning_index: ControlSequence,

    /// The current macro definition under construction. Corresponds to def_ref in TeX82.
    pub macro_def: Macro,
    /// The current token list under construction. Corresponds to def_ref in TeX82.
    pub def_ref: Vec<Token>,
    /// The current argument token list under construction. This is done by temp_head in TeX82.
    pub argument: Vec<Token>,
    /// The current template under construction. This is done by hold_head in TeX82.
    pub template: Vec<Token>,

    /// Used to communicate the kind of macro call currently ongoing.
    /// See 387.
    long_state: LongState,

    // Stuff related to conditionals
    /// See 489.
    cond_stack: Vec<ConditionState>,
    if_limit: IfLimit,
    cur_if: IfTest,
    if_line: usize,
    /// See 493.
    skip_line: usize,

    pub read_file: [Option<BufReader<File>>; 16],

    /// A stack of the v-templates of the currently active alignment cells.
    pub v_templates: Vec<RcTokenList>,

    /// See 527.
    pub name_in_progress: bool,

    /// This flag is set while we are scanning the tokens for a \write command.
    pub scanning_write_tokens: bool,

    /// See 1266.
    pub after_token: Option<Token>,
}

/// The ScannerStatus is used to catch errors caused by missing opening or
/// closing braces. It is checked at the end of a file.
/// See 305.
#[derive(Clone, Copy, PartialEq)]
pub enum ScannerStatus {
    /// A file can safely be closed here.
    Normal,
    /// We are skipping through a non-selected conditional branch.
    /// Subfiles are allowed to close but not the base file.
    Skipping,
    /// We are currently defining a macro. A file should not end here.
    Defining,
    /// We are currently trying to find the arguments of a macro. A file should not end here.
    Matching,
    /// We are currently reading an alignment preamble. A file should not end here.
    Aligning,
    /// We are reading a token list for "\message", "\write" or similar.
    /// A file should not end here.
    Absorbing,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LongState {
    Call,
    LongCall,
    OuterCall,
}

impl Scanner {
    /// See 331.
    #[inline]
    pub fn new(first_line: Vec<u8>, first_non_space_pos: usize) -> Self {
        Self {
            input_stack: InputStack::new(first_line, first_non_space_pos),
            align_state: 1_000_000,
            scanner_status: ScannerStatus::Normal,
            warning_index: ControlSequence::Undefined,
            macro_def: Macro::default(),
            def_ref: Vec::new(),
            argument: Vec::new(),
            template: Vec::new(),
            long_state: LongState::Call,

            cond_stack: Vec::new(),
            if_limit: IfLimit::Normal,
            cur_if: IfTest::IfChar,
            if_line: 0,
            skip_line: 0,

            read_file: [
                None, None, None, None, None, None, None, None, None, None, None, None, None, None,
                None, None,
            ],

            v_templates: Vec::new(),

            name_in_progress: false,

            scanning_write_tokens: false,

            after_token: None,
        }
    }

    #[inline(never)]
    /// Gets the next command and token.
    /// See 341., 343., 360. and 362.
    pub fn get_next(
        &mut self,
        allow_new_cs: bool,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> (Command, Token) {
        loop {
            let (command, token) = match self.input_stack.get_next(allow_new_cs, eqtb, logger) {
                NextResult::Token(token) => (self.create_command_with_checks(token, eqtb), token),
                NextResult::DontExpand(cs) => {
                    // See 358.
                    let mut command = eqtb.control_sequences.get(cs).clone();
                    let token = Token::CSToken { cs };
                    if let Command::Expandable(_) = command {
                        command =
                            Command::Unexpandable(UnexpandableCommand::Relax { no_expand: true });
                    }
                    return (command, token);
                }
                NextResult::LineEnded => {
                    logger.check_interrupt(self, eqtb);
                    continue;
                }
                NextResult::FileEnded => {
                    if self.scanner_status != ScannerStatus::Normal {
                        self.check_outer_validity(None, eqtb, logger);
                    }
                    continue;
                }
                NextResult::StreamLineEnded => {
                    return (
                        // NOTE: This is a placeholder command. It should not be used.
                        Command::Unexpandable(UnexpandableCommand::Relax { no_expand: false }),
                        Token::Null,
                    );
                }
                NextResult::TokenListEnded => {
                    self.end_token_list(eqtb, logger);
                    continue;
                }
                NextResult::InvalidChar => {
                    Self::decry_invalid_character(self, eqtb, logger);
                    // This goes to restart because `decry_invalid_character` might change
                    // the current input source.
                    continue;
                }
            };

            if let Some(ending_chr) = self.alignment_entry_just_ended(&command) {
                self.insert_v_template(ending_chr, eqtb, logger);
            } else {
                // Check whether we need to act now.
                if self.scanner_status != ScannerStatus::Normal {
                    if let Command::Expandable(
                        ExpandableCommand::Macro(MacroCall { outer: true, .. })
                        | ExpandableCommand::EndTemplate,
                    ) = command
                    {
                        if !self.check_outer_validity(Some(token), eqtb, logger) {
                            let command = Command::Unexpandable(UnexpandableCommand::Spacer);
                            return (command, Token::Spacer(b' '));
                        }
                    }
                }
                return (command, token);
            }
        }
    }

    fn create_command_with_checks(&mut self, token: Token, eqtb: &Eqtb) -> Command {
        match token {
            Token::LeftBrace(c) => {
                self.align_state += 1;
                Command::Unexpandable(UnexpandableCommand::LeftBrace(c))
            }
            Token::RightBrace(c) => {
                self.align_state -= 1;
                Command::Unexpandable(UnexpandableCommand::RightBrace(c))
            }
            Token::MathShift(c) => Command::Unexpandable(UnexpandableCommand::MathShift(c)),
            Token::TabMark(c) => Command::Unexpandable(UnexpandableCommand::TabMark(c)),
            Token::MacParam(c) => Command::Unexpandable(UnexpandableCommand::MacParam(c)),
            Token::SuperMark(c) => {
                Command::Unexpandable(UnexpandableCommand::Math(MathCommand::Superscript(c)))
            }
            Token::SubMark(c) => {
                Command::Unexpandable(UnexpandableCommand::Math(MathCommand::Subscript(c)))
            }
            Token::Spacer(_) => Command::Unexpandable(UnexpandableCommand::Spacer),
            Token::Letter(c) => Command::Unexpandable(UnexpandableCommand::Letter(c)),
            Token::OtherChar(c) => Command::Unexpandable(UnexpandableCommand::Other(c)),

            Token::CSToken { cs } => eqtb.control_sequences.get(cs).clone(),
            Token::Null => {
                panic!("Should not appear hear")
            }
        }
    }

    /// See 346.
    fn decry_invalid_character(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) {
        logger.print_err("Text line contains an invalid character");
        let help = &[
            "A funny symbol that I can't read has just been input.",
            "Continue, and I'll forget that it ever happened.",
        ];
        logger.deletions_allowed = false;
        logger.error(help, scanner, eqtb);
        logger.deletions_allowed = true;
    }

    /// If the command ended the current alignment entry, gives back the command.
    /// See 342.
    fn alignment_entry_just_ended(&mut self, command: &Command) -> Option<AlignCommand> {
        match command {
            Command::Unexpandable(UnexpandableCommand::TabMark(_)) if self.align_state == 0 => {
                Some(AlignCommand::Tab)
            }
            Command::Unexpandable(UnexpandableCommand::Span) if self.align_state == 0 => {
                Some(AlignCommand::Span)
            }
            Command::Unexpandable(UnexpandableCommand::CarRet { .. }) if self.align_state == 0 => {
                Some(AlignCommand::CarRet)
            }
            _ => None,
        }
    }

    /// See 789.
    fn insert_v_template(
        &mut self,
        ending_command: AlignCommand,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        let v_template = match self.v_templates.pop() {
            Some(v_template) => v_template,
            None => {
                fatal_error(
                    "(interwoven alignment preambles are not allowed)",
                    &self.input_stack,
                    eqtb,
                    logger,
                );
            }
        };

        if self.scanner_status == ScannerStatus::Aligning {
            fatal_error(
                "(interwoven alignment preambles are not allowed)",
                &self.input_stack,
                eqtb,
                logger,
            );
        }
        self.input_stack.begin_token_list(
            v_template,
            TokenSourceType::VTemplate {
                // Store the command that ended the cell.
                ending_command,
            },
            eqtb,
            logger,
        );
        self.align_state = 1_000_000;
    }

    /// When an outer control sequence appears or when a file ends,
    /// we use this to check whether everything is in order.
    /// The passed argument is None if a file ends or else the current
    /// token.
    /// The return value is false if the check failed or true otherwise.
    /// In case that the check failed, the current token has been
    /// backed up and should be replaced by something innocent.
    /// See 336.
    fn check_outer_validity(
        &mut self,
        current_token: Option<Token>,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> bool {
        logger.deletions_allowed = false;
        if let Some(token) = current_token {
            self.input_stack
                .back_up_outer_control_sequence(token, eqtb, logger);
        }
        let file_ended = current_token.is_none();
        if self.scanner_status != ScannerStatus::Skipping {
            self.tell_user_what_has_run_away_and_try_to_recover(file_ended, eqtb, logger);
        } else {
            logger.print_err("Incomplete ");
            self.cur_if.display(logger);
            logger.print_str("; all text was ignored after line ");
            logger.print_int(self.skip_line as i32);
            let help = &[
                match current_token {
                    Some(_) => "A forbidden control sequence occurred in skipped text.",
                    None => "The file ended while I was skipping conditional text.",
                },
                "This kind of error happens when you say `\\if...' and forget",
                "the matching `\\fi'. I've inserted a `\\fi'; this might work.",
            ];
            let frozen_fi = Token::CSToken {
                cs: ControlSequence::FrozenFi,
            };
            self.ins_error(frozen_fi, help, eqtb, logger);
        }
        logger.deletions_allowed = true;
        false
    }

    /// See 338.
    fn tell_user_what_has_run_away_and_try_to_recover(
        &mut self,
        file_ended: bool,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        self.runaway(eqtb, logger);
        if file_ended {
            logger.print_err("File ended");
        } else {
            logger.print_err("Forbidden control sequence found");
        }
        logger.print_str(" while scanning ");
        self.print_either_definition_or_use_or_preamble_or_text(eqtb, logger);
        logger.print_str(" of ");
        self.warning_index.sprint_cs(eqtb, logger);
        let help = &[
            "I suspect you have forgotten a `}', causing me",
            "to read past where you wanted me to stop.",
            "I'll try to recover; but if the error is serious,",
            "you'd better type `E' or `X' now and fix your file.",
        ];
        logger.error(help, self, eqtb)
    }

    /// See 339.
    fn print_either_definition_or_use_or_preamble_or_text(
        &mut self,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        let token_list = match self.scanner_status {
            ScannerStatus::Defining => {
                logger.print_str("definition");
                vec![Token::RIGHT_BRACE_TOKEN]
            }
            ScannerStatus::Matching => {
                logger.print_str("use");
                self.long_state = LongState::OuterCall;
                vec![eqtb.par_token]
            }
            ScannerStatus::Aligning => {
                logger.print_str("preamble");
                self.align_state = -1_000_000;
                vec![
                    Token::CSToken {
                        cs: ControlSequence::FrozenCr,
                    },
                    Token::RIGHT_BRACE_TOKEN,
                ]
            }
            ScannerStatus::Absorbing => {
                logger.print_str("text");
                vec![Token::RIGHT_BRACE_TOKEN]
            }
            _ => panic!("Impossible"),
        };
        self.ins_list(token_list, eqtb, logger)
    }

    /// Indicates whether we have a runaway token list depending on the state of
    /// [`ScannerStatus`].
    /// See 306.
    pub fn runaway(&self, eqtb: &Eqtb, logger: &mut Logger) {
        match self.scanner_status {
            ScannerStatus::Normal | ScannerStatus::Skipping => {}
            ScannerStatus::Defining => {
                logger.print_nl_str("Runaway definition?");
                logger.print_ln();
                show_macro_def(&self.macro_def, ERROR_LINE - 10, logger, eqtb);
            }
            ScannerStatus::Matching => {
                logger.print_nl_str("Runaway argument?");
                logger.print_ln();
                show_token_list(&self.argument, ERROR_LINE - 10, logger, eqtb);
            }
            ScannerStatus::Aligning => {
                logger.print_nl_str("Runaway preamble?");
                logger.print_ln();
                show_token_list(&self.template, ERROR_LINE - 10, logger, eqtb);
            }
            ScannerStatus::Absorbing => {
                logger.print_nl_str("Runaway text?");
                logger.print_ln();
                show_token_list(&self.def_ref, ERROR_LINE - 10, logger, eqtb);
            }
        };
    }

    /// Gets the next command and token.
    /// See 365.
    pub fn get_command_and_token(
        &mut self,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> (Command, Token) {
        self.get_next(true, eqtb, logger)
    }

    /// Gets the next token.
    /// See 365.
    pub fn get_token(&mut self, eqtb: &mut Eqtb, logger: &mut Logger) -> Token {
        self.get_next(true, eqtb, logger).1
    }

    /// Puts the given Token list back on the input stack.
    /// See 323.
    pub fn back_list(&mut self, token_list: Vec<Token>, eqtb: &Eqtb, logger: &mut Logger) {
        self.input_stack.back_list(token_list, eqtb, logger)
    }

    /// See 323.
    pub fn ins_list(&mut self, token_list: Vec<Token>, eqtb: &Eqtb, logger: &mut Logger) {
        let rc_token_list = std::rc::Rc::new(token_list);
        self.input_stack
            .begin_token_list(rc_token_list, TokenSourceType::Inserted, eqtb, logger)
    }

    /// See 324.
    pub fn end_token_list(&mut self, eqtb: &mut Eqtb, logger: &mut Logger) {
        if let &InputSource::TokenSource {
            source_type: TokenSourceType::UTemplate,
            ..
        } = self.input_stack.current_source()
        {
            // At the beginning of a utemplate the align_state is set to 1_000_000.
            // If the align_state is far from this value here, we most likely are dealing with
            // interwoven alignments.
            if self.align_state > 500_000 {
                self.align_state = 0;
            } else {
                fatal_error(
                    "(interwoven alignment preambles are not allowed)",
                    &self.input_stack,
                    eqtb,
                    logger,
                );
            }
        }
        self.input_stack.pop_input();
        logger.check_interrupt(self, eqtb);
    }

    /// Puts a single token back on the stack.
    /// As opposed to backlist, this also removes finished
    /// Token lists from the stack and corrects the align state
    /// in case a brace is being put back.
    /// See 325.
    pub fn back_input(&mut self, token: Token, eqtb: &mut Eqtb, logger: &mut Logger) {
        self.input_token(TokenSourceType::BackedUp, token, eqtb, logger);

        // Potentially update the align state.
        if token.is_left_brace() {
            self.align_state -= 1;
        } else if token.is_right_brace() {
            self.align_state += 1;
        }
    }

    /// Puts a single token on the stack.
    /// Also removes finished Token lists from the stack.
    /// See 325.
    pub fn input_token(
        &mut self,
        source_type: TokenSourceType,
        token: Token,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        self.remove_depleted_token_sources(eqtb, logger);

        // Create a token list with only the current token.
        let token_list = vec![token];
        let rc_token_list = std::rc::Rc::new(token_list);

        self.input_stack.push_input(
            InputSource::TokenSource {
                source_type,
                reader: TokenListReader {
                    token_list: rc_token_list,
                    pos: 0,
                },
            },
            eqtb,
            logger,
        )
    }

    /// Insert a token as the the next token.
    /// See 326.
    pub fn insert_token_into_input(&mut self, token: Token, eqtb: &mut Eqtb, logger: &mut Logger) {
        // NOTE The align_state can with the current implementation temporarily be in a weirdly
        // changed state between the point where a brace is given to \aftergroup and this point
        // where it is put back onto the input stack.
        self.back_input(token, eqtb, logger)
    }

    /// Back up the current token and call error.
    /// See 327.
    pub fn back_error(
        &mut self,
        token: Token,
        help: &[&str],
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        logger.ok_to_interrupt = false;
        self.back_input(token, eqtb, logger);
        logger.ok_to_interrupt = true;
        logger.error(help, self, eqtb)
    }

    /// Insert the given token and call error.
    /// See 327.
    pub fn ins_error(&mut self, token: Token, help: &[&str], eqtb: &mut Eqtb, logger: &mut Logger) {
        logger.ok_to_interrupt = false;
        self.input_token(TokenSourceType::Inserted, token, eqtb, logger);
        logger.ok_to_interrupt = true;
        logger.error(help, self, eqtb)
    }

    /// Push the current token back and precede it by a relax token.
    /// See 379.
    fn insert_relax(&mut self, token: Token, eqtb: &mut Eqtb, logger: &mut Logger) {
        self.back_input(token, eqtb, logger);
        let frozen_relax = Token::CSToken {
            cs: ControlSequence::FrozenRelax,
        };
        self.input_token(TokenSourceType::Inserted, frozen_relax, eqtb, logger);
    }

    /// See 433.
    pub fn scan_register_index(&mut self, eqtb: &mut Eqtb, logger: &mut Logger) -> RegisterIndex {
        let value = Integer::scan_int(self, eqtb, logger);
        match RegisterIndex::try_from(value) {
            Ok(register) => register,
            Err(_) => {
                logger.print_err("Bad register code");
                let help = &[
                    &format!(
                        "A register number must be between 0 and {}.",
                        RegisterIndex::MAX
                    ),
                    "I changed this one to zero.",
                ];
                logger.int_error(value, help, self, eqtb);
                0
            }
        }
    }

    /// See 434.
    pub fn scan_char_num(&mut self, eqtb: &mut Eqtb, logger: &mut Logger) -> u8 {
        let value = Integer::scan_int(self, eqtb, logger);
        match u8::try_from(value) {
            Ok(char_num) => char_num,
            Err(_) => {
                logger.print_err("Bad character code");
                let help = &[
                    "A character number must be between 0 and 255.",
                    "I changed this one to zero.",
                ];
                logger.int_error(value, help, self, eqtb);
                0
            }
        }
    }

    /// Suppress the expansion of the next token.
    /// See 369.
    pub fn suppress_expansion_of_next_token(&mut self, eqtb: &mut Eqtb, logger: &mut Logger) {
        // The scanner status is temporarily set to normal to allow
        // outer macros to appear after a noexpand.
        let save_scanner_status = self.scanner_status;
        self.scanner_status = ScannerStatus::Normal;
        let token = self.get_token(eqtb, logger);
        self.scanner_status = save_scanner_status;
        // If the token was an expandable one.
        if let Token::CSToken { cs } = token {
            self.remove_depleted_token_sources(eqtb, logger);

            self.input_stack.push_input(
                InputSource::DontExpand {
                    cs,
                    has_been_read: false,
                },
                eqtb,
                logger,
            );
        } else {
            // Put the token back into the input.
            self.back_input(token, eqtb, logger);
        }
    }

    /// Ends the input from the current input file by not reading
    /// in any new lines.
    pub fn end_input_from_current_file(&mut self) {
        self.input_stack.end_input_from_current_file()
    }

    /// See 1269.
    pub fn insert_token_saved_by_afterassignment_if_any(
        &mut self,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        if let Some(token) = self.after_token.take() {
            self.back_input(token, eqtb, logger);
        }
    }

    /// Initiates reading a file when "\input" is called.
    /// See 537.
    pub fn start_input(&mut self, eqtb: &mut Eqtb, logger: &mut Logger) {
        let file_name = self.scan_file_name(eqtb, logger);
        let mut path = PathBuf::from(String::from_utf8(file_name).unwrap());
        if path.extension().is_none() {
            path.set_extension("tex");
        }
        // Open the file or keep asking for input until a filename
        // has been given that can be opened.
        let input_file = loop {
            if let Ok(file) = open_in(&path) {
                break file;
            }
            // If the path is only a filename, look for it in default directoy.
            if path.parent().is_some_and(|p| p.as_os_str().is_empty()) {
                let mut alternative_path = PathBuf::from(TEX_AREA);
                alternative_path.push(path.file_name().unwrap());
                if let Ok(file) = open_in(&alternative_path) {
                    path = alternative_path;
                    break file;
                }
            }
            path =
                logger.prompt_file_name(&path, "input file name", "tex", &self.input_stack, eqtb);
        };

        if logger.job_name.is_none() {
            logger.job_name = Some(
                path.file_stem()
                    .expect("Path should have stem here")
                    .to_os_string(),
            );
            logger.open_log_file(&self.input_stack, eqtb);
        }

        self.input_from_file(&path, input_file, eqtb, logger);
    }

    /// Add a file to the top of the current input stack.
    /// See 483., 537. and 538.
    fn input_from_file(&mut self, path: &Path, file: File, eqtb: &mut Eqtb, logger: &mut Logger) {
        self.input_stack.input_from_file(path, file, eqtb, logger)
    }

    /// Remove depleted Token sources from the stack but not VTemplates as these should only be
    /// removed when encountering \endv commands.
    /// From 325.
    fn remove_depleted_token_sources(&mut self, eqtb: &mut Eqtb, logger: &mut Logger) {
        loop {
            match self.input_stack.current_source() {
                InputSource::MacroCall { reader } => {
                    if reader.pos < reader.macro_def.replacement_text.len() {
                        break;
                    } else {
                        self.end_token_list(eqtb, logger);
                    }
                }
                InputSource::TokenSource {
                    reader,
                    source_type,
                } => {
                    if let TokenSourceType::VTemplate { .. } = source_type {
                        break;
                    } else if reader.pos < reader.token_list.len() {
                        break;
                    } else {
                        self.end_token_list(eqtb, logger);
                    }
                }
                InputSource::DontExpand { has_been_read, .. } => {
                    if *has_been_read {
                        self.end_token_list(eqtb, logger);
                    } else {
                        break;
                    }
                }
                InputSource::TextSource { .. } => break,
            }
        }
    }
}
