use super::expansion::{expand, get_x_token};
use super::{Scanner, ScannerStatus};
use crate::command::{Command, ExpandableCommand, UnexpandableCommand};
use crate::eqtb::{ControlSequence, Eqtb};
use crate::logger::Logger;
use crate::macros::{Macro, MacroToken, ParamToken};
use crate::token::Token;
use crate::token_lists::the_toks;

impl Scanner {
    /// See 473.
    pub fn scan_macro_definition(
        &mut self,
        cs: ControlSequence,
        xpand: bool,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> Macro {
        self.scanner_status = ScannerStatus::Defining;
        self.warning_index = cs;
        self.macro_def = Macro::default();

        // The highest parameter number so far.
        let mut param_count = 0;
        let Ok(hash_brace) =
            self.scan_and_build_parameter_part_of_macro_definition(&mut param_count, eqtb, logger)
        else {
            self.scanner_status = ScannerStatus::Normal;
            return std::mem::take(&mut self.macro_def);
        };
        self.scan_replacement_text(param_count, xpand, eqtb, logger);
        self.scanner_status = ScannerStatus::Normal;
        if let Some(brace_token) = hash_brace {
            self.macro_def
                .replacement_text
                .push(MacroToken::Normal(brace_token));
        }
        std::mem::take(&mut self.macro_def)
    }

    /// Returns any encountered hash brace or an error if we hit upon a right brace.
    /// See 474.
    fn scan_and_build_parameter_part_of_macro_definition(
        &mut self,
        param_count: &mut u8,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> Result<Option<Token>, ()> {
        let (mut command, mut token);
        loop {
            (command, token) = self.get_command_and_token(eqtb, logger);
            if token.is_left_brace() || token.is_right_brace() {
                break;
            }
            if let Command::Unexpandable(UnexpandableCommand::MacParam(c)) = command {
                if let Some(hash_brace) =
                    self.scan_parameter_or_hash_brace(c, param_count, eqtb, logger)
                {
                    return Ok(Some(hash_brace));
                } else {
                    continue;
                }
            } else {
                self.macro_def
                    .parameter_text
                    .push(ParamToken::Normal(token));
            }
        }
        self.macro_def.parameter_text.push(ParamToken::End);
        if let Command::Unexpandable(UnexpandableCommand::RightBrace(_)) = command {
            self.express_shock_at_missing_left_brace(eqtb, logger);
            return Err(());
        }
        Ok(None)
    }

    /// See 475.
    fn express_shock_at_missing_left_brace(&mut self, eqtb: &mut Eqtb, logger: &mut Logger) {
        logger.print_err("Missing { inserted");
        self.align_state += 1;
        let help = &[
            "Where was the left brace? You said something like `\\def\\a}',",
            "which I'm going to interpret as `\\def\\a{}'.",
        ];
        logger.error(help, self, eqtb)
    }

    /// Returns a hash brace if encountered or None.
    /// See 476.
    fn scan_parameter_or_hash_brace(
        &mut self,
        mac_param_char: u8,
        param_count: &mut u8,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> Option<Token> {
        let s = ParamToken::Match(mac_param_char);

        let token = self.get_token(eqtb, logger);
        if let Token::LeftBrace(_) = token {
            self.macro_def
                .parameter_text
                .push(ParamToken::Normal(token));
            self.macro_def.parameter_text.push(ParamToken::End);
            return Some(token);
        }
        if *param_count == 9 {
            logger.print_err("You already have nine parameters");
            let help = &[
                "I'm going to ignore the # sign you just used,",
                "as well as the token that followed it.",
            ];
            logger.error(help, self, eqtb);
        } else {
            *param_count += 1;
            if token != Token::OtherChar(b'0' + *param_count) {
                logger.print_err("Parameters must be numbered consecutively");
                let help = &[
                    "I've inserted the digit you should have used after the #.",
                    "Type `1' to delete what you did use.",
                ];
                self.back_error(token, help, eqtb, logger);
            }
            self.macro_def.parameter_text.push(s);
        }
        None
    }

    /// See 477.
    fn scan_replacement_text(
        &mut self,
        param_count: u8,
        xpand: bool,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        let mut unbalance = 1;
        loop {
            let (command, token) = if xpand {
                let (unexpandable_command, token) =
                    self.expand_next_part_of_replacement_text(eqtb, logger);
                (Command::Unexpandable(unexpandable_command), token)
            } else {
                self.get_command_and_token(eqtb, logger)
            };
            if token.is_left_brace() {
                unbalance += 1;
            } else if token.is_right_brace() {
                unbalance -= 1;
                if unbalance == 0 {
                    return;
                }
            } else {
                if let Command::Unexpandable(UnexpandableCommand::MacParam(_)) = command {
                    let macro_token = self.look_for_parameter_number_or_double_hash(
                        token,
                        param_count,
                        xpand,
                        eqtb,
                        logger,
                    );
                    self.macro_def.replacement_text.push(macro_token);
                    continue;
                }
            }
            self.macro_def
                .replacement_text
                .push(MacroToken::Normal(token));
        }
    }

    /// See 478.
    fn expand_next_part_of_replacement_text(
        &mut self,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> (UnexpandableCommand, Token) {
        loop {
            let (command, token) = self.get_next(false, eqtb, logger);
            match command {
                Command::Unexpandable(unexpandable_command) => {
                    return (unexpandable_command, token)
                }
                Command::Expandable(ExpandableCommand::The) => {
                    let toks = the_toks(self, eqtb, logger);
                    for token in toks {
                        self.macro_def
                            .replacement_text
                            .push(MacroToken::Normal(token));
                    }
                }
                Command::Expandable(expandable_command) => {
                    expand(expandable_command, token, self, eqtb, logger);
                }
            }
        }
    }

    /// See 479.
    fn look_for_parameter_number_or_double_hash(
        &mut self,
        token: Token,
        param_count: u8,
        xpand: bool,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> MacroToken {
        let s = token;
        let (command, token) = if xpand {
            let (unexpandable_command, token) = get_x_token(self, eqtb, logger);
            (Command::Unexpandable(unexpandable_command), token)
        } else {
            self.get_command_and_token(eqtb, logger)
        };
        if let Command::Unexpandable(UnexpandableCommand::MacParam(_)) = command {
            MacroToken::Normal(token)
        } else {
            match token {
                Token::OtherChar(dig @ b'1'..=b'9') if dig - b'0' <= param_count => {
                    MacroToken::OutParam(dig - b'0')
                }
                _ => {
                    logger.print_err("Illegal parameter number in definition of ");
                    self.warning_index.sprint_cs(eqtb, logger);
                    let help = &[
                        "You meant to type ## instead of #, right?",
                        "Or maybe a } was forgotten somewhere earlier, and things",
                        "are all screwed up? I'm going to assume that you meant ##.",
                    ];
                    self.back_error(token, help, eqtb, logger);
                    MacroToken::Normal(s)
                }
            }
        }
    }

    /// See 473.
    pub fn scan_toks(
        &mut self,
        cs: ControlSequence,
        xpand: bool,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> Vec<Token> {
        self.scanner_status = ScannerStatus::Absorbing;
        self.warning_index = cs;
        self.def_ref = Vec::new();

        self.scan_left_brace(eqtb, logger);

        self.scan_and_build_body_of_token_list(xpand, eqtb, logger);
        self.scanner_status = ScannerStatus::Normal;
        std::mem::take(&mut self.def_ref)
    }

    /// See 477.
    fn scan_and_build_body_of_token_list(
        &mut self,
        xpand: bool,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        let mut unbalance = 1;
        loop {
            let token = if xpand {
                let (_, token) = self.expand_next_part_of_input(eqtb, logger);
                token
            } else {
                self.get_token(eqtb, logger)
            };
            if token.is_left_brace() {
                unbalance += 1;
            } else if token.is_right_brace() {
                unbalance -= 1;
                if unbalance == 0 {
                    return;
                }
            }
            self.def_ref.push(token);
        }
    }

    /// See 478.
    fn expand_next_part_of_input(
        &mut self,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> (UnexpandableCommand, Token) {
        loop {
            let (command, token) = self.get_next(false, eqtb, logger);
            match command {
                Command::Unexpandable(unexpandable_command) => {
                    return (unexpandable_command, token)
                }
                Command::Expandable(ExpandableCommand::The) => {
                    let mut toks = the_toks(self, eqtb, logger);
                    self.def_ref.append(&mut toks);
                }
                Command::Expandable(expandable_command) => {
                    expand(expandable_command, token, self, eqtb, logger);
                }
            }
        }
    }
}
