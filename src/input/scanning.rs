//! A collection of scanning helper functions.

use super::expansion::get_x_token;
use super::Scanner;
use crate::command::{MathCommand, UnexpandableCommand};
use crate::eqtb::Eqtb;
use crate::logger::Logger;
use crate::token::Token;

impl Scanner {
    /// Keep getting and expanding tokens until a non-spacer token is found.
    /// See 406.
    pub fn get_next_non_blank_non_call_token(
        &mut self,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> (UnexpandableCommand, Token) {
        loop {
            let (unexpandable_command, token) = get_x_token(self, eqtb, logger);
            match unexpandable_command {
                UnexpandableCommand::Spacer => {
                    // Consume the space.
                }
                _ => return (unexpandable_command, token),
            }
        }
    }

    /// Keep getting and expanding tokens until a non-spacer non-relax token is found.
    /// See 404.
    pub fn get_next_non_blank_non_relax_non_call_token(
        &mut self,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> (UnexpandableCommand, Token) {
        loop {
            let (unexpandable_command, token) = get_x_token(self, eqtb, logger);
            match unexpandable_command {
                UnexpandableCommand::Relax { .. } | UnexpandableCommand::Spacer => {
                    // Continue scanning.
                }
                _ => return (unexpandable_command, token),
            }
        }
    }

    /// Expands tokens while consuming blanks, plus and minus signs and
    /// returns the first other command and token, as well as whether an odd number of minus
    /// signs was consumed.
    /// See 441.
    pub fn get_next_non_blank_non_sign_token(
        &mut self,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> (UnexpandableCommand, Token, bool) {
        let mut negative = false;
        loop {
            let (unexpandable_command, mut token) =
                self.get_next_non_blank_non_call_token(eqtb, logger);
            if token == Token::MINUS_TOKEN {
                negative = !negative;
                token = Token::PLUS_TOKEN;
            }
            if token != Token::PLUS_TOKEN {
                return (unexpandable_command, token, negative);
            }
        }
    }

    /// See 443.
    pub fn scan_optional_space(&mut self, eqtb: &mut Eqtb, logger: &mut Logger) {
        let (unexpandable_command, token) = get_x_token(self, eqtb, logger);
        if let UnexpandableCommand::Spacer = unexpandable_command {
            // Do nothing.
        } else {
            self.back_input(token, eqtb, logger);
        }
    }

    /// Consumes an optional `=` sign after expansion. Leading whitespace is allowed.
    /// See 405.
    pub fn scan_optional_equals(&mut self, eqtb: &mut Eqtb, logger: &mut Logger) {
        let (_, token) = self.get_next_non_blank_non_call_token(eqtb, logger);
        if token != Token::OtherChar(b'=') {
            self.back_input(token, eqtb, logger);
        }
    }

    /// Attempts to scan the given keyword from the input after expansion.
    /// Leading spaces are allowed. In case of failure the already scanned
    /// and expanded tokens are put back on the input stack.
    /// Return true if the keyword was successfully scanned, false otherwise.
    /// See 407.
    pub fn scan_keyword(&mut self, keyword: &[u8], eqtb: &mut Eqtb, logger: &mut Logger) -> bool {
        let mut scanned_tokens = Vec::new();
        for chr in keyword {
            // Allow leading spaces.
            let (mut unexpandable_command, mut token) = get_x_token(self, eqtb, logger);
            if scanned_tokens.is_empty() {
                while let UnexpandableCommand::Spacer = unexpandable_command {
                    (unexpandable_command, token) = get_x_token(self, eqtb, logger);
                }
            }
            // NOTE: This currently does not update align_state, so it could in very strange
            // circumstances create an inconsistent internal state. This is currently the same
            // as in TeX82.
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
                    if c == *chr || c == (*chr - b'a' + b'A') {
                        scanned_tokens.push(token);
                        continue;
                    }
                }
                Token::CSToken { .. } => {}
                Token::Null => {
                    panic!("Should not appear here")
                }
            }

            self.back_input(token, eqtb, logger);
            if !scanned_tokens.is_empty() {
                self.back_list(scanned_tokens, eqtb, logger);
            }
            return false;
        }
        true
    }

    /// Attempts to scan a left brace.
    /// Macro calls are expanded and \relax and spaces are allowed.
    /// In case of a missing left brace, one is inserted.
    /// See 403.
    pub fn scan_left_brace(&mut self, eqtb: &mut Eqtb, logger: &mut Logger) {
        let (unexpandable_command, token) =
            self.get_next_non_blank_non_relax_non_call_token(eqtb, logger);
        if let UnexpandableCommand::LeftBrace(_) = unexpandable_command {
            // We consume the expected left brace.
        } else {
            logger.print_err("Missing { inserted");
            let help = &[
                "A left brace was mandatory here, so I've put one in.",
                "You might want to delete and/or insert some corrections",
                "so that I will find a matching right brace soon.",
                "(If you're confused by all this, try typing `I}' now.)",
            ];
            self.back_error(token, help, eqtb, logger);
            self.align_state += 1;
        }
    }

    /// Scan a file name from the current input.
    /// See 526.
    pub fn scan_file_name(&mut self, eqtb: &mut Eqtb, logger: &mut Logger) -> Vec<u8> {
        self.name_in_progress = true;
        let mut file_name = Vec::new();
        let (mut unexpandable_command, mut token) =
            self.get_next_non_blank_non_call_token(eqtb, logger);
        loop {
            match unexpandable_command {
                UnexpandableCommand::LeftBrace(c)
                | UnexpandableCommand::RightBrace(c)
                | UnexpandableCommand::MathShift(c)
                | UnexpandableCommand::TabMark(c)
                | UnexpandableCommand::MacParam(c)
                | UnexpandableCommand::Math(MathCommand::Superscript(c))
                | UnexpandableCommand::Math(MathCommand::Subscript(c))
                | UnexpandableCommand::Letter(c)
                | UnexpandableCommand::Other(c) => {
                    if c == b' ' {
                        break;
                    }
                    file_name.push(c);
                }
                UnexpandableCommand::Spacer => break,
                _ => {
                    self.back_input(token, eqtb, logger);
                    break;
                }
            }
            (unexpandable_command, token) = get_x_token(self, eqtb, logger);
        }

        self.name_in_progress = false;
        file_name
    }
}
