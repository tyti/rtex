use crate::eqtb::ControlSequence;
use crate::macros::Macro;
use crate::token::Token;
use crate::token_lists::RcTokenList;

use std::rc::Rc;

/// A token list as input source.
/// See 307.
#[derive(Debug)]
pub struct MacroReader {
    pub address: ControlSequence,
    pub parameters: Vec<RcTokenList>,
    pub macro_def: Rc<Macro>,
    pub pos: usize,
}

impl MacroReader {
    /// Get the next token from the token list.
    /// See 357.
    pub fn get_next_token(&mut self) -> Option<Token> {
        match self.macro_def.text.get(self.pos) {
            Some(token) => {
                self.pos += 1;
                Some(*token)
            }
            None => None,
        }
    }
}
