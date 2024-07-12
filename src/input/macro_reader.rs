use crate::eqtb::ControlSequence;
use crate::macros::{Macro, MacroToken};
use crate::token_lists::RcTokenList;

use std::rc::Rc;

/// See 307.
#[derive(Debug)]
/// Reads from a macro call.
pub struct MacroReader {
    pub cs: ControlSequence,
    pub macro_def: Rc<Macro>,
    pub parameters: Vec<RcTokenList>,
    pub pos: usize,
}

impl MacroReader {
    /// Get the next token from the macro.
    /// See 357.
    pub fn get_next_token(&mut self) -> Option<MacroToken> {
        match self.macro_def.replacement_text.get(self.pos) {
            Some(macro_token) => {
                self.pos += 1;
                Some(*macro_token)
            }
            None => None,
        }
    }
}
