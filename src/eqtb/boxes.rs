use super::RegisterIndex;
use crate::format::{Dumpable, FormatError};
use crate::nodes::ListNode;

use std::io::Write;

/// See 230.
pub struct BoxParameters {
    boxes: Vec<Option<ListNode>>,
}

impl BoxParameters {
    /// All token list variables are undefined initially.
    /// See 232.
    pub fn new() -> Self {
        Self {
            boxes: vec![None; RegisterIndex::MAX as usize + 1],
        }
    }

    pub fn get(&self, dim_var: BoxVariable) -> &Option<ListNode> {
        &self.boxes[dim_var.0 as usize]
    }

    pub fn set(&mut self, dim_var: BoxVariable, new_value: Option<ListNode>) -> Option<ListNode> {
        std::mem::replace(&mut self.boxes[dim_var.0 as usize], new_value)
    }
}

/// Specifies a box variable in the Eqtb.
/// See 230.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BoxVariable(pub RegisterIndex);

impl BoxVariable {
    /// The name of the box variable without preceding escape character.
    /// See 233.
    pub fn to_string(self) -> Vec<u8> {
        format!("box{}", self.0).as_bytes().to_vec()
    }
}

impl Dumpable for BoxVariable {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.0.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let register = RegisterIndex::undump(lines)?;
        Ok(Self(register))
    }
}

impl Dumpable for BoxParameters {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.boxes.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let boxes = Dumpable::undump(lines)?;
        Ok(Self { boxes })
    }
}
