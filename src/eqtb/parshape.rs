use crate::dimension::Dimension;
use crate::format::{Dumpable, FormatError};

use std::io::Write;

/// Vector of indentation and width pairs.
pub type ParagraphShape = Vec<(Dimension, Dimension)>;

/// See 230.
pub struct ParShapeParameter {
    par_shape: ParagraphShape,
}

impl ParShapeParameter {
    /// See 232.
    pub fn new() -> Self {
        Self { par_shape: vec![] }
    }

    pub fn get(&self, _var: ParShapeVariable) -> &ParagraphShape {
        &self.par_shape
    }

    pub fn set(&mut self, _var: ParShapeVariable, new_value: ParagraphShape) -> ParagraphShape {
        std::mem::replace(&mut self.par_shape, new_value)
    }
}

/// Specifies the paragraph shape variable in the Eqtb.
/// See 230.
#[derive(Clone, Copy, PartialEq)]
pub struct ParShapeVariable;

impl ParShapeVariable {
    /// The name of the paragraph shape variable without preceding escape character.
    /// See 231. and 233.
    pub fn to_string(self) -> Vec<u8> {
        b"parshape".to_vec()
    }
}

impl Dumpable for ParShapeParameter {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.par_shape.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let par_shape = ParagraphShape::undump(lines)?;
        Ok(Self { par_shape })
    }
}
