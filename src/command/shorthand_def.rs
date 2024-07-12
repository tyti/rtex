use crate::format::{Dumpable, FormatError};
use crate::print::Printer;

use std::io::Write;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShorthandDef {
    CharDef,
    MathCharDef,
    CountDef,
    DimenDef,
    SkipDef,
    MuSkipDef,
    ToksDef,
}

impl ShorthandDef {
    pub fn display(&self, printer: &mut impl Printer) {
        let s: &[u8] = match self {
            Self::CharDef => b"chardef",
            Self::MathCharDef => b"mathchardef",
            Self::CountDef => b"countdef",
            Self::DimenDef => b"dimendef",
            Self::SkipDef => b"skipdef",
            Self::MuSkipDef => b"muskipdef",
            Self::ToksDef => b"toksdef",
        };
        printer.print_esc_str(s);
    }
}

impl Dumpable for ShorthandDef {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::CharDef => writeln!(target, "CharDef")?,
            Self::MathCharDef => writeln!(target, "MathCharDef")?,
            Self::CountDef => writeln!(target, "CountDef")?,
            Self::DimenDef => writeln!(target, "DimenDef")?,
            Self::SkipDef => writeln!(target, "SkipDef")?,
            Self::MuSkipDef => writeln!(target, "MuSkipDef")?,
            Self::ToksDef => writeln!(target, "ToksDef")?,
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "CharDef" => Ok(Self::CharDef),
            "MathCharDef" => Ok(Self::MathCharDef),
            "CountDef" => Ok(Self::CountDef),
            "DimenDef" => Ok(Self::DimenDef),
            "SkipDef" => Ok(Self::SkipDef),
            "MuSkipDef" => Ok(Self::MuSkipDef),
            "ToksDef" => Ok(Self::ToksDef),
            _ => Err(FormatError::ParseError),
        }
    }
}
