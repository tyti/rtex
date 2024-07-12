use crate::format::{Dumpable, FormatError};
use crate::print::Printer;

use std::io::Write;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LimitType {
    DisplayLimits,
    Limits,
    NoLimits,
}

impl LimitType {
    pub fn display(&self, printer: &mut impl Printer) {
        match self {
            Self::DisplayLimits => printer.print_esc_str(b"displaylimits"),
            Self::Limits => printer.print_esc_str(b"limits"),
            Self::NoLimits => printer.print_esc_str(b"nolimits"),
        }
    }
}

impl Dumpable for LimitType {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::DisplayLimits => writeln!(target, "DisplayLimits")?,
            Self::Limits => writeln!(target, "Limits")?,
            Self::NoLimits => writeln!(target, "NoLimits")?,
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "DisplayLimits" => Ok(Self::DisplayLimits),
            "Limits" => Ok(Self::Limits),
            "NoLimits" => Ok(Self::NoLimits),
            _ => Err(FormatError::ParseError),
        }
    }
}
