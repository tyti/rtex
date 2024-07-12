use crate::format::{Dumpable, FormatError};
use crate::print::Printer;

use std::io::Write;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Prefix {
    Long,
    Outer,
    Global,
}

impl Prefix {
    pub fn display(&self, printer: &mut impl Printer) {
        let s: &[u8] = match self {
            Self::Long => b"long",
            Self::Outer => b"outer",
            Self::Global => b"global",
        };
        printer.print_esc_str(s);
    }
}

impl Dumpable for Prefix {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::Long => writeln!(target, "Long")?,
            Self::Outer => writeln!(target, "Outer")?,
            Self::Global => writeln!(target, "Global")?,
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "Long" => Ok(Self::Long),
            "Outer" => Ok(Self::Outer),
            "Global" => Ok(Self::Global),
            _ => Err(FormatError::ParseError),
        }
    }
}
