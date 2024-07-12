use crate::format::{Dumpable, FormatError};
use crate::print::Printer;

use std::io::Write;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RemoveItem {
    Penalty,
    Kern,
    Glue,
}

impl RemoveItem {
    pub fn display(&self, printer: &mut impl Printer) {
        match self {
            Self::Penalty => printer.print_esc_str(b"unpenalty"),
            Self::Kern => printer.print_esc_str(b"unkern"),
            Self::Glue => printer.print_esc_str(b"unskip"),
        }
    }
}

impl Dumpable for RemoveItem {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::Penalty => writeln!(target, "Penalty")?,
            Self::Kern => writeln!(target, "Kern")?,
            Self::Glue => writeln!(target, "Glue")?,
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "Penalty" => Ok(Self::Penalty),
            "Kern" => Ok(Self::Kern),
            "Glue" => Ok(Self::Glue),
            _ => Err(FormatError::ParseError),
        }
    }
}
