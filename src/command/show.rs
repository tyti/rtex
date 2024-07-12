use crate::format::{Dumpable, FormatError};
use crate::print::Printer;

use std::io::Write;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShowCommand {
    Show,
    ShowBox,
    ShowThe,
    ShowLists,
}

impl ShowCommand {
    pub fn display(&self, printer: &mut impl Printer) {
        match self {
            Self::Show => printer.print_esc_str(b"show"),
            Self::ShowBox => printer.print_esc_str(b"showbox"),
            Self::ShowThe => printer.print_esc_str(b"showthe"),
            Self::ShowLists => printer.print_esc_str(b"showlists"),
        }
    }
}

impl Dumpable for ShowCommand {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::Show => writeln!(target, "Show")?,
            Self::ShowBox => writeln!(target, "ShowBox")?,
            Self::ShowThe => writeln!(target, "ShowThe")?,
            Self::ShowLists => writeln!(target, "ShowLists")?,
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "Show" => Ok(Self::Show),
            "ShowBox" => Ok(Self::ShowBox),
            "ShowThe" => Ok(Self::ShowThe),
            "ShowLists" => Ok(Self::ShowLists),
            _ => Err(FormatError::ParseError),
        }
    }
}
