use crate::format::{Dumpable, FormatError};
use crate::print::Printer;

use std::io::Write;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MakeBox {
    Box,
    Copy,
    LastBox,
    VSplit,
    VTop,
    VBox,
    HBox,
}

impl MakeBox {
    pub fn display(&self, printer: &mut impl Printer) {
        match self {
            Self::Box => printer.print_esc_str(b"box"),
            Self::Copy => printer.print_esc_str(b"copy"),
            Self::LastBox => printer.print_esc_str(b"lastbox"),
            Self::VSplit => printer.print_esc_str(b"vsplit"),
            Self::VTop => printer.print_esc_str(b"vtop"),
            Self::VBox => printer.print_esc_str(b"vbox"),
            Self::HBox => printer.print_esc_str(b"hbox"),
        }
    }
}

impl Dumpable for MakeBox {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::Box => writeln!(target, "Box")?,
            Self::Copy => writeln!(target, "Copy")?,
            Self::LastBox => writeln!(target, "LastBox")?,
            Self::VSplit => writeln!(target, "VSplit")?,
            Self::VTop => writeln!(target, "VTop")?,
            Self::VBox => writeln!(target, "VBox")?,
            Self::HBox => writeln!(target, "HBox")?,
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "Box" => Ok(Self::Box),
            "Copy" => Ok(Self::Copy),
            "LastBox" => Ok(Self::LastBox),
            "VSplit" => Ok(Self::VSplit),
            "VTop" => Ok(Self::VTop),
            "VBox" => Ok(Self::VBox),
            "HBox" => Ok(Self::HBox),
            _ => Err(FormatError::ParseError),
        }
    }
}
