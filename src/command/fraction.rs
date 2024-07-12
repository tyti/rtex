use crate::format::{Dumpable, FormatError};
use crate::print::Printer;

use std::io::Write;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FractionCommand {
    pub typ: FractionType,
    pub is_delimited: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FractionType {
    Above,
    Over,
    Atop,
}

impl FractionCommand {
    pub fn display(&self, printer: &mut impl Printer) {
        if !self.is_delimited {
            match self.typ {
                FractionType::Above => printer.print_esc_str(b"above"),
                FractionType::Over => printer.print_esc_str(b"over"),
                FractionType::Atop => printer.print_esc_str(b"atop"),
            }
        } else {
            match self.typ {
                FractionType::Above => printer.print_esc_str(b"abovewithdelims"),
                FractionType::Over => printer.print_esc_str(b"overwithdelims"),
                FractionType::Atop => printer.print_esc_str(b"atopwithdelims"),
            }
        }
    }
}

impl Dumpable for FractionType {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::Above => writeln!(target, "Above")?,
            Self::Over => writeln!(target, "Over")?,
            Self::Atop => writeln!(target, "Atop")?,
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "Above" => Ok(Self::Above),
            "Over" => Ok(Self::Over),
            "Atop" => Ok(Self::Atop),
            _ => Err(FormatError::ParseError),
        }
    }
}

impl Dumpable for FractionCommand {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.typ.dump(target)?;
        self.is_delimited.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let typ = FractionType::undump(lines)?;
        let is_delimited = bool::undump(lines)?;
        Ok(Self { typ, is_delimited })
    }
}
