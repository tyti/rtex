use crate::format::{Dumpable, FormatError};
use crate::print::Printer;

use std::io::Write;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Hskip {
    Hskip,
    Hfil,
    Hfill,
    Hss,
    Hfilneg,
}

impl Hskip {
    pub fn display(&self, printer: &mut impl Printer) {
        match self {
            Self::Hskip => printer.print_esc_str(b"hskip"),
            Self::Hfil => printer.print_esc_str(b"hfil"),
            Self::Hfill => printer.print_esc_str(b"hfill"),
            Self::Hss => printer.print_esc_str(b"hss"),
            Self::Hfilneg => printer.print_esc_str(b"hfilneg"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Vskip {
    Vskip,
    Vfil,
    Vfill,
    Vss,
    Vfilneg,
}

impl Vskip {
    pub fn display(&self, printer: &mut impl Printer) {
        match self {
            Self::Vskip => printer.print_esc_str(b"vskip"),
            Self::Vfil => printer.print_esc_str(b"vfil"),
            Self::Vfill => printer.print_esc_str(b"vfill"),
            Self::Vss => printer.print_esc_str(b"vss"),
            Self::Vfilneg => printer.print_esc_str(b"vfilneg"),
        }
    }
}

impl Dumpable for Hskip {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::Hskip => writeln!(target, "Hskip")?,
            Self::Hfil => writeln!(target, "Hfil")?,
            Self::Hfill => writeln!(target, "Hfill")?,
            Self::Hss => writeln!(target, "Hss")?,
            Self::Hfilneg => writeln!(target, "Hfilneg")?,
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "Hskip" => Ok(Self::Hskip),
            "Hfil" => Ok(Self::Hfil),
            "Hfill" => Ok(Self::Hfill),
            "Hss" => Ok(Self::Hss),
            "Hfilneg" => Ok(Self::Hfilneg),
            _ => Err(FormatError::ParseError),
        }
    }
}

impl Dumpable for Vskip {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::Vskip => writeln!(target, "Vskip")?,
            Self::Vfil => writeln!(target, "Vfil")?,
            Self::Vfill => writeln!(target, "Vfill")?,
            Self::Vss => writeln!(target, "Vss")?,
            Self::Vfilneg => writeln!(target, "Vfilneg")?,
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "Vskip" => Ok(Self::Vskip),
            "Vfil" => Ok(Self::Vfil),
            "Vfill" => Ok(Self::Vfill),
            "Vss" => Ok(Self::Vss),
            "Vfilneg" => Ok(Self::Vfilneg),
            _ => Err(FormatError::ParseError),
        }
    }
}
