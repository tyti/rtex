use crate::dimension::scan_normal_dimen;
use crate::eqtb::{BoxVariable, Eqtb};
use crate::format::{Dumpable, FormatError};
use crate::input::Scanner;
use crate::logger::Logger;
use crate::print::Printer;

use std::io::Write;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BoxDimension {
    Width,
    Height,
    Depth,
}

impl BoxDimension {
    pub fn display(&self, printer: &mut impl Printer) {
        match self {
            Self::Width => printer.print_esc_str(b"wd"),
            Self::Height => printer.print_esc_str(b"ht"),
            Self::Depth => printer.print_esc_str(b"dp"),
        }
    }

    /// See 1247.
    pub fn alter_box_dimen(self, scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) {
        let b = scanner.scan_register_index(eqtb, logger);
        scanner.scan_optional_equals(eqtb, logger);
        let dimen = scan_normal_dimen(scanner, eqtb, logger);
        let mut boks = eqtb.boxes.set(BoxVariable(b), None);
        if let Some(list_node) = &mut boks {
            match self {
                Self::Width => {
                    list_node.width = dimen;
                }
                Self::Height => {
                    list_node.height = dimen;
                }
                Self::Depth => {
                    list_node.depth = dimen;
                }
            }
        }
        eqtb.boxes.set(BoxVariable(b), boks);
    }
}

impl Dumpable for BoxDimension {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::Width => writeln!(target, "Width")?,
            Self::Height => writeln!(target, "Height")?,
            Self::Depth => writeln!(target, "Depth")?,
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "Width" => Ok(Self::Width),
            "Height" => Ok(Self::Height),
            "Depth" => Ok(Self::Depth),
            _ => Err(FormatError::ParseError),
        }
    }
}
