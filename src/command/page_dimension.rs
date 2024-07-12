use crate::dimension::scan_normal_dimen;
use crate::eqtb::Eqtb;
use crate::format::{Dumpable, FormatError};
use crate::input::Scanner;
use crate::logger::Logger;
use crate::print::Printer;

use std::io::Write;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PageDimension {
    PageGoal,
    Height,
    Stretch,
    FilStretch,
    FillStretch,
    FilllStretch,
    Shrink,
    Depth,
}

impl PageDimension {
    pub fn display(&self, printer: &mut impl Printer) {
        match self {
            Self::PageGoal => printer.print_esc_str(b"pagegoal"),
            Self::Height => printer.print_esc_str(b"pagetotal"),
            Self::Stretch => printer.print_esc_str(b"pagestretch"),
            Self::FilStretch => printer.print_esc_str(b"pagefilstretch"),
            Self::FillStretch => printer.print_esc_str(b"pagefillstretch"),
            Self::FilllStretch => printer.print_esc_str(b"pagefilllstretch"),
            Self::Shrink => printer.print_esc_str(b"pageshrink"),
            Self::Depth => printer.print_esc_str(b"pagedepth"),
        }
    }
    /// See 1245.
    pub fn alter_page_so_far(
        self,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        scanner.scan_optional_equals(eqtb, logger);
        let dimen = scan_normal_dimen(scanner, eqtb, logger);
        let target = match self {
            Self::PageGoal => &mut eqtb.page_dims.page_goal,
            Self::Height => &mut eqtb.page_dims.height,
            Self::Stretch => &mut eqtb.page_dims.stretch.normal,
            Self::FilStretch => &mut eqtb.page_dims.stretch.fil,
            Self::FillStretch => &mut eqtb.page_dims.stretch.fill,
            Self::FilllStretch => &mut eqtb.page_dims.stretch.filll,
            Self::Shrink => &mut eqtb.page_dims.shrink,
            Self::Depth => &mut eqtb.page_dims.depth,
        };
        *target = dimen;
    }
}

impl Dumpable for PageDimension {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::PageGoal => writeln!(target, "PageGoal")?,
            Self::Height => writeln!(target, "Height")?,
            Self::Stretch => writeln!(target, "Stretch")?,
            Self::FilStretch => writeln!(target, "FilStretch")?,
            Self::FillStretch => writeln!(target, "FillStretch")?,
            Self::FilllStretch => writeln!(target, "FilllStretch")?,
            Self::Shrink => writeln!(target, "Shrink")?,
            Self::Depth => writeln!(target, "Depth")?,
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "PageGoal" => Ok(Self::PageGoal),
            "Height" => Ok(Self::Height),
            "Stretch" => Ok(Self::Stretch),
            "FilStretch" => Ok(Self::FilStretch),
            "FillStretch" => Ok(Self::FillStretch),
            "FilllStretch" => Ok(Self::FilllStretch),
            "Shrink" => Ok(Self::Shrink),
            "Depth" => Ok(Self::Depth),
            _ => Err(FormatError::ParseError),
        }
    }
}
