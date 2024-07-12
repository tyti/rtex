use crate::format::{Dumpable, FormatError};
use crate::print::Printer;

use std::io::Write;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DefCommand {
    pub global: bool,
    pub expand: bool,
}

impl DefCommand {
    pub fn display(&self, printer: &mut impl Printer) {
        let s: &[u8] = if self.global {
            if self.expand {
                b"xdef"
            } else {
                b"gdef"
            }
        } else {
            if self.expand {
                b"edef"
            } else {
                b"def"
            }
        };
        printer.print_esc_str(s);
    }
}

impl Dumpable for DefCommand {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.global.dump(target)?;
        self.expand.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let global = bool::undump(lines)?;
        let expand = bool::undump(lines)?;
        Ok(Self { global, expand })
    }
}
