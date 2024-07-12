use super::fraction::FractionCommand;
use super::limits::LimitType;
use super::{chr_cmd, InternalCommand};
use crate::format::{Dumpable, FormatError};
use crate::math::MathStyle;
use crate::nodes::noads::NoadType;
use crate::print::Printer;

use std::io::Write;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MathCommand {
    Superscript(u8),
    Subscript(u8),
    Left,
    Right,
    MSkip,
    MKern,
    MathAccent,
    NormalNoad(NoadType),
    Underline,
    Overline,
    Limits(LimitType),
    Fraction(FractionCommand),
    Style(MathStyle),
    Choice,
    NonScript,
    Vcenter,
    DelimNum,
    MathCharNum,
    /// The internal math character is never more than 15 bits.
    MathCharGiven(u16),
    Radical,
}

impl MathCommand {
    pub fn display(&self, printer: &mut impl Printer) {
        match self {
            Self::Superscript(c) => chr_cmd("superscript character ", *c, printer),
            Self::Subscript(c) => chr_cmd("subscript character ", *c, printer),
            Self::Left => printer.print_esc_str(b"left"),
            Self::Right => printer.print_esc_str(b"right"),
            Self::MSkip => printer.print_esc_str(b"mskip"),
            Self::MKern => printer.print_esc_str(b"mkern"),
            Self::MathAccent => printer.print_esc_str(b"mathaccent"),
            Self::NormalNoad(noad_type) => match noad_type {
                NoadType::Ord => printer.print_esc_str(b"mathord"),
                NoadType::Op { .. } => printer.print_esc_str(b"mathop"),
                NoadType::Bin => printer.print_esc_str(b"mathbin"),
                NoadType::Rel => printer.print_esc_str(b"mathrel"),
                NoadType::Open => printer.print_esc_str(b"mathopen"),
                NoadType::Close => printer.print_esc_str(b"mathclose"),
                NoadType::Punct => printer.print_esc_str(b"mathpunct"),
                NoadType::Inner => printer.print_esc_str(b"mathinner"),
            },
            Self::Underline => printer.print_esc_str(b"underline"),
            Self::Overline => printer.print_esc_str(b"overline"),
            Self::Limits(limit_type) => limit_type.display(printer),
            Self::Fraction(fraction_command) => fraction_command.display(printer),
            Self::Style(math_style) => math_style.display(printer),
            Self::Choice => printer.print_esc_str(b"mathchoice"),
            Self::NonScript => printer.print_esc_str(b"nonscript"),
            Self::Vcenter => printer.print_esc_str(b"vcenter"),
            Self::DelimNum => printer.print_esc_str(b"delimiter"),
            Self::MathCharNum => printer.print_esc_str(b"mathchar"),
            Self::MathCharGiven(c) => {
                printer.print_esc_str(b"mathchar");
                printer.print_hex(*c as i32);
            }
            Self::Radical => printer.print_esc_str(b"radical"),
        }
    }

    pub fn try_to_internal(&self) -> Option<InternalCommand> {
        if let &Self::MathCharGiven(c) = self {
            Some(InternalCommand::MathCharGiven(c))
        } else {
            None
        }
    }
}

impl Dumpable for MathCommand {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::Superscript(c) => {
                writeln!(target, "Superscript")?;
                c.dump(target)?;
            }
            Self::Subscript(c) => {
                writeln!(target, "Subscript")?;
                c.dump(target)?;
            }
            Self::Left => writeln!(target, "Left")?,
            Self::Right => writeln!(target, "Right")?,
            Self::MSkip => writeln!(target, "MSkip")?,
            Self::MKern => writeln!(target, "MKern")?,
            Self::MathAccent => writeln!(target, "MathAccent")?,
            Self::NormalNoad(noad_type) => {
                writeln!(target, "NormalNoad")?;
                noad_type.dump(target)?;
            }
            Self::Underline => writeln!(target, "Underline")?,
            Self::Overline => writeln!(target, "Overline")?,
            Self::Limits(limit_type) => {
                writeln!(target, "Limits")?;
                limit_type.dump(target)?;
            }
            Self::Fraction(fraction_command) => {
                writeln!(target, "Fraction")?;
                fraction_command.dump(target)?;
            }
            Self::Style(math_style) => {
                writeln!(target, "Style")?;
                math_style.dump(target)?;
            }
            Self::Choice => writeln!(target, "Choice")?,
            Self::NonScript => writeln!(target, "NonScript")?,
            Self::Vcenter => writeln!(target, "Vcenter")?,
            Self::DelimNum => writeln!(target, "DelimNum")?,
            Self::MathCharNum => writeln!(target, "MathCharNum")?,
            Self::MathCharGiven(c) => {
                writeln!(target, "MathCharGiven")?;
                c.dump(target)?;
            }
            Self::Radical => writeln!(target, "Radical")?,
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "Superscript" => {
                let c = u8::undump(lines)?;
                Ok(Self::Superscript(c))
            }
            "Subscript" => {
                let c = u8::undump(lines)?;
                Ok(Self::Subscript(c))
            }
            "Left" => Ok(Self::Left),
            "Right" => Ok(Self::Right),
            "MSkip" => Ok(Self::MSkip),
            "MKern" => Ok(Self::MKern),
            "MathAccent" => Ok(Self::MathAccent),
            "NormalNoad" => {
                let noad_type = NoadType::undump(lines)?;
                Ok(Self::NormalNoad(noad_type))
            }
            "Underline" => Ok(Self::Underline),
            "Overline" => Ok(Self::Overline),
            "Limits" => {
                let limit_type = LimitType::undump(lines)?;
                Ok(Self::Limits(limit_type))
            }
            "Fraction" => {
                let fraction_command = FractionCommand::undump(lines)?;
                Ok(Self::Fraction(fraction_command))
            }
            "Style" => {
                let math_style = MathStyle::undump(lines)?;
                Ok(Self::Style(math_style))
            }
            "Choice" => Ok(Self::Choice),
            "NonScript" => Ok(Self::NonScript),
            "Vcenter" => Ok(Self::Vcenter),
            "DelimNum" => Ok(Self::DelimNum),
            "MathCharNum" => Ok(Self::MathCharNum),
            "MathCharGiven" => {
                let c = u16::undump(lines)?;
                Ok(Self::MathCharGiven(c))
            }
            "Radical" => Ok(Self::Radical),
            _ => Err(FormatError::ParseError),
        }
    }
}
