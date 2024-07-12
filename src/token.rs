use crate::eqtb::{ControlSequence, Eqtb};
use crate::format::{Dumpable, FormatError};
use crate::print::Printer;

use std::io::Write;

/// A token.
///
/// A token is either a character token or a control sequence.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token {
    LeftBrace(u8),
    RightBrace(u8),
    MathShift(u8),
    TabMark(u8),
    MacParam(u8),
    SuperMark(u8),
    SubMark(u8),
    /// A space token has character code 32 unless changed by \uppercase or \lowercase.
    Spacer(u8),
    Letter(u8),
    OtherChar(u8),

    /// Used to indicate end of stream.
    Null,

    /// A control sequence token.
    CSToken {
        cs: ControlSequence,
    },
}

impl Token {
    /// A space token with character code 32.
    /// See 289.
    pub const SPACE_TOKEN: Token = Token::Spacer(b' ');

    /// Starts an octal constant.
    /// See 438.
    pub const OCTAL_TOKEN: Token = Token::OtherChar(b'\'');
    /// Starts a hex constant.
    /// See 438.
    pub const HEX_TOKEN: Token = Token::OtherChar(b'"');
    /// Starts an alphabetic constant.
    /// See 438.
    pub const ALPHA_TOKEN: Token = Token::OtherChar(b'`');
    /// A decimal point.
    /// See 438.
    pub const POINT_TOKEN: Token = Token::OtherChar(b'.');
    /// A decimal comma as used in parts of Europe.
    /// See 438.
    pub const CONTINENTAL_POINT_TOKEN: Token = Token::OtherChar(b',');
    /// A plus sign.
    pub const PLUS_TOKEN: Token = Token::OtherChar(b'+');
    /// A minus sign.
    pub const MINUS_TOKEN: Token = Token::OtherChar(b'-');
    /// A right-brace token where the character is '}'.
    pub const RIGHT_BRACE_TOKEN: Token = Token::RightBrace(b'}');
    /// A left-brace token where the character is '{'.
    pub const LEFT_BRACE_TOKEN: Token = Token::LeftBrace(b'{');
    /// A math-shift token where the character is '$'.
    pub const MATH_SHIFT_TOKEN: Token = Token::MathShift(b'$');

    pub fn is_left_brace(&self) -> bool {
        matches!(self, Self::LeftBrace(_))
    }

    pub fn is_right_brace(&self) -> bool {
        matches!(self, Self::RightBrace(_))
    }

    /// See 293. and 294.
    pub fn display(&self, printer: &mut impl Printer, eqtb: &Eqtb) {
        match *self {
            Token::LeftBrace(c)
            | Token::RightBrace(c)
            | Token::MathShift(c)
            | Token::TabMark(c)
            | Token::SuperMark(c)
            | Token::SubMark(c)
            | Token::Spacer(c)
            | Token::Letter(c)
            | Token::OtherChar(c) => printer.print(c),
            Token::MacParam(c) => {
                printer.print(c);
                printer.print(c);
            }
            Token::CSToken { cs } => cs.print_cs(eqtb, printer),
            Token::Null => panic!("Should not be possible"),
        }
    }
}

impl Dumpable for Token {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::LeftBrace(c) => {
                writeln!(target, "LeftBrace")?;
                c.dump(target)?;
            }
            Self::RightBrace(c) => {
                writeln!(target, "RightBrace")?;
                c.dump(target)?;
            }
            Self::MathShift(c) => {
                writeln!(target, "MathShift")?;
                c.dump(target)?;
            }
            Self::TabMark(c) => {
                writeln!(target, "TabMark")?;
                c.dump(target)?;
            }
            Self::MacParam(c) => {
                writeln!(target, "MacParam")?;
                c.dump(target)?;
            }
            Self::SuperMark(c) => {
                writeln!(target, "SuperMark")?;
                c.dump(target)?;
            }
            Self::SubMark(c) => {
                writeln!(target, "SubMark")?;
                c.dump(target)?;
            }
            Self::Spacer(c) => {
                writeln!(target, "Spacer")?;
                c.dump(target)?;
            }
            Self::Letter(c) => {
                writeln!(target, "Letter")?;
                c.dump(target)?;
            }
            Self::OtherChar(c) => {
                writeln!(target, "OtherChar")?;
                c.dump(target)?;
            }
            Self::Null => writeln!(target, "Null")?,
            Self::CSToken { cs } => {
                writeln!(target, "CSToken")?;
                cs.dump(target)?;
            }
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "LeftBrace" => {
                let c = u8::undump(lines)?;
                Ok(Self::LeftBrace(c))
            }
            "RightBrace" => {
                let c = u8::undump(lines)?;
                Ok(Self::RightBrace(c))
            }
            "MathShift" => {
                let c = u8::undump(lines)?;
                Ok(Self::MathShift(c))
            }
            "TabMark" => {
                let c = u8::undump(lines)?;
                Ok(Self::TabMark(c))
            }
            "MacParam" => {
                let c = u8::undump(lines)?;
                Ok(Self::MacParam(c))
            }
            "SuperMark" => {
                let c = u8::undump(lines)?;
                Ok(Self::SuperMark(c))
            }
            "SubMark" => {
                let c = u8::undump(lines)?;
                Ok(Self::SubMark(c))
            }
            "Spacer" => {
                let c = u8::undump(lines)?;
                Ok(Self::Spacer(c))
            }
            "Letter" => {
                let c = u8::undump(lines)?;
                Ok(Self::Letter(c))
            }
            "OtherChar" => {
                let c = u8::undump(lines)?;
                Ok(Self::OtherChar(c))
            }
            "Null" => Ok(Self::Null),
            "CSToken" => {
                let cs = ControlSequence::undump(lines)?;
                Ok(Self::CSToken { cs })
            }
            _ => Err(FormatError::ParseError),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dump_token() {
        let char_token = Token::LeftBrace(12);
        let cs_token = Token::CSToken {
            cs: ControlSequence::Escaped(12),
        };

        let mut file = Vec::new();
        char_token.dump(&mut file).unwrap();
        cs_token.dump(&mut file).unwrap();

        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let char_token_undumped = Token::undump(&mut lines).unwrap();
        let cs_token_undumped = Token::undump(&mut lines).unwrap();
        assert_eq!(char_token, char_token_undumped);
        assert_eq!(cs_token, cs_token_undumped);
    }
}
