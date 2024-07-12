use crate::format::{Dumpable, FormatError};

use std::io::Write;

const NULL_CODE: u8 = 0;
pub const CARRIAGE_RETURN: u8 = 13;
const INVALID_CODE: u8 = 127;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CatCode {
    Escape = 0,
    LeftBrace = 1,
    RightBrace = 2,
    MathShift = 3,
    TabMark = 4,
    CarRet = 5,
    MacParam = 6,
    SupMark = 7,
    SubMark = 8,
    Ignore = 9,
    Spacer = 10,
    Letter = 11,
    OtherChar = 12,
    ActiveChar = 13,
    Comment = 14,
    InvalidChar = 15,
}

impl TryFrom<i32> for CatCode {
    type Error = ();

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        let cat_code = match value {
            0 => Self::Escape,
            1 => Self::LeftBrace,
            2 => Self::RightBrace,
            3 => Self::MathShift,
            4 => Self::TabMark,
            5 => Self::CarRet,
            6 => Self::MacParam,
            7 => Self::SupMark,
            8 => Self::SubMark,
            9 => Self::Ignore,
            10 => Self::Spacer,
            11 => Self::Letter,
            12 => Self::OtherChar,
            13 => Self::ActiveChar,
            14 => Self::Comment,
            15 => Self::InvalidChar,
            _ => return Err(()),
        };
        Ok(cat_code)
    }
}

impl Dumpable for CatCode {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::Escape => writeln!(target, "Escape")?,
            Self::LeftBrace => writeln!(target, "LeftBrace")?,
            Self::RightBrace => writeln!(target, "RightBrace")?,
            Self::MathShift => writeln!(target, "MathShift")?,
            Self::TabMark => writeln!(target, "TabMark")?,
            Self::CarRet => writeln!(target, "CarRet")?,
            Self::MacParam => writeln!(target, "MacParam")?,
            Self::SupMark => writeln!(target, "SupMark")?,
            Self::SubMark => writeln!(target, "SubMark")?,
            Self::Ignore => writeln!(target, "Ignore")?,
            Self::Spacer => writeln!(target, "Spacer")?,
            Self::Letter => writeln!(target, "Letter")?,
            Self::OtherChar => writeln!(target, "OtherChar")?,
            Self::ActiveChar => writeln!(target, "ActiveChar")?,
            Self::Comment => writeln!(target, "Comment")?,
            Self::InvalidChar => writeln!(target, "InvalidChar")?,
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "Escape" => Ok(Self::Escape),
            "LeftBrace" => Ok(Self::LeftBrace),
            "RightBrace" => Ok(Self::RightBrace),
            "MathShift" => Ok(Self::MathShift),
            "TabMark" => Ok(Self::TabMark),
            "CarRet" => Ok(Self::CarRet),
            "MacParam" => Ok(Self::MacParam),
            "SupMark" => Ok(Self::SupMark),
            "SubMark" => Ok(Self::SubMark),
            "Ignore" => Ok(Self::Ignore),
            "Spacer" => Ok(Self::Spacer),
            "Letter" => Ok(Self::Letter),
            "OtherChar" => Ok(Self::OtherChar),
            "ActiveChar" => Ok(Self::ActiveChar),
            "Comment" => Ok(Self::Comment),
            "InvalidChar" => Ok(Self::InvalidChar),
            _ => Err(FormatError::ParseError),
        }
    }
}

/// See 247.
pub struct CatCodes {
    cat_codes: [CatCode; 256],
}

impl CatCodes {
    /// See 232. and 240.
    pub fn new() -> Self {
        let mut params = Self {
            cat_codes: [CatCode::OtherChar; 256],
        };
        params.cat_codes[CARRIAGE_RETURN as usize] = CatCode::CarRet;
        params.cat_codes[b' ' as usize] = CatCode::Spacer;
        params.cat_codes[b'\\' as usize] = CatCode::Escape;
        params.cat_codes[b'%' as usize] = CatCode::Comment;
        params.cat_codes[INVALID_CODE as usize] = CatCode::InvalidChar;
        params.cat_codes[NULL_CODE as usize] = CatCode::Ignore;

        for k in b'A'..=b'Z' {
            params.cat_codes[k as usize] = CatCode::Letter;
            params.cat_codes[(k + b'a' - b'A') as usize] = CatCode::Letter;
        }

        params
    }

    fn index(&self, chr: u8) -> &CatCode {
        &self.cat_codes[chr as usize]
    }

    fn index_mut(&mut self, chr: u8) -> &mut CatCode {
        &mut self.cat_codes[chr as usize]
    }

    pub fn get(&self, chr: u8) -> &CatCode {
        self.index(chr)
    }

    pub fn set(&mut self, chr: u8, new_cat_code: CatCode) -> CatCode {
        let prev_value = *self.index_mut(chr);
        *self.index_mut(chr) = new_cat_code;
        prev_value
    }
}

impl Dumpable for CatCodes {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        for char_code in self.cat_codes {
            char_code.dump(target)?;
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let mut cat_codes = std::array::from_fn(|_| CatCode::InvalidChar);
        for i in 0..256 {
            cat_codes[i] = CatCode::undump(lines)?;
        }
        Ok(Self { cat_codes })
    }
}
