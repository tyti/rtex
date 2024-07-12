use crate::format::{Dumpable, FormatError};

use std::io::Write;

/// Mask for indicating a math symbol of class 7 (Variable).
pub const VAR_CODE: u16 = 0x7000;

/// See 247.
pub struct CodeParameters {
    lc_codes: [i32; 256],
    uc_codes: [i32; 256],
    sf_codes: [i32; 256],
    math_codes: [i32; 256],
    del_codes: [i32; 256],
}

impl CodeParameters {
    /// See 232. and 240.
    pub fn new() -> Self {
        let mut params = Self {
            lc_codes: [0; 256],
            uc_codes: [0; 256],
            sf_codes: [1000; 256],
            math_codes: [0; 256],
            del_codes: [-1; 256],
        };

        for k in 0..256 {
            params.math_codes[k] = k as i32;
        }
        for k in b'0'..=b'9' {
            params.math_codes[k as usize] = k as i32 + VAR_CODE as i32;
        }

        for k in b'A'..=b'Z' {
            params.math_codes[k as usize] = k as i32 + VAR_CODE as i32 + 0x100;
            params.math_codes[(k + b'a' - b'A') as usize] =
                (k + b'a' - b'A') as i32 + VAR_CODE as i32 + 0x100;
            params.lc_codes[k as usize] = (k + b'a' - b'A') as i32;
            params.lc_codes[(k + b'a' - b'A') as usize] = (k + b'a' - b'A') as i32;
            params.uc_codes[k as usize] = k as i32;
            params.uc_codes[(k + b'a' - b'A') as usize] = k as i32;
            params.sf_codes[k as usize] = 999;
        }

        params.del_codes[b'.' as usize] = 0;
        params
    }

    fn index(&self, index: CodeVariable) -> &i32 {
        match index {
            CodeVariable::LcCode(n) => &self.lc_codes[n],
            CodeVariable::UcCode(n) => &self.uc_codes[n],
            CodeVariable::SfCode(n) => &self.sf_codes[n],
            CodeVariable::MathCode(n) => &self.math_codes[n],
            CodeVariable::DelCode(n) => &self.del_codes[n],
        }
    }

    fn index_mut(&mut self, index: CodeVariable) -> &mut i32 {
        match index {
            CodeVariable::LcCode(n) => &mut self.lc_codes[n],
            CodeVariable::UcCode(n) => &mut self.uc_codes[n],
            CodeVariable::SfCode(n) => &mut self.sf_codes[n],
            CodeVariable::MathCode(n) => &mut self.math_codes[n],
            CodeVariable::DelCode(n) => &mut self.del_codes[n],
        }
    }

    pub fn get(&self, code_var: CodeVariable) -> &i32 {
        self.index(code_var)
    }

    pub fn set(&mut self, code_var: CodeVariable, new_value: i32) -> i32 {
        let prev_value = *self.index_mut(code_var);
        *self.index_mut(code_var) = new_value;
        prev_value
    }
}

/// Specifies a code variable in the Eqtb.
/// Note that we include the delimiter codes here as well
/// that are stored separately from the others in TeX82
/// See 230. and 236.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CodeVariable {
    LcCode(usize),
    UcCode(usize),
    SfCode(usize),
    MathCode(usize),
    DelCode(usize),
}

impl CodeVariable {
    /// The name of the code variable without preceding escape character.
    /// See 235. and 242.
    pub fn to_string(self) -> Vec<u8> {
        match self {
            Self::LcCode(n) => format!("lccode{}", n).as_bytes().to_vec(),
            Self::UcCode(n) => format!("uccode{}", n).as_bytes().to_vec(),
            Self::SfCode(n) => format!("sfcode{}", n).as_bytes().to_vec(),
            Self::MathCode(n) => format!("mathcode{}", n).as_bytes().to_vec(),
            Self::DelCode(n) => format!("delcode{}", n).as_bytes().to_vec(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CodeType {
    LcCode,
    UcCode,
    SfCode,
    MathCode,
    DelCode,
}

impl CodeType {
    pub fn to_variable(self, n: usize) -> CodeVariable {
        match self {
            Self::LcCode => CodeVariable::LcCode(n),
            Self::UcCode => CodeVariable::UcCode(n),
            Self::SfCode => CodeVariable::SfCode(n),
            Self::MathCode => CodeVariable::MathCode(n),
            Self::DelCode => CodeVariable::DelCode(n),
        }
    }

    /// The type of the code without preceding escape character.
    /// See 235. and 242.
    pub fn as_str(&self) -> &[u8] {
        match self {
            Self::LcCode => b"lccode",
            Self::UcCode => b"uccode",
            Self::SfCode => b"sfcode",
            Self::MathCode => b"mathcode",
            Self::DelCode => b"delcode",
        }
    }
}

impl Dumpable for CodeVariable {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::LcCode(n) => {
                writeln!(target, "LcCode")?;
                n.dump(target)?;
            }
            Self::UcCode(n) => {
                writeln!(target, "UcCode")?;
                n.dump(target)?;
            }
            Self::SfCode(n) => {
                writeln!(target, "SfCode")?;
                n.dump(target)?;
            }
            Self::MathCode(n) => {
                writeln!(target, "MathCode")?;
                n.dump(target)?;
            }
            Self::DelCode(n) => {
                writeln!(target, "DelCode")?;
                n.dump(target)?;
            }
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "LcCode" => {
                let n = usize::undump(lines)?;
                Ok(Self::LcCode(n))
            }
            "UcCode" => {
                let n = usize::undump(lines)?;
                Ok(Self::UcCode(n))
            }
            "SfCode" => {
                let n = usize::undump(lines)?;
                Ok(Self::SfCode(n))
            }
            "MathCode" => {
                let n = usize::undump(lines)?;
                Ok(Self::MathCode(n))
            }
            "DelCode" => {
                let n = usize::undump(lines)?;
                Ok(Self::DelCode(n))
            }
            _ => Err(FormatError::ParseError),
        }
    }
}

impl Dumpable for CodeParameters {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.lc_codes.dump(target)?;
        self.uc_codes.dump(target)?;
        self.sf_codes.dump(target)?;
        self.math_codes.dump(target)?;
        self.del_codes.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let lc_codes = Dumpable::undump(lines)?;
        let uc_codes = Dumpable::undump(lines)?;
        let sf_codes = Dumpable::undump(lines)?;
        let math_codes = Dumpable::undump(lines)?;
        let del_codes = Dumpable::undump(lines)?;
        Ok(Self {
            lc_codes,
            uc_codes,
            sf_codes,
            math_codes,
            del_codes,
        })
    }
}

impl Dumpable for CodeType {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::LcCode => {
                writeln!(target, "LcCode")?;
            }
            Self::UcCode => {
                writeln!(target, "UcCode")?;
            }
            Self::SfCode => {
                writeln!(target, "SfCode")?;
            }
            Self::MathCode => {
                writeln!(target, "MathCode")?;
            }
            Self::DelCode => {
                writeln!(target, "DelCode")?;
            }
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "LcCode" => Ok(Self::LcCode),
            "UcCode" => Ok(Self::UcCode),
            "SfCode" => Ok(Self::SfCode),
            "MathCode" => Ok(Self::MathCode),
            "DelCode" => Ok(Self::DelCode),
            _ => Err(FormatError::ParseError),
        }
    }
}
