use super::RegisterIndex;

use crate::format::{Dumpable, FormatError};
use crate::token_lists::RcTokenList;

use std::io::Write;

/// See 230.
pub struct TokenListParameters {
    output_routine: Option<RcTokenList>,
    every_par: Option<RcTokenList>,
    every_math: Option<RcTokenList>,
    every_display: Option<RcTokenList>,
    every_hbox: Option<RcTokenList>,
    every_vbox: Option<RcTokenList>,
    every_job: Option<RcTokenList>,
    every_cr: Option<RcTokenList>,
    err_help: Option<RcTokenList>,
    toks: Vec<Option<RcTokenList>>,
}

impl TokenListParameters {
    /// All token list variables are undefined initially.
    /// See 232.
    pub fn new() -> Self {
        Self {
            output_routine: None,
            every_par: None,
            every_math: None,
            every_display: None,
            every_hbox: None,
            every_vbox: None,
            every_job: None,
            every_cr: None,
            err_help: None,
            toks: vec![None; RegisterIndex::MAX as usize + 1],
        }
    }

    fn index(&self, index: TokenListVariable) -> &Option<RcTokenList> {
        match index {
            TokenListVariable::OutputRoutine => &self.output_routine,
            TokenListVariable::EveryPar => &self.every_par,
            TokenListVariable::EveryMath => &self.every_math,
            TokenListVariable::EveryDisplay => &self.every_display,
            TokenListVariable::EveryHbox => &self.every_hbox,
            TokenListVariable::EveryVbox => &self.every_vbox,
            TokenListVariable::EveryJob => &self.every_job,
            TokenListVariable::EveryCr => &self.every_cr,
            TokenListVariable::ErrHelp => &self.err_help,
            TokenListVariable::Toks(register) => &self.toks[register as usize],
        }
    }

    fn index_mut(&mut self, index: TokenListVariable) -> &mut Option<RcTokenList> {
        match index {
            TokenListVariable::OutputRoutine => &mut self.output_routine,
            TokenListVariable::EveryPar => &mut self.every_par,
            TokenListVariable::EveryMath => &mut self.every_math,
            TokenListVariable::EveryDisplay => &mut self.every_display,
            TokenListVariable::EveryHbox => &mut self.every_hbox,
            TokenListVariable::EveryVbox => &mut self.every_vbox,
            TokenListVariable::EveryJob => &mut self.every_job,
            TokenListVariable::EveryCr => &mut self.every_cr,
            TokenListVariable::ErrHelp => &mut self.err_help,
            TokenListVariable::Toks(register) => &mut self.toks[register as usize],
        }
    }

    pub fn get(&self, token_list_var: TokenListVariable) -> &Option<RcTokenList> {
        self.index(token_list_var)
    }

    pub fn set(
        &mut self,
        token_list_var: TokenListVariable,
        new_value: Option<RcTokenList>,
    ) -> Option<RcTokenList> {
        std::mem::replace(self.index_mut(token_list_var), new_value)
    }
}

/// Specifies a token list variable in the Eqtb.
/// See 230.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenListVariable {
    OutputRoutine,
    EveryPar,
    EveryMath,
    EveryDisplay,
    EveryHbox,
    EveryVbox,
    EveryJob,
    EveryCr,
    ErrHelp,
    Toks(RegisterIndex),
}

impl TokenListVariable {
    /// The name of the token list variable without preceding escape character.
    /// See 231. and 233.
    pub fn to_string(self) -> Vec<u8> {
        match self {
            Self::OutputRoutine => b"output".to_vec(),
            Self::EveryPar => b"everypar".to_vec(),
            Self::EveryMath => b"everymath".to_vec(),
            Self::EveryDisplay => b"everydisplay".to_vec(),
            Self::EveryHbox => b"everyhbox".to_vec(),
            Self::EveryVbox => b"everyvbox".to_vec(),
            Self::EveryJob => b"everyjob".to_vec(),
            Self::EveryCr => b"everycr".to_vec(),
            Self::ErrHelp => b"errhelp".to_vec(),
            Self::Toks(register) => format!("toks{}", register).as_bytes().to_vec(),
        }
    }
}

impl Dumpable for TokenListParameters {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.output_routine.dump(target)?;
        self.every_par.dump(target)?;
        self.every_math.dump(target)?;
        self.every_display.dump(target)?;
        self.every_hbox.dump(target)?;
        self.every_vbox.dump(target)?;
        self.every_job.dump(target)?;
        self.every_cr.dump(target)?;
        self.err_help.dump(target)?;
        self.toks.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let output_routine = <Option<RcTokenList>>::undump(lines)?;
        let every_par = <Option<RcTokenList>>::undump(lines)?;
        let every_math = <Option<RcTokenList>>::undump(lines)?;
        let every_display = <Option<RcTokenList>>::undump(lines)?;
        let every_hbox = <Option<RcTokenList>>::undump(lines)?;
        let every_vbox = <Option<RcTokenList>>::undump(lines)?;
        let every_job = <Option<RcTokenList>>::undump(lines)?;
        let every_cr = <Option<RcTokenList>>::undump(lines)?;
        let err_help = <Option<RcTokenList>>::undump(lines)?;
        let toks = Dumpable::undump(lines)?;
        Ok(Self {
            output_routine,
            every_par,
            every_math,
            every_display,
            every_hbox,
            every_vbox,
            every_job,
            every_cr,
            err_help,
            toks,
        })
    }
}

impl Dumpable for TokenListVariable {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::OutputRoutine => writeln!(target, "OutputRoutine")?,
            Self::EveryPar => writeln!(target, "EveryPar")?,
            Self::EveryMath => writeln!(target, "EveryMath")?,
            Self::EveryDisplay => writeln!(target, "EveryDisplay")?,
            Self::EveryHbox => writeln!(target, "EveryHbox")?,
            Self::EveryVbox => writeln!(target, "EveryVbox")?,
            Self::EveryJob => writeln!(target, "EveryJob")?,
            Self::EveryCr => writeln!(target, "EveryCr")?,
            Self::ErrHelp => writeln!(target, "ErrHelp")?,
            Self::Toks(n) => {
                writeln!(target, "Toks")?;
                n.dump(target)?;
            }
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "OutputRoutine" => Ok(Self::OutputRoutine),
            "EveryPar" => Ok(Self::EveryPar),
            "EveryMath" => Ok(Self::EveryMath),
            "EveryDisplay" => Ok(Self::EveryDisplay),
            "EveryHbox" => Ok(Self::EveryHbox),
            "EveryVbox" => Ok(Self::EveryVbox),
            "EveryJob" => Ok(Self::EveryJob),
            "EveryCr" => Ok(Self::EveryCr),
            "ErrHelp" => Ok(Self::ErrHelp),
            "Toks" => {
                let n = RegisterIndex::undump(lines)?;
                Ok(Self::Toks(n))
            }
            _ => Err(FormatError::ParseError),
        }
    }
}
