use super::RegisterIndex;
use crate::format::{Dumpable, FormatError};
use crate::nodes::GlueSpec;

use std::io::Write;
use std::rc::Rc;

pub type Skip = Rc<GlueSpec>;

/// Named glue specifications.
/// See 224.
pub struct SkipParameters {
    line_skip: Skip,
    baseline_skip: Skip,
    par_skip: Skip,
    above_display_skip: Skip,
    below_display_skip: Skip,
    above_display_short_skip: Skip,
    below_display_short_skip: Skip,
    left_skip: Skip,
    right_skip: Skip,
    top_skip: Skip,
    split_top_skip: Skip,
    tab_skip: Skip,
    space_skip: Skip,
    xspace_skip: Skip,
    par_fill_skip: Skip,
    thin_mu_skip: Skip,
    med_mu_skip: Skip,
    thick_mu_skip: Skip,
    skips: Vec<Skip>,
    mu_skips: Vec<Skip>,
}

impl SkipParameters {
    /// All skip variables are initiated with zero.
    /// See 228.
    pub fn new() -> Self {
        Self {
            line_skip: GlueSpec::zero_glue(),
            baseline_skip: GlueSpec::zero_glue(),
            par_skip: GlueSpec::zero_glue(),
            above_display_skip: GlueSpec::zero_glue(),
            below_display_skip: GlueSpec::zero_glue(),
            above_display_short_skip: GlueSpec::zero_glue(),
            below_display_short_skip: GlueSpec::zero_glue(),
            left_skip: GlueSpec::zero_glue(),
            right_skip: GlueSpec::zero_glue(),
            top_skip: GlueSpec::zero_glue(),
            split_top_skip: GlueSpec::zero_glue(),
            tab_skip: GlueSpec::zero_glue(),
            space_skip: GlueSpec::zero_glue(),
            xspace_skip: GlueSpec::zero_glue(),
            par_fill_skip: GlueSpec::zero_glue(),
            thin_mu_skip: GlueSpec::zero_glue(),
            med_mu_skip: GlueSpec::zero_glue(),
            thick_mu_skip: GlueSpec::zero_glue(),
            skips: vec![GlueSpec::zero_glue(); RegisterIndex::MAX as usize + 1],
            mu_skips: vec![GlueSpec::zero_glue(); RegisterIndex::MAX as usize + 1],
        }
    }

    fn index(&self, index: SkipVariable) -> &Skip {
        match index {
            SkipVariable::LineSkip => &self.line_skip,
            SkipVariable::BaselineSkip => &self.baseline_skip,
            SkipVariable::ParSkip => &self.par_skip,
            SkipVariable::AboveDisplaySkip => &self.above_display_skip,
            SkipVariable::BelowDisplaySkip => &self.below_display_skip,
            SkipVariable::AboveDisplayShortSkip => &self.above_display_short_skip,
            SkipVariable::BelowDisplayShortSkip => &self.below_display_short_skip,
            SkipVariable::LeftSkip => &self.left_skip,
            SkipVariable::RightSkip => &self.right_skip,
            SkipVariable::TopSkip => &self.top_skip,
            SkipVariable::SplitTopSkip => &self.split_top_skip,
            SkipVariable::TabSkip => &self.tab_skip,
            SkipVariable::SpaceSkip => &self.space_skip,
            SkipVariable::XspaceSkip => &self.xspace_skip,
            SkipVariable::ParFillSkip => &self.par_fill_skip,
            SkipVariable::ThinMuSkip => &self.thin_mu_skip,
            SkipVariable::MedMuSkip => &self.med_mu_skip,
            SkipVariable::ThickMuSkip => &self.thick_mu_skip,
            SkipVariable::Skip(register) => &self.skips[register as usize],
            SkipVariable::MuSkip(register) => &self.mu_skips[register as usize],
        }
    }

    fn index_mut(&mut self, index: SkipVariable) -> &mut Skip {
        match index {
            SkipVariable::LineSkip => &mut self.line_skip,
            SkipVariable::BaselineSkip => &mut self.baseline_skip,
            SkipVariable::ParSkip => &mut self.par_skip,
            SkipVariable::AboveDisplaySkip => &mut self.above_display_skip,
            SkipVariable::BelowDisplaySkip => &mut self.below_display_skip,
            SkipVariable::AboveDisplayShortSkip => &mut self.above_display_short_skip,
            SkipVariable::BelowDisplayShortSkip => &mut self.below_display_short_skip,
            SkipVariable::LeftSkip => &mut self.left_skip,
            SkipVariable::RightSkip => &mut self.right_skip,
            SkipVariable::TopSkip => &mut self.top_skip,
            SkipVariable::SplitTopSkip => &mut self.split_top_skip,
            SkipVariable::TabSkip => &mut self.tab_skip,
            SkipVariable::SpaceSkip => &mut self.space_skip,
            SkipVariable::XspaceSkip => &mut self.xspace_skip,
            SkipVariable::ParFillSkip => &mut self.par_fill_skip,
            SkipVariable::ThinMuSkip => &mut self.thin_mu_skip,
            SkipVariable::MedMuSkip => &mut self.med_mu_skip,
            SkipVariable::ThickMuSkip => &mut self.thick_mu_skip,
            SkipVariable::Skip(register) => &mut self.skips[register as usize],
            SkipVariable::MuSkip(register) => &mut self.mu_skips[register as usize],
        }
    }

    pub fn get(&self, skip_var: SkipVariable) -> &Skip {
        self.index(skip_var)
    }

    pub fn set(&mut self, skip_var: SkipVariable, new_value: Skip) -> Skip {
        std::mem::replace(self.index_mut(skip_var), new_value)
    }
}

/// Specifies a skip variable in the Eqtb.
/// See 224.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SkipVariable {
    LineSkip,
    BaselineSkip,
    ParSkip,
    AboveDisplaySkip,
    BelowDisplaySkip,
    AboveDisplayShortSkip,
    BelowDisplayShortSkip,
    LeftSkip,
    RightSkip,
    TopSkip,
    SplitTopSkip,
    TabSkip,
    SpaceSkip,
    XspaceSkip,
    ParFillSkip,
    ThinMuSkip,
    MedMuSkip,
    ThickMuSkip,
    Skip(RegisterIndex),
    MuSkip(RegisterIndex),
}

impl SkipVariable {
    /// The name of the skip variable without preceding escape character.
    /// See 248., 249., and 251.
    pub fn to_string(self) -> Vec<u8> {
        match self {
            Self::LineSkip => b"lineskip".to_vec(),
            Self::BaselineSkip => b"baselineskip".to_vec(),
            Self::ParSkip => b"parskip".to_vec(),
            Self::AboveDisplaySkip => b"abovedisplayskip".to_vec(),
            Self::BelowDisplaySkip => b"belowdisplayskip".to_vec(),
            Self::AboveDisplayShortSkip => b"abovedisplayshortskip".to_vec(),
            Self::BelowDisplayShortSkip => b"belowdisplayshortskip".to_vec(),
            Self::LeftSkip => b"leftskip".to_vec(),
            Self::RightSkip => b"rightskip".to_vec(),
            Self::TopSkip => b"topskip".to_vec(),
            Self::SplitTopSkip => b"splittopskip".to_vec(),
            Self::TabSkip => b"tabskip".to_vec(),
            Self::SpaceSkip => b"spaceskip".to_vec(),
            Self::XspaceSkip => b"xspaceskip".to_vec(),
            Self::ParFillSkip => b"parfillskip".to_vec(),
            Self::ThinMuSkip => b"thinmuskip".to_vec(),
            Self::MedMuSkip => b"medmuskip".to_vec(),
            Self::ThickMuSkip => b"thickmuskip".to_vec(),
            Self::Skip(register) => format!("skip{}", register).as_bytes().to_vec(),
            Self::MuSkip(register) => format!("muskip{}", register).as_bytes().to_vec(),
        }
    }
}

impl Dumpable for SkipParameters {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.line_skip.dump(target)?;
        self.baseline_skip.dump(target)?;
        self.par_skip.dump(target)?;
        self.above_display_skip.dump(target)?;
        self.below_display_skip.dump(target)?;
        self.above_display_short_skip.dump(target)?;
        self.below_display_short_skip.dump(target)?;
        self.left_skip.dump(target)?;
        self.right_skip.dump(target)?;
        self.top_skip.dump(target)?;
        self.split_top_skip.dump(target)?;
        self.tab_skip.dump(target)?;
        self.space_skip.dump(target)?;
        self.xspace_skip.dump(target)?;
        self.par_fill_skip.dump(target)?;
        self.thin_mu_skip.dump(target)?;
        self.med_mu_skip.dump(target)?;
        self.thick_mu_skip.dump(target)?;
        self.skips.dump(target)?;
        self.mu_skips.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let line_skip = <Skip>::undump(lines)?;
        let baseline_skip = <Skip>::undump(lines)?;
        let par_skip = <Skip>::undump(lines)?;
        let above_display_skip = <Skip>::undump(lines)?;
        let below_display_skip = <Skip>::undump(lines)?;
        let above_display_short_skip = <Skip>::undump(lines)?;
        let below_display_short_skip = <Skip>::undump(lines)?;
        let left_skip = <Skip>::undump(lines)?;
        let right_skip = <Skip>::undump(lines)?;
        let top_skip = <Skip>::undump(lines)?;
        let split_top_skip = <Skip>::undump(lines)?;
        let tab_skip = <Skip>::undump(lines)?;
        let space_skip = <Skip>::undump(lines)?;
        let xspace_skip = <Skip>::undump(lines)?;
        let par_fill_skip = <Skip>::undump(lines)?;
        let thin_mu_skip = <Skip>::undump(lines)?;
        let med_mu_skip = <Skip>::undump(lines)?;
        let thick_mu_skip = <Skip>::undump(lines)?;

        let skips = Dumpable::undump(lines)?;
        let mu_skips = Dumpable::undump(lines)?;
        Ok(Self {
            line_skip,
            baseline_skip,
            par_skip,
            above_display_skip,
            below_display_skip,
            above_display_short_skip,
            below_display_short_skip,
            left_skip,
            right_skip,
            top_skip,
            split_top_skip,
            tab_skip,
            space_skip,
            xspace_skip,
            par_fill_skip,
            thin_mu_skip,
            med_mu_skip,
            thick_mu_skip,
            skips,
            mu_skips,
        })
    }
}

impl Dumpable for SkipVariable {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::LineSkip => writeln!(target, "LineSkip")?,
            Self::BaselineSkip => writeln!(target, "BaselineSkip")?,
            Self::ParSkip => writeln!(target, "ParSkip")?,
            Self::AboveDisplaySkip => writeln!(target, "AboveDisplaySkip")?,
            Self::BelowDisplaySkip => writeln!(target, "BelowDisplaySkip")?,
            Self::AboveDisplayShortSkip => writeln!(target, "AboveDisplayShortSkip")?,
            Self::BelowDisplayShortSkip => writeln!(target, "BelowDisplayShortSkip")?,
            Self::LeftSkip => writeln!(target, "LeftSkip")?,
            Self::RightSkip => writeln!(target, "RightSkip")?,
            Self::TopSkip => writeln!(target, "TopSkip")?,
            Self::SplitTopSkip => writeln!(target, "SplitTopSkip")?,
            Self::TabSkip => writeln!(target, "TabSkip")?,
            Self::SpaceSkip => writeln!(target, "SpaceSkip")?,
            Self::XspaceSkip => writeln!(target, "XspaceSkip")?,
            Self::ParFillSkip => writeln!(target, "ParFillSkip")?,
            Self::ThinMuSkip => writeln!(target, "ThinMuSkip")?,
            Self::MedMuSkip => writeln!(target, "MedMuSkip")?,
            Self::ThickMuSkip => writeln!(target, "ThickMuSkip")?,
            Self::Skip(n) => {
                writeln!(target, "Skip")?;
                n.dump(target)?;
            }
            Self::MuSkip(n) => {
                writeln!(target, "MuSkip")?;
                n.dump(target)?;
            }
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        let skip_var = match variant {
            "LineSkip" => Self::LineSkip,
            "BaselineSkip" => Self::BaselineSkip,
            "ParSkip" => Self::ParSkip,
            "AboveDisplaySkip" => Self::AboveDisplaySkip,
            "BelowDisplaySkip" => Self::BelowDisplaySkip,
            "AboveDisplayShortSkip" => Self::AboveDisplayShortSkip,
            "BelowDisplayShortSkip" => Self::BelowDisplayShortSkip,
            "LeftSkip" => Self::LeftSkip,
            "RightSkip" => Self::RightSkip,
            "TopSkip" => Self::TopSkip,
            "SplitTopSkip" => Self::SplitTopSkip,
            "TabSkip" => Self::TabSkip,
            "SpaceSkip" => Self::SpaceSkip,
            "XspaceSkip" => Self::XspaceSkip,
            "ParFillSkip" => Self::ParFillSkip,
            "ThinMuSkip" => Self::ThinMuSkip,
            "MedMuSkip" => Self::MedMuSkip,
            "ThickMuSkip" => Self::ThickMuSkip,
            "Skip" => {
                let n = RegisterIndex::undump(lines)?;
                Self::Skip(n)
            }
            "MuSkip" => {
                let n = RegisterIndex::undump(lines)?;
                Self::MuSkip(n)
            }
            _ => return Err(FormatError::ParseError),
        };
        Ok(skip_var)
    }
}
