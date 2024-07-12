use super::RegisterIndex;
use crate::dimension::Dimension;
use crate::format::{Dumpable, FormatError};

use std::io::Write;

/// See 247.
pub struct DimensionParameters {
    par_indent: Dimension,
    math_surround: Dimension,
    line_skip_limit: Dimension,
    hsize: Dimension,
    vsize: Dimension,
    max_depth: Dimension,
    split_max_depth: Dimension,
    box_max_depth: Dimension,
    hfuzz: Dimension,
    vfuzz: Dimension,
    delimiter_shortfall: Dimension,
    null_delimiter_space: Dimension,
    script_space: Dimension,
    pre_display_size: Dimension,
    display_width: Dimension,
    display_indent: Dimension,
    overfull_rule: Dimension,
    hang_indent: Dimension,
    h_offset: Dimension,
    v_offset: Dimension,
    emergency_stretch: Dimension,
    dimens: Vec<Dimension>,
}

impl DimensionParameters {
    /// All dimension variables are initiated with zero.
    /// See 250.
    pub fn new() -> Self {
        Self {
            par_indent: 0,
            math_surround: 0,
            line_skip_limit: 0,
            hsize: 0,
            vsize: 0,
            max_depth: 0,
            split_max_depth: 0,
            box_max_depth: 0,
            hfuzz: 0,
            vfuzz: 0,
            delimiter_shortfall: 0,
            null_delimiter_space: 0,
            script_space: 0,
            pre_display_size: 0,
            display_width: 0,
            display_indent: 0,
            overfull_rule: 0,
            hang_indent: 0,
            h_offset: 0,
            v_offset: 0,
            emergency_stretch: 0,
            dimens: vec![0; RegisterIndex::MAX as usize + 1],
        }
    }

    fn index(&self, index: DimensionVariable) -> &Dimension {
        match index {
            DimensionVariable::ParIndent => &self.par_indent,
            DimensionVariable::MathSurround => &self.math_surround,
            DimensionVariable::LineSkipLimit => &self.line_skip_limit,
            DimensionVariable::Hsize => &self.hsize,
            DimensionVariable::Vsize => &self.vsize,
            DimensionVariable::MaxDepth => &self.max_depth,
            DimensionVariable::SplitMaxDepth => &self.split_max_depth,
            DimensionVariable::BoxMaxDepth => &self.box_max_depth,
            DimensionVariable::Hfuzz => &self.hfuzz,
            DimensionVariable::Vfuzz => &self.vfuzz,
            DimensionVariable::DelimiterShortfall => &self.delimiter_shortfall,
            DimensionVariable::NullDelimiterSpace => &self.null_delimiter_space,
            DimensionVariable::ScriptSpace => &self.script_space,
            DimensionVariable::PreDisplaySize => &self.pre_display_size,
            DimensionVariable::DisplayWidth => &self.display_width,
            DimensionVariable::DisplayIndent => &self.display_indent,
            DimensionVariable::OverfullRule => &self.overfull_rule,
            DimensionVariable::HangIndent => &self.hang_indent,
            DimensionVariable::HOffset => &self.h_offset,
            DimensionVariable::VOffset => &self.v_offset,
            DimensionVariable::EmergencyStretch => &self.emergency_stretch,
            DimensionVariable::Dimen(register) => &self.dimens[register as usize],
        }
    }

    fn index_mut(&mut self, index: DimensionVariable) -> &mut Dimension {
        match index {
            DimensionVariable::ParIndent => &mut self.par_indent,
            DimensionVariable::MathSurround => &mut self.math_surround,
            DimensionVariable::LineSkipLimit => &mut self.line_skip_limit,
            DimensionVariable::Hsize => &mut self.hsize,
            DimensionVariable::Vsize => &mut self.vsize,
            DimensionVariable::MaxDepth => &mut self.max_depth,
            DimensionVariable::SplitMaxDepth => &mut self.split_max_depth,
            DimensionVariable::BoxMaxDepth => &mut self.box_max_depth,
            DimensionVariable::Hfuzz => &mut self.hfuzz,
            DimensionVariable::Vfuzz => &mut self.vfuzz,
            DimensionVariable::DelimiterShortfall => &mut self.delimiter_shortfall,
            DimensionVariable::NullDelimiterSpace => &mut self.null_delimiter_space,
            DimensionVariable::ScriptSpace => &mut self.script_space,
            DimensionVariable::PreDisplaySize => &mut self.pre_display_size,
            DimensionVariable::DisplayWidth => &mut self.display_width,
            DimensionVariable::DisplayIndent => &mut self.display_indent,
            DimensionVariable::OverfullRule => &mut self.overfull_rule,
            DimensionVariable::HangIndent => &mut self.hang_indent,
            DimensionVariable::HOffset => &mut self.h_offset,
            DimensionVariable::VOffset => &mut self.v_offset,
            DimensionVariable::EmergencyStretch => &mut self.emergency_stretch,
            DimensionVariable::Dimen(register) => &mut self.dimens[register as usize],
        }
    }

    pub fn get(&self, dim_var: DimensionVariable) -> &Dimension {
        self.index(dim_var)
    }

    pub fn set(&mut self, dim_var: DimensionVariable, new_value: Dimension) -> Dimension {
        let prev_value = *self.index_mut(dim_var);
        *self.index_mut(dim_var) = new_value;
        prev_value
    }
}

/// Specifies a dimension variable in the Eqtb.
/// See 247.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DimensionVariable {
    ParIndent,
    MathSurround,
    LineSkipLimit,
    Hsize,
    Vsize,
    MaxDepth,
    SplitMaxDepth,
    BoxMaxDepth,
    Hfuzz,
    Vfuzz,
    DelimiterShortfall,
    NullDelimiterSpace,
    ScriptSpace,
    PreDisplaySize,
    DisplayWidth,
    DisplayIndent,
    OverfullRule,
    HangIndent,
    HOffset,
    VOffset,
    EmergencyStretch,
    Dimen(RegisterIndex),
}

impl DimensionVariable {
    /// The name of the dimension variable without preceding escape character.
    /// See 248., 249., and 251.
    pub fn to_string(self) -> Vec<u8> {
        match self {
            Self::ParIndent => b"parindent".to_vec(),
            Self::MathSurround => b"mathsurround".to_vec(),
            Self::LineSkipLimit => b"lineskiplimit".to_vec(),
            Self::Hsize => b"hsize".to_vec(),
            Self::Vsize => b"vsize".to_vec(),
            Self::MaxDepth => b"maxdepth".to_vec(),
            Self::SplitMaxDepth => b"splitmaxdepth".to_vec(),
            Self::BoxMaxDepth => b"boxmaxdepth".to_vec(),
            Self::Hfuzz => b"hfuzz".to_vec(),
            Self::Vfuzz => b"vfuzz".to_vec(),
            Self::DelimiterShortfall => b"delimitershortfall".to_vec(),
            Self::NullDelimiterSpace => b"nulldelimiterspace".to_vec(),
            Self::ScriptSpace => b"scriptspace".to_vec(),
            Self::PreDisplaySize => b"predisplaysize".to_vec(),
            Self::DisplayWidth => b"displaywidth".to_vec(),
            Self::DisplayIndent => b"displayindent".to_vec(),
            Self::OverfullRule => b"overfullrule".to_vec(),
            Self::HangIndent => b"hangindent".to_vec(),
            Self::HOffset => b"hoffset".to_vec(),
            Self::VOffset => b"voffset".to_vec(),
            Self::EmergencyStretch => b"emergencystretch".to_vec(),
            Self::Dimen(register) => format!("dimen{}", register).as_bytes().to_vec(),
        }
    }
}

impl Dumpable for DimensionParameters {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.par_indent.dump(target)?;
        self.math_surround.dump(target)?;
        self.line_skip_limit.dump(target)?;
        self.hsize.dump(target)?;
        self.vsize.dump(target)?;
        self.max_depth.dump(target)?;
        self.split_max_depth.dump(target)?;
        self.box_max_depth.dump(target)?;
        self.hfuzz.dump(target)?;
        self.vfuzz.dump(target)?;
        self.delimiter_shortfall.dump(target)?;
        self.null_delimiter_space.dump(target)?;
        self.script_space.dump(target)?;
        self.pre_display_size.dump(target)?;
        self.display_width.dump(target)?;
        self.display_indent.dump(target)?;
        self.overfull_rule.dump(target)?;
        self.hang_indent.dump(target)?;
        self.h_offset.dump(target)?;
        self.v_offset.dump(target)?;
        self.emergency_stretch.dump(target)?;
        self.dimens.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let par_indent = <Dimension>::undump(lines)?;
        let math_surround = <Dimension>::undump(lines)?;
        let line_skip_limit = <Dimension>::undump(lines)?;
        let hsize = <Dimension>::undump(lines)?;
        let vsize = <Dimension>::undump(lines)?;
        let max_depth = <Dimension>::undump(lines)?;
        let split_max_depth = <Dimension>::undump(lines)?;
        let box_max_depth = <Dimension>::undump(lines)?;
        let hfuzz = <Dimension>::undump(lines)?;
        let vfuzz = <Dimension>::undump(lines)?;
        let delimiter_shortfall = <Dimension>::undump(lines)?;
        let null_delimiter_space = <Dimension>::undump(lines)?;
        let script_space = <Dimension>::undump(lines)?;
        let pre_display_size = <Dimension>::undump(lines)?;
        let display_width = <Dimension>::undump(lines)?;
        let display_indent = <Dimension>::undump(lines)?;
        let overfull_rule = <Dimension>::undump(lines)?;
        let hang_indent = <Dimension>::undump(lines)?;
        let h_offset = <Dimension>::undump(lines)?;
        let v_offset = <Dimension>::undump(lines)?;
        let emergency_stretch = <Dimension>::undump(lines)?;
        let dimens = Dumpable::undump(lines)?;

        Ok(Self {
            par_indent,
            math_surround,
            line_skip_limit,
            hsize,
            vsize,
            max_depth,
            split_max_depth,
            box_max_depth,
            hfuzz,
            vfuzz,
            delimiter_shortfall,
            null_delimiter_space,
            script_space,
            pre_display_size,
            display_width,
            display_indent,
            overfull_rule,
            hang_indent,
            h_offset,
            v_offset,
            emergency_stretch,
            dimens,
        })
    }
}

impl Dumpable for DimensionVariable {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::ParIndent => writeln!(target, "ParIndent")?,
            Self::MathSurround => writeln!(target, "MathSurround")?,
            Self::LineSkipLimit => writeln!(target, "LineSkipLimit")?,
            Self::Hsize => writeln!(target, "Hsize")?,
            Self::Vsize => writeln!(target, "Vsize")?,
            Self::MaxDepth => writeln!(target, "MaxDepth")?,
            Self::SplitMaxDepth => writeln!(target, "SplitMaxDepth")?,
            Self::BoxMaxDepth => writeln!(target, "BoxMaxDepth")?,
            Self::Hfuzz => writeln!(target, "Hfuzz")?,
            Self::Vfuzz => writeln!(target, "Vfuzz")?,
            Self::DelimiterShortfall => writeln!(target, "DelimiterShortfall")?,
            Self::NullDelimiterSpace => writeln!(target, "NullDelimiterSpace")?,
            Self::ScriptSpace => writeln!(target, "ScriptSpace")?,
            Self::PreDisplaySize => writeln!(target, "PreDisplaySize")?,
            Self::DisplayWidth => writeln!(target, "DisplayWidth")?,
            Self::DisplayIndent => writeln!(target, "DisplayIndent")?,
            Self::OverfullRule => writeln!(target, "OverfullRule")?,
            Self::HangIndent => writeln!(target, "HangIndent")?,
            Self::HOffset => writeln!(target, "HOffset")?,
            Self::VOffset => writeln!(target, "VOffset")?,
            Self::EmergencyStretch => writeln!(target, "EmergencyStretch")?,
            Self::Dimen(n) => {
                writeln!(target, "Dimen")?;
                n.dump(target)?;
            }
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "ParIndent" => Ok(Self::ParIndent),
            "MathSurround" => Ok(Self::MathSurround),
            "LineSkipLimit" => Ok(Self::LineSkipLimit),
            "Hsize" => Ok(Self::Hsize),
            "Vsize" => Ok(Self::Vsize),
            "MaxDepth" => Ok(Self::MaxDepth),
            "SplitMaxDepth" => Ok(Self::SplitMaxDepth),
            "BoxMaxDepth" => Ok(Self::BoxMaxDepth),
            "Hfuzz" => Ok(Self::Hfuzz),
            "Vfuzz" => Ok(Self::Vfuzz),
            "DelimiterShortfall" => Ok(Self::DelimiterShortfall),
            "NullDelimiterSpace" => Ok(Self::NullDelimiterSpace),
            "ScriptSpace" => Ok(Self::ScriptSpace),
            "PreDisplaySize" => Ok(Self::PreDisplaySize),
            "DisplayWidth" => Ok(Self::DisplayWidth),
            "DisplayIndent" => Ok(Self::DisplayIndent),
            "OverfullRule" => Ok(Self::OverfullRule),
            "HangIndent" => Ok(Self::HangIndent),
            "HOffset" => Ok(Self::HOffset),
            "VOffset" => Ok(Self::VOffset),
            "EmergencyStretch" => Ok(Self::EmergencyStretch),
            "Dimen" => {
                let n = RegisterIndex::undump(lines)?;
                Ok(Self::Dimen(n))
            }
            _ => Err(FormatError::ParseError),
        }
    }
}
