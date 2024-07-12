use super::{RegisterIndex, CARRIAGE_RETURN};
use crate::format::{Dumpable, FormatError};
use crate::integer::Integer;

use std::io::Write;

/// See 236.
pub struct IntegerParameters {
    pretolerance: Integer,
    tolerance: Integer,
    line_penalty: Integer,
    hyphen_penalty: Integer,
    ex_hyphen_penalty: Integer,
    club_penalty: Integer,
    widow_penalty: Integer,
    display_widow_penalty: Integer,
    broken_penalty: Integer,
    bin_op_penalty: Integer,
    rel_penalty: Integer,
    pre_display_penalty: Integer,
    post_display_penalty: Integer,
    inter_line_penalty: Integer,
    double_hyphen_demerits: Integer,
    final_hyphen_demerits: Integer,
    adj_demerits: Integer,
    mag: Integer,
    delimiter_factor: Integer,
    looseness: Integer,
    time: Integer,
    day: Integer,
    month: Integer,
    year: Integer,
    show_box_breadth: Integer,
    show_box_depth: Integer,
    hbadness: Integer,
    vbadness: Integer,
    pausing: Integer,
    tracing_online: Integer,
    tracing_macros: Integer,
    tracing_stats: Integer,
    tracing_paragraphs: Integer,
    tracing_pages: Integer,
    tracing_output: Integer,
    tracing_lost_chars: Integer,
    tracing_commands: Integer,
    tracing_restores: Integer,
    uc_hyph: Integer,
    output_penalty: Integer,
    max_dead_cycles: Integer,
    hang_after: Integer,
    floating_penalty: Integer,
    global_defs: Integer,
    cur_fam: Integer,
    escape_char: Integer,
    default_hyphen_char: Integer,
    default_skew_char: Integer,
    end_line_char: Integer,
    new_line_char: Integer,
    language: Integer,
    left_hyphen_min: Integer,
    right_hyphen_min: Integer,
    holding_inserts: Integer,
    error_context_lines: Integer,
    counts: Vec<Integer>,
}

impl IntegerParameters {
    /// All integer variables are initiated with zero except for
    /// `Mag`, `Tolerance`, `HangAfter`, `MaxDeadCycles`, `EscapeChar` and
    /// `EndLineChar`.
    /// See 240.
    pub fn new() -> Self {
        Self {
            // Non-zero values.
            mag: 1000,
            tolerance: 10000,
            hang_after: 1,
            max_dead_cycles: 25,
            escape_char: b'\\' as Integer,
            end_line_char: CARRIAGE_RETURN as Integer,

            // The rest is all zeroes.
            pretolerance: 0,
            line_penalty: 0,
            hyphen_penalty: 0,
            ex_hyphen_penalty: 0,
            club_penalty: 0,
            widow_penalty: 0,
            display_widow_penalty: 0,
            broken_penalty: 0,
            bin_op_penalty: 0,
            rel_penalty: 0,
            pre_display_penalty: 0,
            post_display_penalty: 0,
            inter_line_penalty: 0,
            double_hyphen_demerits: 0,
            final_hyphen_demerits: 0,
            adj_demerits: 0,
            delimiter_factor: 0,
            looseness: 0,
            time: 0,
            day: 0,
            month: 0,
            year: 0,
            show_box_breadth: 0,
            show_box_depth: 0,
            hbadness: 0,
            vbadness: 0,
            pausing: 0,
            tracing_online: 0,
            tracing_macros: 0,
            tracing_stats: 0,
            tracing_paragraphs: 0,
            tracing_pages: 0,
            tracing_output: 0,
            tracing_lost_chars: 0,
            tracing_commands: 0,
            tracing_restores: 0,
            uc_hyph: 0,
            output_penalty: 0,
            floating_penalty: 0,
            global_defs: 0,
            cur_fam: 0,
            default_hyphen_char: 0,
            default_skew_char: 0,
            new_line_char: 0,
            language: 0,
            left_hyphen_min: 0,
            right_hyphen_min: 0,
            holding_inserts: 0,
            error_context_lines: 0,
            counts: vec![0; RegisterIndex::MAX as usize + 1],
        }
    }

    fn index(&self, index: IntegerVariable) -> &Integer {
        match index {
            IntegerVariable::Pretolerance => &self.pretolerance,
            IntegerVariable::Tolerance => &self.tolerance,
            IntegerVariable::LinePenalty => &self.line_penalty,
            IntegerVariable::HyphenPenalty => &self.hyphen_penalty,
            IntegerVariable::ExHyphenPenalty => &self.ex_hyphen_penalty,
            IntegerVariable::ClubPenalty => &self.club_penalty,
            IntegerVariable::WidowPenalty => &self.widow_penalty,
            IntegerVariable::DisplayWidowPenalty => &self.display_widow_penalty,
            IntegerVariable::BrokenPenalty => &self.broken_penalty,
            IntegerVariable::BinOpPenalty => &self.bin_op_penalty,
            IntegerVariable::RelPenalty => &self.rel_penalty,
            IntegerVariable::PreDisplayPenalty => &self.pre_display_penalty,
            IntegerVariable::PostDisplayPenalty => &self.post_display_penalty,
            IntegerVariable::InterLinePenalty => &self.inter_line_penalty,
            IntegerVariable::DoubleHyphenDemerits => &self.double_hyphen_demerits,
            IntegerVariable::FinalHyphenDemerits => &self.final_hyphen_demerits,
            IntegerVariable::AdjDemerits => &self.adj_demerits,
            IntegerVariable::Mag => &self.mag,
            IntegerVariable::DelimiterFactor => &self.delimiter_factor,
            IntegerVariable::Looseness => &self.looseness,
            IntegerVariable::Time => &self.time,
            IntegerVariable::Day => &self.day,
            IntegerVariable::Month => &self.month,
            IntegerVariable::Year => &self.year,
            IntegerVariable::ShowBoxBreadth => &self.show_box_breadth,
            IntegerVariable::ShowBoxDepth => &self.show_box_depth,
            IntegerVariable::Hbadness => &self.hbadness,
            IntegerVariable::Vbadness => &self.vbadness,
            IntegerVariable::Pausing => &self.pausing,
            IntegerVariable::TracingOnline => &self.tracing_online,
            IntegerVariable::TracingMacros => &self.tracing_macros,
            IntegerVariable::TracingStats => &self.tracing_stats,
            IntegerVariable::TracingParagraphs => &self.tracing_paragraphs,
            IntegerVariable::TracingPages => &self.tracing_pages,
            IntegerVariable::TracingOutput => &self.tracing_output,
            IntegerVariable::TracingLostChars => &self.tracing_lost_chars,
            IntegerVariable::TracingCommands => &self.tracing_commands,
            IntegerVariable::TracingRestores => &self.tracing_restores,
            IntegerVariable::UcHyph => &self.uc_hyph,
            IntegerVariable::OutputPenalty => &self.output_penalty,
            IntegerVariable::MaxDeadCycles => &self.max_dead_cycles,
            IntegerVariable::HangAfter => &self.hang_after,
            IntegerVariable::FloatingPenalty => &self.floating_penalty,
            IntegerVariable::GlobalDefs => &self.global_defs,
            IntegerVariable::CurFam => &self.cur_fam,
            IntegerVariable::EscapeChar => &self.escape_char,
            IntegerVariable::DefaultHyphenChar => &self.default_hyphen_char,
            IntegerVariable::DefaultSkewChar => &self.default_skew_char,
            IntegerVariable::EndLineChar => &self.end_line_char,
            IntegerVariable::NewLineChar => &self.new_line_char,
            IntegerVariable::Language => &self.language,
            IntegerVariable::LeftHyphenMin => &self.left_hyphen_min,
            IntegerVariable::RightHyphenMin => &self.right_hyphen_min,
            IntegerVariable::HoldingInserts => &self.holding_inserts,
            IntegerVariable::ErrorContextLines => &self.error_context_lines,
            IntegerVariable::Count(register) => &self.counts[register as usize],
        }
    }

    fn index_mut(&mut self, index: IntegerVariable) -> &mut Integer {
        match index {
            IntegerVariable::Pretolerance => &mut self.pretolerance,
            IntegerVariable::Tolerance => &mut self.tolerance,
            IntegerVariable::LinePenalty => &mut self.line_penalty,
            IntegerVariable::HyphenPenalty => &mut self.hyphen_penalty,
            IntegerVariable::ExHyphenPenalty => &mut self.ex_hyphen_penalty,
            IntegerVariable::ClubPenalty => &mut self.club_penalty,
            IntegerVariable::WidowPenalty => &mut self.widow_penalty,
            IntegerVariable::DisplayWidowPenalty => &mut self.display_widow_penalty,
            IntegerVariable::BrokenPenalty => &mut self.broken_penalty,
            IntegerVariable::BinOpPenalty => &mut self.bin_op_penalty,
            IntegerVariable::RelPenalty => &mut self.rel_penalty,
            IntegerVariable::PreDisplayPenalty => &mut self.pre_display_penalty,
            IntegerVariable::PostDisplayPenalty => &mut self.post_display_penalty,
            IntegerVariable::InterLinePenalty => &mut self.inter_line_penalty,
            IntegerVariable::DoubleHyphenDemerits => &mut self.double_hyphen_demerits,
            IntegerVariable::FinalHyphenDemerits => &mut self.final_hyphen_demerits,
            IntegerVariable::AdjDemerits => &mut self.adj_demerits,
            IntegerVariable::Mag => &mut self.mag,
            IntegerVariable::DelimiterFactor => &mut self.delimiter_factor,
            IntegerVariable::Looseness => &mut self.looseness,
            IntegerVariable::Time => &mut self.time,
            IntegerVariable::Day => &mut self.day,
            IntegerVariable::Month => &mut self.month,
            IntegerVariable::Year => &mut self.year,
            IntegerVariable::ShowBoxBreadth => &mut self.show_box_breadth,
            IntegerVariable::ShowBoxDepth => &mut self.show_box_depth,
            IntegerVariable::Hbadness => &mut self.hbadness,
            IntegerVariable::Vbadness => &mut self.vbadness,
            IntegerVariable::Pausing => &mut self.pausing,
            IntegerVariable::TracingOnline => &mut self.tracing_online,
            IntegerVariable::TracingMacros => &mut self.tracing_macros,
            IntegerVariable::TracingStats => &mut self.tracing_stats,
            IntegerVariable::TracingParagraphs => &mut self.tracing_paragraphs,
            IntegerVariable::TracingPages => &mut self.tracing_pages,
            IntegerVariable::TracingOutput => &mut self.tracing_output,
            IntegerVariable::TracingLostChars => &mut self.tracing_lost_chars,
            IntegerVariable::TracingCommands => &mut self.tracing_commands,
            IntegerVariable::TracingRestores => &mut self.tracing_restores,
            IntegerVariable::UcHyph => &mut self.uc_hyph,
            IntegerVariable::OutputPenalty => &mut self.output_penalty,
            IntegerVariable::MaxDeadCycles => &mut self.max_dead_cycles,
            IntegerVariable::HangAfter => &mut self.hang_after,
            IntegerVariable::FloatingPenalty => &mut self.floating_penalty,
            IntegerVariable::GlobalDefs => &mut self.global_defs,
            IntegerVariable::CurFam => &mut self.cur_fam,
            IntegerVariable::EscapeChar => &mut self.escape_char,
            IntegerVariable::DefaultHyphenChar => &mut self.default_hyphen_char,
            IntegerVariable::DefaultSkewChar => &mut self.default_skew_char,
            IntegerVariable::EndLineChar => &mut self.end_line_char,
            IntegerVariable::NewLineChar => &mut self.new_line_char,
            IntegerVariable::Language => &mut self.language,
            IntegerVariable::LeftHyphenMin => &mut self.left_hyphen_min,
            IntegerVariable::RightHyphenMin => &mut self.right_hyphen_min,
            IntegerVariable::HoldingInserts => &mut self.holding_inserts,
            IntegerVariable::ErrorContextLines => &mut self.error_context_lines,
            IntegerVariable::Count(register) => &mut self.counts[register as usize],
        }
    }

    pub fn get(&self, int_var: IntegerVariable) -> &Integer {
        self.index(int_var)
    }

    pub fn set(&mut self, int_var: IntegerVariable, new_value: Integer) -> Integer {
        let prev_value = *self.index_mut(int_var);
        *self.index_mut(int_var) = new_value;
        prev_value
    }
}

/// Specifies an integer variable in the Eqtb.
/// See 236.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntegerVariable {
    Pretolerance,
    Tolerance,
    LinePenalty,
    HyphenPenalty,
    ExHyphenPenalty,
    ClubPenalty,
    WidowPenalty,
    DisplayWidowPenalty,
    BrokenPenalty,
    BinOpPenalty,
    RelPenalty,
    PreDisplayPenalty,
    PostDisplayPenalty,
    InterLinePenalty,
    DoubleHyphenDemerits,
    FinalHyphenDemerits,
    AdjDemerits,
    Mag,
    DelimiterFactor,
    Looseness,
    Time,
    Day,
    Month,
    Year,
    ShowBoxBreadth,
    ShowBoxDepth,
    Hbadness,
    Vbadness,
    Pausing,
    TracingOnline,
    TracingMacros,
    TracingStats,
    TracingParagraphs,
    TracingPages,
    TracingOutput,
    TracingLostChars,
    TracingCommands,
    TracingRestores,
    UcHyph,
    OutputPenalty,
    MaxDeadCycles,
    HangAfter,
    FloatingPenalty,
    GlobalDefs,
    CurFam,
    EscapeChar,
    DefaultHyphenChar,
    DefaultSkewChar,
    EndLineChar,
    NewLineChar,
    Language,
    LeftHyphenMin,
    RightHyphenMin,
    HoldingInserts,
    ErrorContextLines,
    Count(RegisterIndex),
}

impl IntegerVariable {
    /// The name of the integer variable without preceding escape character.
    /// See 237., 239., and 242.
    pub fn to_string(self) -> Vec<u8> {
        match self {
            Self::Pretolerance => b"pretolerance".to_vec(),
            Self::Tolerance => b"tolerance".to_vec(),
            Self::LinePenalty => b"linepenalty".to_vec(),
            Self::HyphenPenalty => b"hyphenpenalty".to_vec(),
            Self::ExHyphenPenalty => b"exhyphenpenalty".to_vec(),
            Self::ClubPenalty => b"clubpenalty".to_vec(),
            Self::WidowPenalty => b"widowpenalty".to_vec(),
            Self::DisplayWidowPenalty => b"displaywidowpenalty".to_vec(),
            Self::BrokenPenalty => b"brokenpenalty".to_vec(),
            Self::BinOpPenalty => b"binoppenalty".to_vec(),
            Self::RelPenalty => b"relpenalty".to_vec(),
            Self::PreDisplayPenalty => b"predisplaypenalty".to_vec(),
            Self::PostDisplayPenalty => b"postdisplaypenalty".to_vec(),
            Self::InterLinePenalty => b"interlinepenalty".to_vec(),
            Self::DoubleHyphenDemerits => b"doublehyphendemerits".to_vec(),
            Self::FinalHyphenDemerits => b"finalhyphendemerits".to_vec(),
            Self::AdjDemerits => b"adjdemerits".to_vec(),
            Self::Mag => b"mag".to_vec(),
            Self::DelimiterFactor => b"delimiterfactor".to_vec(),
            Self::Looseness => b"looseness".to_vec(),
            Self::Time => b"time".to_vec(),
            Self::Day => b"day".to_vec(),
            Self::Month => b"month".to_vec(),
            Self::Year => b"year".to_vec(),
            Self::ShowBoxBreadth => b"showboxbreadth".to_vec(),
            Self::ShowBoxDepth => b"showboxdepth".to_vec(),
            Self::Hbadness => b"hbadness".to_vec(),
            Self::Vbadness => b"vbadness".to_vec(),
            Self::Pausing => b"pausing".to_vec(),
            Self::TracingOnline => b"tracingonline".to_vec(),
            Self::TracingMacros => b"tracingmacros".to_vec(),
            Self::TracingStats => b"tracingstats".to_vec(),
            Self::TracingParagraphs => b"tracingparagraphs".to_vec(),
            Self::TracingPages => b"tracingpages".to_vec(),
            Self::TracingOutput => b"tracingoutput".to_vec(),
            Self::TracingLostChars => b"tracinglostchars".to_vec(),
            Self::TracingCommands => b"tracingcommands".to_vec(),
            Self::TracingRestores => b"tracingrestores".to_vec(),
            Self::UcHyph => b"uchyph".to_vec(),
            Self::OutputPenalty => b"outputpenalty".to_vec(),
            Self::MaxDeadCycles => b"maxdeadcycles".to_vec(),
            Self::HangAfter => b"hangafter".to_vec(),
            Self::FloatingPenalty => b"floatingpenalty".to_vec(),
            Self::GlobalDefs => b"globaldefs".to_vec(),
            Self::CurFam => b"fam".to_vec(),
            Self::EscapeChar => b"escapechar".to_vec(),
            Self::DefaultHyphenChar => b"defaulthyphenchar".to_vec(),
            Self::DefaultSkewChar => b"defaultskewchar".to_vec(),
            Self::EndLineChar => b"endlinechar".to_vec(),
            Self::NewLineChar => b"newlinechar".to_vec(),
            Self::Language => b"language".to_vec(),
            Self::LeftHyphenMin => b"lefthyphenmin".to_vec(),
            Self::RightHyphenMin => b"righthyphenmin".to_vec(),
            Self::HoldingInserts => b"holdinginserts".to_vec(),
            Self::ErrorContextLines => b"errorcontextlines".to_vec(),
            Self::Count(register) => format!("count{}", register).as_bytes().to_vec(),
        }
    }
}

impl Dumpable for IntegerParameters {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.pretolerance.dump(target)?;
        self.tolerance.dump(target)?;
        self.line_penalty.dump(target)?;
        self.hyphen_penalty.dump(target)?;
        self.ex_hyphen_penalty.dump(target)?;
        self.club_penalty.dump(target)?;
        self.widow_penalty.dump(target)?;
        self.display_widow_penalty.dump(target)?;
        self.broken_penalty.dump(target)?;
        self.bin_op_penalty.dump(target)?;
        self.rel_penalty.dump(target)?;
        self.pre_display_penalty.dump(target)?;
        self.post_display_penalty.dump(target)?;
        self.inter_line_penalty.dump(target)?;
        self.double_hyphen_demerits.dump(target)?;
        self.final_hyphen_demerits.dump(target)?;
        self.adj_demerits.dump(target)?;
        self.mag.dump(target)?;
        self.delimiter_factor.dump(target)?;
        self.looseness.dump(target)?;
        self.time.dump(target)?;
        self.day.dump(target)?;
        self.month.dump(target)?;
        self.year.dump(target)?;
        self.show_box_breadth.dump(target)?;
        self.show_box_depth.dump(target)?;
        self.hbadness.dump(target)?;
        self.vbadness.dump(target)?;
        self.pausing.dump(target)?;
        self.tracing_online.dump(target)?;
        self.tracing_macros.dump(target)?;
        self.tracing_stats.dump(target)?;
        self.tracing_paragraphs.dump(target)?;
        self.tracing_pages.dump(target)?;
        self.tracing_output.dump(target)?;
        self.tracing_lost_chars.dump(target)?;
        self.tracing_commands.dump(target)?;
        self.tracing_restores.dump(target)?;
        self.uc_hyph.dump(target)?;
        self.output_penalty.dump(target)?;
        self.max_dead_cycles.dump(target)?;
        self.hang_after.dump(target)?;
        self.floating_penalty.dump(target)?;
        self.global_defs.dump(target)?;
        self.cur_fam.dump(target)?;
        self.escape_char.dump(target)?;
        self.default_hyphen_char.dump(target)?;
        self.default_skew_char.dump(target)?;
        self.end_line_char.dump(target)?;
        self.new_line_char.dump(target)?;
        self.language.dump(target)?;
        self.left_hyphen_min.dump(target)?;
        self.right_hyphen_min.dump(target)?;
        self.holding_inserts.dump(target)?;
        self.error_context_lines.dump(target)?;
        self.counts.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let pretolerance = <Integer>::undump(lines)?;
        let tolerance = <Integer>::undump(lines)?;
        let line_penalty = <Integer>::undump(lines)?;
        let hyphen_penalty = <Integer>::undump(lines)?;
        let ex_hyphen_penalty = <Integer>::undump(lines)?;
        let club_penalty = <Integer>::undump(lines)?;
        let widow_penalty = <Integer>::undump(lines)?;
        let display_widow_penalty = <Integer>::undump(lines)?;
        let broken_penalty = <Integer>::undump(lines)?;
        let bin_op_penalty = <Integer>::undump(lines)?;
        let rel_penalty = <Integer>::undump(lines)?;
        let pre_display_penalty = <Integer>::undump(lines)?;
        let post_display_penalty = <Integer>::undump(lines)?;
        let inter_line_penalty = <Integer>::undump(lines)?;
        let double_hyphen_demerits = <Integer>::undump(lines)?;
        let final_hyphen_demerits = <Integer>::undump(lines)?;
        let adj_demerits = <Integer>::undump(lines)?;
        let mag = <Integer>::undump(lines)?;
        let delimiter_factor = <Integer>::undump(lines)?;
        let looseness = <Integer>::undump(lines)?;
        let time = <Integer>::undump(lines)?;
        let day = <Integer>::undump(lines)?;
        let month = <Integer>::undump(lines)?;
        let year = <Integer>::undump(lines)?;
        let show_box_breadth = <Integer>::undump(lines)?;
        let show_box_depth = <Integer>::undump(lines)?;
        let hbadness = <Integer>::undump(lines)?;
        let vbadness = <Integer>::undump(lines)?;
        let pausing = <Integer>::undump(lines)?;
        let tracing_online = <Integer>::undump(lines)?;
        let tracing_macros = <Integer>::undump(lines)?;
        let tracing_stats = <Integer>::undump(lines)?;
        let tracing_paragraphs = <Integer>::undump(lines)?;
        let tracing_pages = <Integer>::undump(lines)?;
        let tracing_output = <Integer>::undump(lines)?;
        let tracing_lost_chars = <Integer>::undump(lines)?;
        let tracing_commands = <Integer>::undump(lines)?;
        let tracing_restores = <Integer>::undump(lines)?;
        let uc_hyph = <Integer>::undump(lines)?;
        let output_penalty = <Integer>::undump(lines)?;
        let max_dead_cycles = <Integer>::undump(lines)?;
        let hang_after = <Integer>::undump(lines)?;
        let floating_penalty = <Integer>::undump(lines)?;
        let global_defs = <Integer>::undump(lines)?;
        let cur_fam = <Integer>::undump(lines)?;
        let escape_char = <Integer>::undump(lines)?;
        let default_hyphen_char = <Integer>::undump(lines)?;
        let default_skew_char = <Integer>::undump(lines)?;
        let end_line_char = <Integer>::undump(lines)?;
        let new_line_char = <Integer>::undump(lines)?;
        let language = <Integer>::undump(lines)?;
        let left_hyphen_min = <Integer>::undump(lines)?;
        let right_hyphen_min = <Integer>::undump(lines)?;
        let holding_inserts = <Integer>::undump(lines)?;
        let error_context_lines = <Integer>::undump(lines)?;

        let counts = Dumpable::undump(lines)?;
        Ok(Self {
            pretolerance,
            tolerance,
            line_penalty,
            hyphen_penalty,
            ex_hyphen_penalty,
            club_penalty,
            widow_penalty,
            display_widow_penalty,
            broken_penalty,
            bin_op_penalty,
            rel_penalty,
            pre_display_penalty,
            post_display_penalty,
            inter_line_penalty,
            double_hyphen_demerits,
            final_hyphen_demerits,
            adj_demerits,
            mag,
            delimiter_factor,
            looseness,
            time,
            day,
            month,
            year,
            show_box_breadth,
            show_box_depth,
            hbadness,
            vbadness,
            pausing,
            tracing_online,
            tracing_macros,
            tracing_stats,
            tracing_paragraphs,
            tracing_pages,
            tracing_output,
            tracing_lost_chars,
            tracing_commands,
            tracing_restores,
            uc_hyph,
            output_penalty,
            max_dead_cycles,
            hang_after,
            floating_penalty,
            global_defs,
            cur_fam,
            escape_char,
            default_hyphen_char,
            default_skew_char,
            end_line_char,
            new_line_char,
            language,
            left_hyphen_min,
            right_hyphen_min,
            holding_inserts,
            error_context_lines,
            counts,
        })
    }
}

impl Dumpable for IntegerVariable {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::Pretolerance => writeln!(target, "Pretolerance")?,
            Self::Tolerance => writeln!(target, "Tolerance")?,
            Self::LinePenalty => writeln!(target, "LinePenalty")?,
            Self::HyphenPenalty => writeln!(target, "HyphenPenalty")?,
            Self::ExHyphenPenalty => writeln!(target, "ExHyphenPenalty")?,
            Self::ClubPenalty => writeln!(target, "ClubPenalty")?,
            Self::WidowPenalty => writeln!(target, "WidowPenalty")?,
            Self::DisplayWidowPenalty => writeln!(target, "DisplayWidowPenalty")?,
            Self::BrokenPenalty => writeln!(target, "BrokenPenalty")?,
            Self::BinOpPenalty => writeln!(target, "BinOpPenalty")?,
            Self::RelPenalty => writeln!(target, "RelPenalty")?,
            Self::PreDisplayPenalty => writeln!(target, "PreDisplayPenalty")?,
            Self::PostDisplayPenalty => writeln!(target, "PostDisplayPenalty")?,
            Self::InterLinePenalty => writeln!(target, "InterLinePenalty")?,
            Self::DoubleHyphenDemerits => writeln!(target, "DoubleHyphenDemerits")?,
            Self::FinalHyphenDemerits => writeln!(target, "FinalHyphenDemerits")?,
            Self::AdjDemerits => writeln!(target, "AdjDemerits")?,
            Self::Mag => writeln!(target, "Mag")?,
            Self::DelimiterFactor => writeln!(target, "DelimiterFactor")?,
            Self::Looseness => writeln!(target, "Looseness")?,
            Self::Time => writeln!(target, "Time")?,
            Self::Day => writeln!(target, "Day")?,
            Self::Month => writeln!(target, "Month")?,
            Self::Year => writeln!(target, "Year")?,
            Self::ShowBoxBreadth => writeln!(target, "ShowBoxBreadth")?,
            Self::ShowBoxDepth => writeln!(target, "ShowBoxDepth")?,
            Self::Hbadness => writeln!(target, "Hbadness")?,
            Self::Vbadness => writeln!(target, "Vbadness")?,
            Self::Pausing => writeln!(target, "Pausing")?,
            Self::TracingOnline => writeln!(target, "TracingOnline")?,
            Self::TracingMacros => writeln!(target, "TracingMacros")?,
            Self::TracingStats => writeln!(target, "TracingStats")?,
            Self::TracingParagraphs => writeln!(target, "TracingParagraphs")?,
            Self::TracingPages => writeln!(target, "TracingPages")?,
            Self::TracingOutput => writeln!(target, "TracingOutput")?,
            Self::TracingLostChars => writeln!(target, "TracingLostChars")?,
            Self::TracingCommands => writeln!(target, "TracingCommands")?,
            Self::TracingRestores => writeln!(target, "TracingRestores")?,
            Self::UcHyph => writeln!(target, "UcHyph")?,
            Self::OutputPenalty => writeln!(target, "OutputPenalty")?,
            Self::MaxDeadCycles => writeln!(target, "MaxDeadCycles")?,
            Self::HangAfter => writeln!(target, "HangAfter")?,
            Self::FloatingPenalty => writeln!(target, "FloatingPenalty")?,
            Self::GlobalDefs => writeln!(target, "GlobalDefs")?,
            Self::CurFam => writeln!(target, "CurFam")?,
            Self::EscapeChar => writeln!(target, "EscapeChar")?,
            Self::DefaultHyphenChar => writeln!(target, "DefaultHyphenChar")?,
            Self::DefaultSkewChar => writeln!(target, "DefaultSkewChar")?,
            Self::EndLineChar => writeln!(target, "EndLineChar")?,
            Self::NewLineChar => writeln!(target, "NewLineChar")?,
            Self::Language => writeln!(target, "Language")?,
            Self::LeftHyphenMin => writeln!(target, "LeftHyphenMin")?,
            Self::RightHyphenMin => writeln!(target, "RightHyphenMin")?,
            Self::HoldingInserts => writeln!(target, "HoldingInserts")?,
            Self::ErrorContextLines => writeln!(target, "ErrorContextLines")?,
            Self::Count(n) => {
                writeln!(target, "Count")?;
                n.dump(target)?;
            }
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "Pretolerance" => Ok(IntegerVariable::Pretolerance),
            "Tolerance" => Ok(IntegerVariable::Tolerance),
            "LinePenalty" => Ok(IntegerVariable::LinePenalty),
            "HyphenPenalty" => Ok(IntegerVariable::HyphenPenalty),
            "ExHyphenPenalty" => Ok(IntegerVariable::ExHyphenPenalty),
            "ClubPenalty" => Ok(IntegerVariable::ClubPenalty),
            "WidowPenalty" => Ok(IntegerVariable::WidowPenalty),
            "DisplayWidowPenalty" => Ok(IntegerVariable::DisplayWidowPenalty),
            "BrokenPenalty" => Ok(IntegerVariable::BrokenPenalty),
            "BinOpPenalty" => Ok(IntegerVariable::BinOpPenalty),
            "RelPenalty" => Ok(IntegerVariable::RelPenalty),
            "PreDisplayPenalty" => Ok(IntegerVariable::PreDisplayPenalty),
            "PostDisplayPenalty" => Ok(IntegerVariable::PostDisplayPenalty),
            "InterLinePenalty" => Ok(IntegerVariable::InterLinePenalty),
            "DoubleHyphenDemerits" => Ok(IntegerVariable::DoubleHyphenDemerits),
            "FinalHyphenDemerits" => Ok(IntegerVariable::FinalHyphenDemerits),
            "AdjDemerits" => Ok(IntegerVariable::AdjDemerits),
            "Mag" => Ok(IntegerVariable::Mag),
            "DelimiterFactor" => Ok(IntegerVariable::DelimiterFactor),
            "Looseness" => Ok(IntegerVariable::Looseness),
            "Time" => Ok(IntegerVariable::Time),
            "Day" => Ok(IntegerVariable::Day),
            "Month" => Ok(IntegerVariable::Month),
            "Year" => Ok(IntegerVariable::Year),
            "ShowBoxBreadth" => Ok(IntegerVariable::ShowBoxBreadth),
            "ShowBoxDepth" => Ok(IntegerVariable::ShowBoxDepth),
            "Hbadness" => Ok(IntegerVariable::Hbadness),
            "Vbadness" => Ok(IntegerVariable::Vbadness),
            "Pausing" => Ok(IntegerVariable::Pausing),
            "TracingOnline" => Ok(IntegerVariable::TracingOnline),
            "TracingMacros" => Ok(IntegerVariable::TracingMacros),
            "TracingStats" => Ok(IntegerVariable::TracingStats),
            "TracingParagraphs" => Ok(IntegerVariable::TracingParagraphs),
            "TracingPages" => Ok(IntegerVariable::TracingPages),
            "TracingOutput" => Ok(IntegerVariable::TracingOutput),
            "TracingLostChars" => Ok(IntegerVariable::TracingLostChars),
            "TracingCommands" => Ok(IntegerVariable::TracingCommands),
            "TracingRestores" => Ok(IntegerVariable::TracingRestores),
            "UcHyph" => Ok(IntegerVariable::UcHyph),
            "OutputPenalty" => Ok(IntegerVariable::OutputPenalty),
            "MaxDeadCycles" => Ok(IntegerVariable::MaxDeadCycles),
            "HangAfter" => Ok(IntegerVariable::HangAfter),
            "FloatingPenalty" => Ok(IntegerVariable::FloatingPenalty),
            "GlobalDefs" => Ok(IntegerVariable::GlobalDefs),
            "CurFam" => Ok(IntegerVariable::CurFam),
            "EscapeChar" => Ok(IntegerVariable::EscapeChar),
            "DefaultHyphenChar" => Ok(IntegerVariable::DefaultHyphenChar),
            "DefaultSkewChar" => Ok(IntegerVariable::DefaultSkewChar),
            "EndLineChar" => Ok(IntegerVariable::EndLineChar),
            "NewLineChar" => Ok(IntegerVariable::NewLineChar),
            "Language" => Ok(IntegerVariable::Language),
            "LeftHyphenMin" => Ok(IntegerVariable::LeftHyphenMin),
            "RightHyphenMin" => Ok(IntegerVariable::RightHyphenMin),
            "HoldingInserts" => Ok(IntegerVariable::HoldingInserts),
            "ErrorContextLines" => Ok(IntegerVariable::ErrorContextLines),
            "Count" => {
                let n = RegisterIndex::undump(lines)?;
                Ok(IntegerVariable::Count(n))
            }
            _ => Err(FormatError::ParseError),
        }
    }
}
