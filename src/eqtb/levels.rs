use super::{
    BoxVariable, CodeVariable, ControlSequence, DimensionVariable, FontIndex, FontVariable,
    IntegerVariable, MathFontSize, RegisterIndex, SkipVariable, TokenListVariable, Variable,
};
use crate::format::{Dumpable, FormatError};

use std::io::Write;

pub type Level = usize;

#[derive(Debug)]
pub struct VariableLevels {
    // Boxes
    boxes: Vec<Level>,

    // Category codes
    cat_code: [Level; 256],

    // Codes
    lc_code: [Level; 256],
    uc_code: [Level; 256],
    sf_code: [Level; 256],
    math_code: [Level; 256],
    del_code: [Level; 256],

    // Control sequences
    active: [Level; 256],
    single: [Level; 256],
    null_cs: Level,

    escaped: Vec<Level>,

    frozen_protection: Level,
    frozen_cr: Level,
    frozen_end_group: Level,
    frozen_right: Level,
    frozen_fi: Level,
    frozen_end_template: Level,
    frozen_endv: Level,
    frozen_relax: Level,
    end_write: Level,
    font_id: [Level; FontIndex::MAX as usize + 1],
    undefined: Level,

    // Dimensions
    par_indent: Level,
    math_surround: Level,
    line_skip_limit: Level,
    hsize: Level,
    vsize: Level,
    max_depth: Level,
    split_max_depth: Level,
    box_max_depth: Level,
    hfuzz: Level,
    vfuzz: Level,
    delimiter_shortfall: Level,
    null_delimiter_space: Level,
    script_space: Level,
    pre_display_size: Level,
    display_width: Level,
    display_indent: Level,
    overfull_rule: Level,
    hang_indent: Level,
    h_offset: Level,
    v_offset: Level,
    emergency_stretch: Level,
    dimen: Vec<Level>,

    // Fonts
    cur_font: Level,
    text_font: [Level; 16],
    script_font: [Level; 16],
    script_script_font: [Level; 16],

    // Integers
    pretolerance: Level,
    tolerance: Level,
    line_penalty: Level,
    hyphen_penalty: Level,
    ex_hyphen_penalty: Level,
    club_penalty: Level,
    widow_penalty: Level,
    display_widow_penalty: Level,
    broken_penalty: Level,
    bin_op_penalty: Level,
    rel_penalty: Level,
    pre_display_penalty: Level,
    post_display_penalty: Level,
    inter_line_penalty: Level,
    double_hyphen_demerits: Level,
    final_hyphen_demerits: Level,
    adj_demerits: Level,
    mag: Level,
    delimiter_factor: Level,
    looseness: Level,
    time: Level,
    day: Level,
    month: Level,
    year: Level,
    show_box_breadth: Level,
    show_box_depth: Level,
    hbadness: Level,
    vbadness: Level,
    pausing: Level,
    tracing_online: Level,
    tracing_macros: Level,
    tracing_stats: Level,
    tracing_paragraphs: Level,
    tracing_pages: Level,
    tracing_output: Level,
    tracing_lost_chars: Level,
    tracing_commands: Level,
    tracing_restores: Level,
    uc_hyph: Level,
    output_penalty: Level,
    max_dead_cycles: Level,
    hang_after: Level,
    floating_penalty: Level,
    global_defs: Level,
    cur_fam: Level,
    escape_char: Level,
    default_hyphen_char: Level,
    default_skew_char: Level,
    end_line_char: Level,
    new_line_char: Level,
    language: Level,
    left_hyphen_min: Level,
    right_hyphen_min: Level,
    holding_inserts: Level,
    error_context_lines: Level,
    count: Vec<Level>,

    // Parshape
    par_shape: Level,

    // Skips
    line_skip: Level,
    baseline_skip: Level,
    par_skip: Level,
    above_display_skip: Level,
    below_display_skip: Level,
    above_display_short_skip: Level,
    below_display_short_skip: Level,
    left_skip: Level,
    right_skip: Level,
    top_skip: Level,
    split_top_skip: Level,
    tab_skip: Level,
    space_skip: Level,
    xspace_skip: Level,
    par_fill_skip: Level,
    thin_mu_skip: Level,
    med_mu_skip: Level,
    thick_mu_skip: Level,
    skip: Vec<Level>,
    mu_skip: Vec<Level>,

    // Token lists
    output_routine: Level,
    every_par: Level,
    every_math: Level,
    every_display: Level,
    every_hbox: Level,
    every_vbox: Level,
    every_job: Level,
    every_cr: Level,
    err_help: Level,
    toks: Vec<Level>,
}

impl VariableLevels {
    pub fn new() -> Self {
        Self {
            // Boxes
            boxes: vec![0; RegisterIndex::MAX as usize + 1],

            // Codes
            cat_code: [0; 256],
            lc_code: [0; 256],
            uc_code: [0; 256],
            sf_code: [0; 256],
            math_code: [0; 256],
            del_code: [0; 256],

            // Control sequences
            active: [0; 256],
            single: [0; 256],
            null_cs: 0,

            escaped: Vec::new(),

            frozen_protection: 0,
            frozen_cr: 0,
            frozen_end_group: 0,
            frozen_right: 0,
            frozen_fi: 0,
            frozen_end_template: 0,
            frozen_endv: 0,
            frozen_relax: 0,
            end_write: 0,
            font_id: [0; FontIndex::MAX as usize + 1],
            undefined: 0,

            // Dimensions
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
            dimen: vec![0; RegisterIndex::MAX as usize + 1],

            // Fonts
            cur_font: 0,
            text_font: [0; 16],
            script_font: [0; 16],
            script_script_font: [0; 16],

            // Integers
            pretolerance: 0,
            tolerance: 0,
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
            mag: 0,
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
            max_dead_cycles: 0,
            hang_after: 0,
            floating_penalty: 0,
            global_defs: 0,
            cur_fam: 0,
            escape_char: 0,
            default_hyphen_char: 0,
            default_skew_char: 0,
            end_line_char: 0,
            new_line_char: 0,
            language: 0,
            left_hyphen_min: 0,
            right_hyphen_min: 0,
            holding_inserts: 0,
            error_context_lines: 0,
            count: vec![0; RegisterIndex::MAX as usize + 1],

            // Parshape
            par_shape: 0,

            // Skips
            line_skip: 0,
            baseline_skip: 0,
            par_skip: 0,
            above_display_skip: 0,
            below_display_skip: 0,
            above_display_short_skip: 0,
            below_display_short_skip: 0,
            left_skip: 0,
            right_skip: 0,
            top_skip: 0,
            split_top_skip: 0,
            tab_skip: 0,
            space_skip: 0,
            xspace_skip: 0,
            par_fill_skip: 0,
            thin_mu_skip: 0,
            med_mu_skip: 0,
            thick_mu_skip: 0,
            skip: vec![0; RegisterIndex::MAX as usize + 1],
            mu_skip: vec![0; RegisterIndex::MAX as usize + 1],

            // Token lists
            output_routine: 0,
            every_par: 0,
            every_math: 0,
            every_display: 0,
            every_hbox: 0,
            every_vbox: 0,
            every_job: 0,
            every_cr: 0,
            err_help: 0,
            toks: vec![0; RegisterIndex::MAX as usize + 1],
        }
    }

    /// Adds an entry for a new escaped command and sets it to the global level.
    pub fn add_new_escaped_command(&mut self) {
        self.escaped.push(0);
    }

    pub fn get(&self, variable: Variable) -> usize {
        match variable {
            Variable::BoxRegister(BoxVariable(n)) => self.boxes[n as usize],
            Variable::CatCode(chr) => self.cat_code[chr as usize],
            Variable::Code(code_variable) => match code_variable {
                CodeVariable::LcCode(n) => self.lc_code[n as usize],
                CodeVariable::UcCode(n) => self.uc_code[n as usize],
                CodeVariable::SfCode(n) => self.sf_code[n as usize],
                CodeVariable::MathCode(n) => self.math_code[n as usize],
                CodeVariable::DelCode(n) => self.del_code[n as usize],
            },
            Variable::ControlSequence(control_sequence) => match control_sequence {
                ControlSequence::Active(c) => self.active[c as usize],
                ControlSequence::Single(c) => self.single[c as usize],
                ControlSequence::NullCs => self.null_cs,
                ControlSequence::Escaped(control_sequence_id) => {
                    self.escaped[control_sequence_id as usize]
                }
                ControlSequence::FrozenProtection => self.frozen_protection,
                ControlSequence::FrozenCr => self.frozen_cr,
                ControlSequence::FrozenEndGroup => self.frozen_end_group,
                ControlSequence::FrozenRight => self.frozen_right,
                ControlSequence::FrozenFi => self.frozen_fi,
                ControlSequence::FrozenEndTemplate => self.frozen_end_template,
                ControlSequence::FrozenEndv => self.frozen_endv,
                ControlSequence::FrozenRelax => self.frozen_relax,
                ControlSequence::EndWrite => self.end_write,
                ControlSequence::FontId(font_index) => self.font_id[font_index as usize],
                ControlSequence::Undefined => self.undefined,
            },
            Variable::Dimen(dimension_variable) => match dimension_variable {
                DimensionVariable::ParIndent => self.par_indent,
                DimensionVariable::MathSurround => self.math_surround,
                DimensionVariable::LineSkipLimit => self.line_skip_limit,
                DimensionVariable::Hsize => self.hsize,
                DimensionVariable::Vsize => self.vsize,
                DimensionVariable::MaxDepth => self.max_depth,
                DimensionVariable::SplitMaxDepth => self.split_max_depth,
                DimensionVariable::BoxMaxDepth => self.box_max_depth,
                DimensionVariable::Hfuzz => self.hfuzz,
                DimensionVariable::Vfuzz => self.vfuzz,
                DimensionVariable::DelimiterShortfall => self.delimiter_shortfall,
                DimensionVariable::NullDelimiterSpace => self.null_delimiter_space,
                DimensionVariable::ScriptSpace => self.script_space,
                DimensionVariable::PreDisplaySize => self.pre_display_size,
                DimensionVariable::DisplayWidth => self.display_width,
                DimensionVariable::DisplayIndent => self.display_indent,
                DimensionVariable::OverfullRule => self.overfull_rule,
                DimensionVariable::HangIndent => self.hang_indent,
                DimensionVariable::HOffset => self.h_offset,
                DimensionVariable::VOffset => self.v_offset,
                DimensionVariable::EmergencyStretch => self.emergency_stretch,
                DimensionVariable::Dimen(register_index) => self.dimen[register_index as usize],
            },
            Variable::Font(font_variable) => match font_variable {
                FontVariable::CurFont => self.cur_font,
                FontVariable::MathFont { size, number } => match size {
                    MathFontSize::Text => self.text_font[number],
                    MathFontSize::Script => self.script_font[number],
                    MathFontSize::ScriptScript => self.script_script_font[number],
                },
            },
            Variable::Integer(integer_variable) => match integer_variable {
                IntegerVariable::Pretolerance => self.pretolerance,
                IntegerVariable::Tolerance => self.tolerance,
                IntegerVariable::LinePenalty => self.line_penalty,
                IntegerVariable::HyphenPenalty => self.hyphen_penalty,
                IntegerVariable::ExHyphenPenalty => self.ex_hyphen_penalty,
                IntegerVariable::ClubPenalty => self.club_penalty,
                IntegerVariable::WidowPenalty => self.widow_penalty,
                IntegerVariable::DisplayWidowPenalty => self.display_widow_penalty,
                IntegerVariable::BrokenPenalty => self.broken_penalty,
                IntegerVariable::BinOpPenalty => self.bin_op_penalty,
                IntegerVariable::RelPenalty => self.rel_penalty,
                IntegerVariable::PreDisplayPenalty => self.pre_display_penalty,
                IntegerVariable::PostDisplayPenalty => self.post_display_penalty,
                IntegerVariable::InterLinePenalty => self.inter_line_penalty,
                IntegerVariable::DoubleHyphenDemerits => self.double_hyphen_demerits,
                IntegerVariable::FinalHyphenDemerits => self.final_hyphen_demerits,
                IntegerVariable::AdjDemerits => self.adj_demerits,
                IntegerVariable::Mag => self.mag,
                IntegerVariable::DelimiterFactor => self.delimiter_factor,
                IntegerVariable::Looseness => self.looseness,
                IntegerVariable::Time => self.time,
                IntegerVariable::Day => self.day,
                IntegerVariable::Month => self.month,
                IntegerVariable::Year => self.year,
                IntegerVariable::ShowBoxBreadth => self.show_box_breadth,
                IntegerVariable::ShowBoxDepth => self.show_box_depth,
                IntegerVariable::Hbadness => self.hbadness,
                IntegerVariable::Vbadness => self.vbadness,
                IntegerVariable::Pausing => self.pausing,
                IntegerVariable::TracingOnline => self.tracing_online,
                IntegerVariable::TracingMacros => self.tracing_macros,
                IntegerVariable::TracingStats => self.tracing_stats,
                IntegerVariable::TracingParagraphs => self.tracing_paragraphs,
                IntegerVariable::TracingPages => self.tracing_pages,
                IntegerVariable::TracingOutput => self.tracing_output,
                IntegerVariable::TracingLostChars => self.tracing_lost_chars,
                IntegerVariable::TracingCommands => self.tracing_commands,
                IntegerVariable::TracingRestores => self.tracing_restores,
                IntegerVariable::UcHyph => self.uc_hyph,
                IntegerVariable::OutputPenalty => self.output_penalty,
                IntegerVariable::MaxDeadCycles => self.max_dead_cycles,
                IntegerVariable::HangAfter => self.hang_after,
                IntegerVariable::FloatingPenalty => self.floating_penalty,
                IntegerVariable::GlobalDefs => self.global_defs,
                IntegerVariable::CurFam => self.cur_fam,
                IntegerVariable::EscapeChar => self.escape_char,
                IntegerVariable::DefaultHyphenChar => self.default_hyphen_char,
                IntegerVariable::DefaultSkewChar => self.default_skew_char,
                IntegerVariable::EndLineChar => self.end_line_char,
                IntegerVariable::NewLineChar => self.new_line_char,
                IntegerVariable::Language => self.language,
                IntegerVariable::LeftHyphenMin => self.left_hyphen_min,
                IntegerVariable::RightHyphenMin => self.right_hyphen_min,
                IntegerVariable::HoldingInserts => self.holding_inserts,
                IntegerVariable::ErrorContextLines => self.error_context_lines,
                IntegerVariable::Count(register_index) => self.count[register_index as usize],
            },
            Variable::ParShape => self.par_shape,
            Variable::Skip(skip_variable) => match skip_variable {
                SkipVariable::LineSkip => self.line_skip,
                SkipVariable::BaselineSkip => self.baseline_skip,
                SkipVariable::ParSkip => self.par_skip,
                SkipVariable::AboveDisplaySkip => self.above_display_skip,
                SkipVariable::BelowDisplaySkip => self.below_display_skip,
                SkipVariable::AboveDisplayShortSkip => self.above_display_short_skip,
                SkipVariable::BelowDisplayShortSkip => self.below_display_short_skip,
                SkipVariable::LeftSkip => self.left_skip,
                SkipVariable::RightSkip => self.right_skip,
                SkipVariable::TopSkip => self.top_skip,
                SkipVariable::SplitTopSkip => self.split_top_skip,
                SkipVariable::TabSkip => self.tab_skip,
                SkipVariable::SpaceSkip => self.space_skip,
                SkipVariable::XspaceSkip => self.xspace_skip,
                SkipVariable::ParFillSkip => self.par_fill_skip,
                SkipVariable::ThinMuSkip => self.thin_mu_skip,
                SkipVariable::MedMuSkip => self.med_mu_skip,
                SkipVariable::ThickMuSkip => self.thick_mu_skip,
                SkipVariable::Skip(register_index) => self.skip[register_index as usize],
                SkipVariable::MuSkip(register_index) => self.mu_skip[register_index as usize],
            },
            Variable::TokenList(token_list_variable) => match token_list_variable {
                TokenListVariable::OutputRoutine => self.output_routine,
                TokenListVariable::EveryPar => self.every_par,
                TokenListVariable::EveryMath => self.every_math,
                TokenListVariable::EveryDisplay => self.every_display,
                TokenListVariable::EveryHbox => self.every_hbox,
                TokenListVariable::EveryVbox => self.every_vbox,
                TokenListVariable::EveryJob => self.every_job,
                TokenListVariable::EveryCr => self.every_cr,
                TokenListVariable::ErrHelp => self.err_help,
                TokenListVariable::Toks(register_index) => self.toks[register_index as usize],
            },
        }
    }

    pub fn set(&mut self, variable: Variable, new_level: usize) -> usize {
        let target = match variable {
            Variable::BoxRegister(BoxVariable(n)) => &mut self.boxes[n as usize],
            Variable::CatCode(chr) => &mut self.cat_code[chr as usize],
            Variable::Code(code_variable) => match code_variable {
                CodeVariable::LcCode(n) => &mut self.lc_code[n as usize],
                CodeVariable::UcCode(n) => &mut self.uc_code[n as usize],
                CodeVariable::SfCode(n) => &mut self.sf_code[n as usize],
                CodeVariable::MathCode(n) => &mut self.math_code[n as usize],
                CodeVariable::DelCode(n) => &mut self.del_code[n as usize],
            },
            Variable::ControlSequence(control_sequence) => match control_sequence {
                ControlSequence::Active(c) => &mut self.active[c as usize],
                ControlSequence::Single(c) => &mut self.single[c as usize],
                ControlSequence::NullCs => &mut self.null_cs,
                ControlSequence::Escaped(control_sequence_id) => {
                    &mut self.escaped[control_sequence_id as usize]
                }
                ControlSequence::FrozenProtection => &mut self.frozen_protection,
                ControlSequence::FrozenCr => &mut self.frozen_cr,
                ControlSequence::FrozenEndGroup => &mut self.frozen_end_group,
                ControlSequence::FrozenRight => &mut self.frozen_right,
                ControlSequence::FrozenFi => &mut self.frozen_fi,
                ControlSequence::FrozenEndTemplate => &mut self.frozen_end_template,
                ControlSequence::FrozenEndv => &mut self.frozen_endv,
                ControlSequence::FrozenRelax => &mut self.frozen_relax,
                ControlSequence::EndWrite => &mut self.end_write,
                ControlSequence::FontId(font_index) => &mut self.font_id[font_index as usize],
                ControlSequence::Undefined => &mut self.undefined,
            },
            Variable::Dimen(dimension_variable) => match dimension_variable {
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
                DimensionVariable::Dimen(register_index) => {
                    &mut self.dimen[register_index as usize]
                }
            },
            Variable::Font(font_variable) => match font_variable {
                FontVariable::CurFont => &mut self.cur_font,
                FontVariable::MathFont { size, number } => match size {
                    MathFontSize::Text => &mut self.text_font[number],
                    MathFontSize::Script => &mut self.script_font[number],
                    MathFontSize::ScriptScript => &mut self.script_script_font[number],
                },
            },
            Variable::Integer(integer_variable) => match integer_variable {
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
                IntegerVariable::Count(register_index) => &mut self.count[register_index as usize],
            },
            Variable::ParShape => &mut self.par_shape,
            Variable::Skip(skip_variable) => match skip_variable {
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
                SkipVariable::Skip(register_index) => &mut self.skip[register_index as usize],
                SkipVariable::MuSkip(register_index) => &mut self.mu_skip[register_index as usize],
            },
            Variable::TokenList(token_list_variable) => match token_list_variable {
                TokenListVariable::OutputRoutine => &mut self.output_routine,
                TokenListVariable::EveryPar => &mut self.every_par,
                TokenListVariable::EveryMath => &mut self.every_math,
                TokenListVariable::EveryDisplay => &mut self.every_display,
                TokenListVariable::EveryHbox => &mut self.every_hbox,
                TokenListVariable::EveryVbox => &mut self.every_vbox,
                TokenListVariable::EveryJob => &mut self.every_job,
                TokenListVariable::EveryCr => &mut self.every_cr,
                TokenListVariable::ErrHelp => &mut self.err_help,
                TokenListVariable::Toks(register_index) => &mut self.toks[register_index as usize],
            },
        };
        let old_level = *target;
        *target = new_level;
        old_level
    }
}

impl Dumpable for VariableLevels {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        // Boxes
        self.boxes.dump(target)?;

        // Codes
        self.cat_code.dump(target)?;
        self.lc_code.dump(target)?;
        self.uc_code.dump(target)?;
        self.sf_code.dump(target)?;
        self.math_code.dump(target)?;
        self.del_code.dump(target)?;

        // Control sequences
        self.active.dump(target)?;
        self.single.dump(target)?;
        self.null_cs.dump(target)?;

        self.escaped.dump(target)?;

        self.frozen_protection.dump(target)?;
        self.frozen_cr.dump(target)?;
        self.frozen_end_group.dump(target)?;
        self.frozen_right.dump(target)?;
        self.frozen_fi.dump(target)?;
        self.frozen_end_template.dump(target)?;
        self.frozen_endv.dump(target)?;
        self.frozen_relax.dump(target)?;
        self.end_write.dump(target)?;
        self.font_id.dump(target)?;
        self.undefined.dump(target)?;

        // Dimensions
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
        self.dimen.dump(target)?;

        // Fonts
        self.cur_font.dump(target)?;
        self.text_font.dump(target)?;
        self.script_font.dump(target)?;
        self.script_script_font.dump(target)?;

        // Integers
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
        self.count.dump(target)?;

        // Parshape
        self.par_shape.dump(target)?;

        // Skips
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
        self.skip.dump(target)?;
        self.mu_skip.dump(target)?;

        // Token lists
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
        // Boxes
        let boxes = Dumpable::undump(lines)?;

        // Codes
        let cat_code = Dumpable::undump(lines)?;
        let lc_code = Dumpable::undump(lines)?;
        let uc_code = Dumpable::undump(lines)?;
        let sf_code = Dumpable::undump(lines)?;
        let math_code = Dumpable::undump(lines)?;
        let del_code = Dumpable::undump(lines)?;

        // Control sequences
        let active = Dumpable::undump(lines)?;
        let single = Dumpable::undump(lines)?;
        let null_cs = Dumpable::undump(lines)?;

        let escaped = Dumpable::undump(lines)?;

        let frozen_protection = Dumpable::undump(lines)?;
        let frozen_cr = Dumpable::undump(lines)?;
        let frozen_end_group = Dumpable::undump(lines)?;
        let frozen_right = Dumpable::undump(lines)?;
        let frozen_fi = Dumpable::undump(lines)?;
        let frozen_end_template = Dumpable::undump(lines)?;
        let frozen_endv = Dumpable::undump(lines)?;
        let frozen_relax = Dumpable::undump(lines)?;
        let end_write = Dumpable::undump(lines)?;
        let font_id = Dumpable::undump(lines)?;
        let undefined = Dumpable::undump(lines)?;

        // Dimensions
        let par_indent = Dumpable::undump(lines)?;
        let math_surround = Dumpable::undump(lines)?;
        let line_skip_limit = Dumpable::undump(lines)?;
        let hsize = Dumpable::undump(lines)?;
        let vsize = Dumpable::undump(lines)?;
        let max_depth = Dumpable::undump(lines)?;
        let split_max_depth = Dumpable::undump(lines)?;
        let box_max_depth = Dumpable::undump(lines)?;
        let hfuzz = Dumpable::undump(lines)?;
        let vfuzz = Dumpable::undump(lines)?;
        let delimiter_shortfall = Dumpable::undump(lines)?;
        let null_delimiter_space = Dumpable::undump(lines)?;
        let script_space = Dumpable::undump(lines)?;
        let pre_display_size = Dumpable::undump(lines)?;
        let display_width = Dumpable::undump(lines)?;
        let display_indent = Dumpable::undump(lines)?;
        let overfull_rule = Dumpable::undump(lines)?;
        let hang_indent = Dumpable::undump(lines)?;
        let h_offset = Dumpable::undump(lines)?;
        let v_offset = Dumpable::undump(lines)?;
        let emergency_stretch = Dumpable::undump(lines)?;
        let dimen = Dumpable::undump(lines)?;

        // Fonts
        let cur_font = Dumpable::undump(lines)?;
        let text_font = Dumpable::undump(lines)?;
        let script_font = Dumpable::undump(lines)?;
        let script_script_font = Dumpable::undump(lines)?;

        // Integers
        let pretolerance = Dumpable::undump(lines)?;
        let tolerance = Dumpable::undump(lines)?;
        let line_penalty = Dumpable::undump(lines)?;
        let hyphen_penalty = Dumpable::undump(lines)?;
        let ex_hyphen_penalty = Dumpable::undump(lines)?;
        let club_penalty = Dumpable::undump(lines)?;
        let widow_penalty = Dumpable::undump(lines)?;
        let display_widow_penalty = Dumpable::undump(lines)?;
        let broken_penalty = Dumpable::undump(lines)?;
        let bin_op_penalty = Dumpable::undump(lines)?;
        let rel_penalty = Dumpable::undump(lines)?;
        let pre_display_penalty = Dumpable::undump(lines)?;
        let post_display_penalty = Dumpable::undump(lines)?;
        let inter_line_penalty = Dumpable::undump(lines)?;
        let double_hyphen_demerits = Dumpable::undump(lines)?;
        let final_hyphen_demerits = Dumpable::undump(lines)?;
        let adj_demerits = Dumpable::undump(lines)?;
        let mag = Dumpable::undump(lines)?;
        let delimiter_factor = Dumpable::undump(lines)?;
        let looseness = Dumpable::undump(lines)?;
        let time = Dumpable::undump(lines)?;
        let day = Dumpable::undump(lines)?;
        let month = Dumpable::undump(lines)?;
        let year = Dumpable::undump(lines)?;
        let show_box_breadth = Dumpable::undump(lines)?;
        let show_box_depth = Dumpable::undump(lines)?;
        let hbadness = Dumpable::undump(lines)?;
        let vbadness = Dumpable::undump(lines)?;
        let pausing = Dumpable::undump(lines)?;
        let tracing_online = Dumpable::undump(lines)?;
        let tracing_macros = Dumpable::undump(lines)?;
        let tracing_stats = Dumpable::undump(lines)?;
        let tracing_paragraphs = Dumpable::undump(lines)?;
        let tracing_pages = Dumpable::undump(lines)?;
        let tracing_output = Dumpable::undump(lines)?;
        let tracing_lost_chars = Dumpable::undump(lines)?;
        let tracing_commands = Dumpable::undump(lines)?;
        let tracing_restores = Dumpable::undump(lines)?;
        let uc_hyph = Dumpable::undump(lines)?;
        let output_penalty = Dumpable::undump(lines)?;
        let max_dead_cycles = Dumpable::undump(lines)?;
        let hang_after = Dumpable::undump(lines)?;
        let floating_penalty = Dumpable::undump(lines)?;
        let global_defs = Dumpable::undump(lines)?;
        let cur_fam = Dumpable::undump(lines)?;
        let escape_char = Dumpable::undump(lines)?;
        let default_hyphen_char = Dumpable::undump(lines)?;
        let default_skew_char = Dumpable::undump(lines)?;
        let end_line_char = Dumpable::undump(lines)?;
        let new_line_char = Dumpable::undump(lines)?;
        let language = Dumpable::undump(lines)?;
        let left_hyphen_min = Dumpable::undump(lines)?;
        let right_hyphen_min = Dumpable::undump(lines)?;
        let holding_inserts = Dumpable::undump(lines)?;
        let error_context_lines = Dumpable::undump(lines)?;
        let count = Dumpable::undump(lines)?;

        // Parshape
        let par_shape = Dumpable::undump(lines)?;

        // Skips
        let line_skip = Dumpable::undump(lines)?;
        let baseline_skip = Dumpable::undump(lines)?;
        let par_skip = Dumpable::undump(lines)?;
        let above_display_skip = Dumpable::undump(lines)?;
        let below_display_skip = Dumpable::undump(lines)?;
        let above_display_short_skip = Dumpable::undump(lines)?;
        let below_display_short_skip = Dumpable::undump(lines)?;
        let left_skip = Dumpable::undump(lines)?;
        let right_skip = Dumpable::undump(lines)?;
        let top_skip = Dumpable::undump(lines)?;
        let split_top_skip = Dumpable::undump(lines)?;
        let tab_skip = Dumpable::undump(lines)?;
        let space_skip = Dumpable::undump(lines)?;
        let xspace_skip = Dumpable::undump(lines)?;
        let par_fill_skip = Dumpable::undump(lines)?;
        let thin_mu_skip = Dumpable::undump(lines)?;
        let med_mu_skip = Dumpable::undump(lines)?;
        let thick_mu_skip = Dumpable::undump(lines)?;
        let skip = Dumpable::undump(lines)?;
        let mu_skip = Dumpable::undump(lines)?;

        // Token lists
        let output_routine = Dumpable::undump(lines)?;
        let every_par = Dumpable::undump(lines)?;
        let every_math = Dumpable::undump(lines)?;
        let every_display = Dumpable::undump(lines)?;
        let every_hbox = Dumpable::undump(lines)?;
        let every_vbox = Dumpable::undump(lines)?;
        let every_job = Dumpable::undump(lines)?;
        let every_cr = Dumpable::undump(lines)?;
        let err_help = Dumpable::undump(lines)?;
        let toks = Dumpable::undump(lines)?;

        Ok(Self {
            boxes,
            cat_code,
            lc_code,
            uc_code,
            sf_code,
            math_code,
            del_code,
            active,
            single,
            null_cs,
            escaped,
            frozen_protection,
            frozen_cr,
            frozen_end_group,
            frozen_right,
            frozen_fi,
            frozen_end_template,
            frozen_endv,
            frozen_relax,
            end_write,
            font_id,
            undefined,
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
            dimen,
            cur_font,
            text_font,
            script_font,
            script_script_font,
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
            count,
            par_shape,
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
            skip,
            mu_skip,
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
