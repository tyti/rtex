use super::control_sequences::ControlSequence;
use super::{
    CodeType, DimensionVariable, Eqtb, IntegerVariable, MathFontSize, SkipVariable,
    TokenListVariable, NULL_FONT,
};
use crate::command::{
    ArithCommand, BoxDimension, Command, ConvertCommand, DefCommand, ExpandableCommand, FiOrElse,
    FractionCommand, FractionType, Hskip, IfTest, LimitType, MakeBox, MarkCommand, MathCommand,
    PageDimension, Prefix, PrefixableCommand, RemoveItem, ShorthandDef, ShowCommand,
    UnexpandableCommand, Vskip,
};
use crate::logger::InteractionMode;
use crate::math::MathStyle;
use crate::nodes::noads::NoadType;
use crate::nodes::LeaderKind;
use crate::scan_internal::ValueType;

pub struct PermanentPrimitiveAddresses {
    pub par_cs: ControlSequence,
    pub write_cs: ControlSequence,
}

impl Eqtb {
    /// Put all the primitvies into the tables.
    /// See 226.
    pub fn put_primitives_into_hash_table(&mut self) -> PermanentPrimitiveAddresses {
        // See 226.
        self.primitive_unexpandable(
            b"lineskip",
            UnexpandableCommand::Prefixable(PrefixableCommand::Glue(SkipVariable::LineSkip)),
        );
        self.primitive_unexpandable(
            b"baselineskip",
            UnexpandableCommand::Prefixable(PrefixableCommand::Glue(SkipVariable::BaselineSkip)),
        );
        self.primitive_unexpandable(
            b"parskip",
            UnexpandableCommand::Prefixable(PrefixableCommand::Glue(SkipVariable::ParSkip)),
        );
        self.primitive_unexpandable(
            b"abovedisplayskip",
            UnexpandableCommand::Prefixable(PrefixableCommand::Glue(
                SkipVariable::AboveDisplaySkip,
            )),
        );
        self.primitive_unexpandable(
            b"belowdisplayskip",
            UnexpandableCommand::Prefixable(PrefixableCommand::Glue(
                SkipVariable::BelowDisplaySkip,
            )),
        );
        self.primitive_unexpandable(
            b"abovedisplayshortskip",
            UnexpandableCommand::Prefixable(PrefixableCommand::Glue(
                SkipVariable::AboveDisplayShortSkip,
            )),
        );
        self.primitive_unexpandable(
            b"belowdisplayshortskip",
            UnexpandableCommand::Prefixable(PrefixableCommand::Glue(
                SkipVariable::BelowDisplayShortSkip,
            )),
        );
        self.primitive_unexpandable(
            b"leftskip",
            UnexpandableCommand::Prefixable(PrefixableCommand::Glue(SkipVariable::LeftSkip)),
        );
        self.primitive_unexpandable(
            b"rightskip",
            UnexpandableCommand::Prefixable(PrefixableCommand::Glue(SkipVariable::RightSkip)),
        );
        self.primitive_unexpandable(
            b"topskip",
            UnexpandableCommand::Prefixable(PrefixableCommand::Glue(SkipVariable::TopSkip)),
        );
        self.primitive_unexpandable(
            b"splittopskip",
            UnexpandableCommand::Prefixable(PrefixableCommand::Glue(SkipVariable::SplitTopSkip)),
        );
        self.primitive_unexpandable(
            b"tabskip",
            UnexpandableCommand::Prefixable(PrefixableCommand::Glue(SkipVariable::TabSkip)),
        );
        self.primitive_unexpandable(
            b"spaceskip",
            UnexpandableCommand::Prefixable(PrefixableCommand::Glue(SkipVariable::SpaceSkip)),
        );
        self.primitive_unexpandable(
            b"xspaceskip",
            UnexpandableCommand::Prefixable(PrefixableCommand::Glue(SkipVariable::XspaceSkip)),
        );
        self.primitive_unexpandable(
            b"parfillskip",
            UnexpandableCommand::Prefixable(PrefixableCommand::Glue(SkipVariable::ParFillSkip)),
        );
        self.primitive_unexpandable(
            b"thinmuskip",
            UnexpandableCommand::Prefixable(PrefixableCommand::MuGlue(SkipVariable::ThinMuSkip)),
        );
        self.primitive_unexpandable(
            b"medmuskip",
            UnexpandableCommand::Prefixable(PrefixableCommand::MuGlue(SkipVariable::MedMuSkip)),
        );
        self.primitive_unexpandable(
            b"thickmuskip",
            UnexpandableCommand::Prefixable(PrefixableCommand::MuGlue(SkipVariable::ThickMuSkip)),
        );

        // See 230.
        self.primitive_unexpandable(
            b"output",
            UnexpandableCommand::Prefixable(PrefixableCommand::TokenList(
                TokenListVariable::OutputRoutine,
            )),
        );
        self.primitive_unexpandable(
            b"everypar",
            UnexpandableCommand::Prefixable(PrefixableCommand::TokenList(
                TokenListVariable::EveryPar,
            )),
        );
        self.primitive_unexpandable(
            b"everymath",
            UnexpandableCommand::Prefixable(PrefixableCommand::TokenList(
                TokenListVariable::EveryMath,
            )),
        );
        self.primitive_unexpandable(
            b"everydisplay",
            UnexpandableCommand::Prefixable(PrefixableCommand::TokenList(
                TokenListVariable::EveryDisplay,
            )),
        );
        self.primitive_unexpandable(
            b"everyhbox",
            UnexpandableCommand::Prefixable(PrefixableCommand::TokenList(
                TokenListVariable::EveryHbox,
            )),
        );
        self.primitive_unexpandable(
            b"everyvbox",
            UnexpandableCommand::Prefixable(PrefixableCommand::TokenList(
                TokenListVariable::EveryVbox,
            )),
        );
        self.primitive_unexpandable(
            b"everyjob",
            UnexpandableCommand::Prefixable(PrefixableCommand::TokenList(
                TokenListVariable::EveryJob,
            )),
        );
        self.primitive_unexpandable(
            b"everycr",
            UnexpandableCommand::Prefixable(PrefixableCommand::TokenList(
                TokenListVariable::EveryCr,
            )),
        );
        self.primitive_unexpandable(
            b"errhelp",
            UnexpandableCommand::Prefixable(PrefixableCommand::TokenList(
                TokenListVariable::ErrHelp,
            )),
        );

        // See 238.
        self.primitive_unexpandable(
            b"pretolerance",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::Pretolerance,
            )),
        );
        self.primitive_unexpandable(
            b"tolerance",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(IntegerVariable::Tolerance)),
        );
        self.primitive_unexpandable(
            b"linepenalty",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::LinePenalty,
            )),
        );
        self.primitive_unexpandable(
            b"hyphenpenalty",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::HyphenPenalty,
            )),
        );
        self.primitive_unexpandable(
            b"exhyphenpenalty",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::ExHyphenPenalty,
            )),
        );
        self.primitive_unexpandable(
            b"clubpenalty",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::ClubPenalty,
            )),
        );
        self.primitive_unexpandable(
            b"widowpenalty",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::WidowPenalty,
            )),
        );
        self.primitive_unexpandable(
            b"displaywidowpenalty",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::DisplayWidowPenalty,
            )),
        );
        self.primitive_unexpandable(
            b"brokenpenalty",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::BrokenPenalty,
            )),
        );
        self.primitive_unexpandable(
            b"binoppenalty",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::BinOpPenalty,
            )),
        );
        self.primitive_unexpandable(
            b"relpenalty",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::RelPenalty,
            )),
        );
        self.primitive_unexpandable(
            b"predisplaypenalty",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::PreDisplayPenalty,
            )),
        );
        self.primitive_unexpandable(
            b"postdisplaypenalty",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::PostDisplayPenalty,
            )),
        );
        self.primitive_unexpandable(
            b"interlinepenalty",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::InterLinePenalty,
            )),
        );
        self.primitive_unexpandable(
            b"doublehyphendemerits",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::DoubleHyphenDemerits,
            )),
        );
        self.primitive_unexpandable(
            b"finalhyphendemerits",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::FinalHyphenDemerits,
            )),
        );
        self.primitive_unexpandable(
            b"adjdemerits",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::AdjDemerits,
            )),
        );
        self.primitive_unexpandable(
            b"mag",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(IntegerVariable::Mag)),
        );
        self.primitive_unexpandable(
            b"delimiterfactor",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::DelimiterFactor,
            )),
        );
        self.primitive_unexpandable(
            b"looseness",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(IntegerVariable::Looseness)),
        );
        self.primitive_unexpandable(
            b"time",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(IntegerVariable::Time)),
        );
        self.primitive_unexpandable(
            b"day",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(IntegerVariable::Day)),
        );
        self.primitive_unexpandable(
            b"month",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(IntegerVariable::Month)),
        );
        self.primitive_unexpandable(
            b"year",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(IntegerVariable::Year)),
        );
        self.primitive_unexpandable(
            b"showboxbreadth",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::ShowBoxBreadth,
            )),
        );
        self.primitive_unexpandable(
            b"showboxdepth",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::ShowBoxDepth,
            )),
        );
        self.primitive_unexpandable(
            b"hbadness",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(IntegerVariable::Hbadness)),
        );
        self.primitive_unexpandable(
            b"vbadness",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(IntegerVariable::Vbadness)),
        );
        self.primitive_unexpandable(
            b"pausing",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(IntegerVariable::Pausing)),
        );
        self.primitive_unexpandable(
            b"tracingonline",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::TracingOnline,
            )),
        );
        self.primitive_unexpandable(
            b"tracingmacros",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::TracingMacros,
            )),
        );
        self.primitive_unexpandable(
            b"tracingstats",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::TracingStats,
            )),
        );
        self.primitive_unexpandable(
            b"tracingparagraphs",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::TracingParagraphs,
            )),
        );
        self.primitive_unexpandable(
            b"tracingpages",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::TracingPages,
            )),
        );
        self.primitive_unexpandable(
            b"tracingoutput",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::TracingOutput,
            )),
        );
        self.primitive_unexpandable(
            b"tracinglostchars",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::TracingLostChars,
            )),
        );
        self.primitive_unexpandable(
            b"tracingcommands",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::TracingCommands,
            )),
        );
        self.primitive_unexpandable(
            b"tracingrestores",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::TracingRestores,
            )),
        );
        self.primitive_unexpandable(
            b"uchyph",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(IntegerVariable::UcHyph)),
        );
        self.primitive_unexpandable(
            b"outputpenalty",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::OutputPenalty,
            )),
        );
        self.primitive_unexpandable(
            b"maxdeadcycles",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::MaxDeadCycles,
            )),
        );
        self.primitive_unexpandable(
            b"hangafter",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(IntegerVariable::HangAfter)),
        );
        self.primitive_unexpandable(
            b"floatingpenalty",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::FloatingPenalty,
            )),
        );
        self.primitive_unexpandable(
            b"globaldefs",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::GlobalDefs,
            )),
        );
        self.primitive_unexpandable(
            b"fam",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(IntegerVariable::CurFam)),
        );
        self.primitive_unexpandable(
            b"escapechar",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::EscapeChar,
            )),
        );
        self.primitive_unexpandable(
            b"defaulthyphenchar",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::DefaultHyphenChar,
            )),
        );
        self.primitive_unexpandable(
            b"defaultskewchar",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::DefaultSkewChar,
            )),
        );
        self.primitive_unexpandable(
            b"endlinechar",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::EndLineChar,
            )),
        );
        self.primitive_unexpandable(
            b"newlinechar",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::NewLineChar,
            )),
        );
        self.primitive_unexpandable(
            b"language",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(IntegerVariable::Language)),
        );
        self.primitive_unexpandable(
            b"lefthyphenmin",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::LeftHyphenMin,
            )),
        );
        self.primitive_unexpandable(
            b"righthyphenmin",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::RightHyphenMin,
            )),
        );
        self.primitive_unexpandable(
            b"holdinginserts",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::HoldingInserts,
            )),
        );
        self.primitive_unexpandable(
            b"errorcontextlines",
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(
                IntegerVariable::ErrorContextLines,
            )),
        );

        // See 248.
        self.primitive_unexpandable(
            b"parindent",
            UnexpandableCommand::Prefixable(PrefixableCommand::Dimension(
                DimensionVariable::ParIndent,
            )),
        );
        self.primitive_unexpandable(
            b"mathsurround",
            UnexpandableCommand::Prefixable(PrefixableCommand::Dimension(
                DimensionVariable::MathSurround,
            )),
        );
        self.primitive_unexpandable(
            b"lineskiplimit",
            UnexpandableCommand::Prefixable(PrefixableCommand::Dimension(
                DimensionVariable::LineSkipLimit,
            )),
        );
        self.primitive_unexpandable(
            b"hsize",
            UnexpandableCommand::Prefixable(PrefixableCommand::Dimension(DimensionVariable::Hsize)),
        );
        self.primitive_unexpandable(
            b"vsize",
            UnexpandableCommand::Prefixable(PrefixableCommand::Dimension(DimensionVariable::Vsize)),
        );
        self.primitive_unexpandable(
            b"maxdepth",
            UnexpandableCommand::Prefixable(PrefixableCommand::Dimension(
                DimensionVariable::MaxDepth,
            )),
        );
        self.primitive_unexpandable(
            b"splitmaxdepth",
            UnexpandableCommand::Prefixable(PrefixableCommand::Dimension(
                DimensionVariable::SplitMaxDepth,
            )),
        );
        self.primitive_unexpandable(
            b"boxmaxdepth",
            UnexpandableCommand::Prefixable(PrefixableCommand::Dimension(
                DimensionVariable::BoxMaxDepth,
            )),
        );
        self.primitive_unexpandable(
            b"hfuzz",
            UnexpandableCommand::Prefixable(PrefixableCommand::Dimension(DimensionVariable::Hfuzz)),
        );
        self.primitive_unexpandable(
            b"vfuzz",
            UnexpandableCommand::Prefixable(PrefixableCommand::Dimension(DimensionVariable::Vfuzz)),
        );
        self.primitive_unexpandable(
            b"delimitershortfall",
            UnexpandableCommand::Prefixable(PrefixableCommand::Dimension(
                DimensionVariable::DelimiterShortfall,
            )),
        );
        self.primitive_unexpandable(
            b"nulldelimiterspace",
            UnexpandableCommand::Prefixable(PrefixableCommand::Dimension(
                DimensionVariable::NullDelimiterSpace,
            )),
        );
        self.primitive_unexpandable(
            b"scriptspace",
            UnexpandableCommand::Prefixable(PrefixableCommand::Dimension(
                DimensionVariable::ScriptSpace,
            )),
        );
        self.primitive_unexpandable(
            b"predisplaysize",
            UnexpandableCommand::Prefixable(PrefixableCommand::Dimension(
                DimensionVariable::PreDisplaySize,
            )),
        );
        self.primitive_unexpandable(
            b"displaywidth",
            UnexpandableCommand::Prefixable(PrefixableCommand::Dimension(
                DimensionVariable::DisplayWidth,
            )),
        );
        self.primitive_unexpandable(
            b"displayindent",
            UnexpandableCommand::Prefixable(PrefixableCommand::Dimension(
                DimensionVariable::DisplayIndent,
            )),
        );
        self.primitive_unexpandable(
            b"overfullrule",
            UnexpandableCommand::Prefixable(PrefixableCommand::Dimension(
                DimensionVariable::OverfullRule,
            )),
        );
        self.primitive_unexpandable(
            b"hangindent",
            UnexpandableCommand::Prefixable(PrefixableCommand::Dimension(
                DimensionVariable::HangIndent,
            )),
        );
        self.primitive_unexpandable(
            b"hoffset",
            UnexpandableCommand::Prefixable(PrefixableCommand::Dimension(
                DimensionVariable::HOffset,
            )),
        );
        self.primitive_unexpandable(
            b"voffset",
            UnexpandableCommand::Prefixable(PrefixableCommand::Dimension(
                DimensionVariable::VOffset,
            )),
        );
        self.primitive_unexpandable(
            b"emergencystretch",
            UnexpandableCommand::Prefixable(PrefixableCommand::Dimension(
                DimensionVariable::EmergencyStretch,
            )),
        );

        // See 265.
        self.primitive_unexpandable(b" ", UnexpandableCommand::ExSpace);
        self.primitive_unexpandable(b"/", UnexpandableCommand::ItalCorr);
        self.primitive_unexpandable(b"accent", UnexpandableCommand::Accent);
        self.primitive_unexpandable(
            b"advance",
            UnexpandableCommand::Prefixable(PrefixableCommand::Arith(ArithCommand::Advance)),
        );
        self.primitive_unexpandable(b"afterassignment", UnexpandableCommand::AfterAssignment);
        self.primitive_unexpandable(b"aftergroup", UnexpandableCommand::AfterGroup);
        self.primitive_unexpandable(b"begingroup", UnexpandableCommand::BeginGroup);
        self.primitive_unexpandable(b"char", UnexpandableCommand::CharNum);
        self.primitive_expandable(b"csname", ExpandableCommand::CsName);
        self.primitive_unexpandable(
            b"delimiter",
            UnexpandableCommand::Math(MathCommand::DelimNum),
        );
        self.primitive_unexpandable(
            b"divide",
            UnexpandableCommand::Prefixable(PrefixableCommand::Arith(ArithCommand::Divide)),
        );
        self.primitive_unexpandable(b"endcsname", UnexpandableCommand::EndCsName);
        let cs = self.primitive_unexpandable(b"endgroup", UnexpandableCommand::EndGroup);
        let cmd = self.control_sequences.get(cs).clone();
        self.control_sequences
            .set(ControlSequence::FrozenEndGroup, cmd);
        self.control_sequences
            .set_text(ControlSequence::FrozenEndGroup, b"endgroup");
        self.variable_levels
            .set(ControlSequence::FrozenEndGroup.to_variable(), 0);
        self.primitive_expandable(b"expandafter", ExpandableCommand::ExpandAfter);
        self.primitive_unexpandable(
            b"font",
            UnexpandableCommand::Prefixable(PrefixableCommand::DefFont),
        );
        self.primitive_unexpandable(
            b"fontdimen",
            UnexpandableCommand::Prefixable(PrefixableCommand::FontDimen),
        );
        self.primitive_unexpandable(b"halign", UnexpandableCommand::Halign);
        self.primitive_unexpandable(b"hrule", UnexpandableCommand::Hrule);
        self.primitive_unexpandable(b"ignorespaces", UnexpandableCommand::IgnoreSpaces);
        self.primitive_unexpandable(b"insert", UnexpandableCommand::Insert);
        self.primitive_unexpandable(b"mark", UnexpandableCommand::Mark);
        self.primitive_unexpandable(
            b"mathaccent",
            UnexpandableCommand::Math(MathCommand::MathAccent),
        );
        self.primitive_unexpandable(
            b"mathchar",
            UnexpandableCommand::Math(MathCommand::MathCharNum),
        );
        self.primitive_unexpandable(
            b"mathchoice",
            UnexpandableCommand::Math(MathCommand::Choice),
        );
        self.primitive_unexpandable(
            b"multiply",
            UnexpandableCommand::Prefixable(PrefixableCommand::Arith(ArithCommand::Multiply)),
        );
        self.primitive_unexpandable(b"noalign", UnexpandableCommand::NoAlign);
        self.primitive_unexpandable(b"noboundary", UnexpandableCommand::NoBoundary);
        self.primitive_expandable(b"noexpand", ExpandableCommand::NoExpand);
        self.primitive_unexpandable(
            b"nonscript",
            UnexpandableCommand::Math(MathCommand::NonScript),
        );
        self.primitive_unexpandable(b"omit", UnexpandableCommand::Omit);
        self.primitive_unexpandable(
            b"parshape",
            UnexpandableCommand::Prefixable(PrefixableCommand::ParShape),
        );
        self.primitive_unexpandable(b"penalty", UnexpandableCommand::Penalty);
        self.primitive_unexpandable(
            b"prevgraf",
            UnexpandableCommand::Prefixable(PrefixableCommand::PrevGraf),
        );
        self.primitive_unexpandable(b"radical", UnexpandableCommand::Math(MathCommand::Radical));
        self.primitive_unexpandable(
            b"read",
            UnexpandableCommand::Prefixable(PrefixableCommand::ReadToCs),
        );
        let cs =
            self.primitive_unexpandable(b"relax", UnexpandableCommand::Relax { no_expand: false });
        let cmd = self.control_sequences.get(cs).clone();
        self.control_sequences
            .set(ControlSequence::FrozenRelax, cmd);
        self.control_sequences
            .set_text(ControlSequence::FrozenRelax, b"relax");
        self.variable_levels
            .set(ControlSequence::FrozenRelax.to_variable(), 0);
        self.primitive_unexpandable(
            b"setbox",
            UnexpandableCommand::Prefixable(PrefixableCommand::SetBox),
        );
        self.primitive_expandable(b"the", ExpandableCommand::The);
        self.primitive_unexpandable(
            b"toks",
            UnexpandableCommand::Prefixable(PrefixableCommand::TokenListRegister),
        );
        self.primitive_unexpandable(b"vadjust", UnexpandableCommand::Vadjust);
        self.primitive_unexpandable(b"valign", UnexpandableCommand::Valign);
        self.primitive_unexpandable(b"vcenter", UnexpandableCommand::Math(MathCommand::Vcenter));
        self.primitive_unexpandable(b"vrule", UnexpandableCommand::Vrule);

        // See 334.
        let par_cs = self.primitive_unexpandable(b"par", UnexpandableCommand::ParEnd);

        // See 376.
        self.primitive_expandable(b"input", ExpandableCommand::Input);
        self.primitive_expandable(b"endinput", ExpandableCommand::EndInput);

        // See 384.
        self.primitive_expandable(b"topmark", ExpandableCommand::Mark(MarkCommand::Top));
        self.primitive_expandable(b"firstmark", ExpandableCommand::Mark(MarkCommand::First));
        self.primitive_expandable(b"botmark", ExpandableCommand::Mark(MarkCommand::Bot));
        self.primitive_expandable(
            b"splitfirstmark",
            ExpandableCommand::Mark(MarkCommand::SplitFirst),
        );
        self.primitive_expandable(
            b"splitbotmark",
            ExpandableCommand::Mark(MarkCommand::SplitBot),
        );

        // See 411.
        self.primitive_unexpandable(
            b"count",
            UnexpandableCommand::Prefixable(PrefixableCommand::Register(ValueType::Int)),
        );
        self.primitive_unexpandable(
            b"dimen",
            UnexpandableCommand::Prefixable(PrefixableCommand::Register(ValueType::Dimen)),
        );
        self.primitive_unexpandable(
            b"skip",
            UnexpandableCommand::Prefixable(PrefixableCommand::Register(ValueType::Glue)),
        );
        self.primitive_unexpandable(
            b"muskip",
            UnexpandableCommand::Prefixable(PrefixableCommand::Register(ValueType::Mu)),
        );

        // See 416.
        self.primitive_unexpandable(
            b"spacefactor",
            UnexpandableCommand::Prefixable(PrefixableCommand::SpaceFactor),
        );
        self.primitive_unexpandable(
            b"prevdepth",
            UnexpandableCommand::Prefixable(PrefixableCommand::PrevDepth),
        );
        self.primitive_unexpandable(
            b"deadcycles",
            UnexpandableCommand::Prefixable(PrefixableCommand::DeadCycles),
        );
        self.primitive_unexpandable(
            b"insertpenalties",
            UnexpandableCommand::Prefixable(PrefixableCommand::InsertPenalties),
        );
        self.primitive_unexpandable(
            b"wd",
            UnexpandableCommand::Prefixable(PrefixableCommand::BoxDimen(BoxDimension::Width)),
        );
        self.primitive_unexpandable(
            b"ht",
            UnexpandableCommand::Prefixable(PrefixableCommand::BoxDimen(BoxDimension::Height)),
        );
        self.primitive_unexpandable(
            b"dp",
            UnexpandableCommand::Prefixable(PrefixableCommand::BoxDimen(BoxDimension::Depth)),
        );
        self.primitive_unexpandable(b"lastpenalty", UnexpandableCommand::LastPenalty);
        self.primitive_unexpandable(b"lastkern", UnexpandableCommand::LastKern);
        self.primitive_unexpandable(b"lastskip", UnexpandableCommand::LastSkip);
        self.primitive_unexpandable(b"inputlineno", UnexpandableCommand::InputLineNumber);
        self.primitive_unexpandable(b"badness", UnexpandableCommand::Badness);

        // See 468.
        self.primitive_expandable(
            b"number",
            ExpandableCommand::Convert(ConvertCommand::Number),
        );
        self.primitive_expandable(
            b"romannumeral",
            ExpandableCommand::Convert(ConvertCommand::RomanNumeral),
        );
        self.primitive_expandable(
            b"string",
            ExpandableCommand::Convert(ConvertCommand::String),
        );
        self.primitive_expandable(
            b"meaning",
            ExpandableCommand::Convert(ConvertCommand::Meaning),
        );
        self.primitive_expandable(
            b"fontname",
            ExpandableCommand::Convert(ConvertCommand::FontName),
        );
        self.primitive_expandable(
            b"jobname",
            ExpandableCommand::Convert(ConvertCommand::JobName),
        );

        // See 487.
        self.primitive_expandable(b"if", ExpandableCommand::IfTest(IfTest::IfChar));
        self.primitive_expandable(b"ifcat", ExpandableCommand::IfTest(IfTest::IfCat));
        self.primitive_expandable(b"ifnum", ExpandableCommand::IfTest(IfTest::IfInt));
        self.primitive_expandable(b"ifdim", ExpandableCommand::IfTest(IfTest::IfDim));
        self.primitive_expandable(b"ifodd", ExpandableCommand::IfTest(IfTest::IfOdd));
        self.primitive_expandable(b"ifvmode", ExpandableCommand::IfTest(IfTest::IfVmode));
        self.primitive_expandable(b"ifhmode", ExpandableCommand::IfTest(IfTest::IfHmode));
        self.primitive_expandable(b"ifmmode", ExpandableCommand::IfTest(IfTest::IfMmode));
        self.primitive_expandable(b"ifinner", ExpandableCommand::IfTest(IfTest::IfInner));
        self.primitive_expandable(b"ifvoid", ExpandableCommand::IfTest(IfTest::IfVoid));
        self.primitive_expandable(b"ifhbox", ExpandableCommand::IfTest(IfTest::IfHbox));
        self.primitive_expandable(b"ifvbox", ExpandableCommand::IfTest(IfTest::IfVbox));
        self.primitive_expandable(b"ifx", ExpandableCommand::IfTest(IfTest::Ifx));
        self.primitive_expandable(b"ifeof", ExpandableCommand::IfTest(IfTest::IfEof));
        self.primitive_expandable(b"iftrue", ExpandableCommand::IfTest(IfTest::IfTrue));
        self.primitive_expandable(b"iffalse", ExpandableCommand::IfTest(IfTest::IfFalse));
        self.primitive_expandable(b"ifcase", ExpandableCommand::IfTest(IfTest::IfCase));

        // See 491.
        self.primitive_expandable(b"fi", ExpandableCommand::FiOrElse(FiOrElse::Fi));
        self.control_sequences.set(
            ControlSequence::FrozenFi,
            Command::Expandable(ExpandableCommand::FiOrElse(FiOrElse::Fi)),
        );
        self.control_sequences
            .set_text(ControlSequence::FrozenFi, b"fi");
        self.variable_levels
            .set(ControlSequence::FrozenFi.to_variable(), 0);
        self.primitive_expandable(b"or", ExpandableCommand::FiOrElse(FiOrElse::Or));
        self.primitive_expandable(b"else", ExpandableCommand::FiOrElse(FiOrElse::Else));

        // See 553.
        let cs = self.primitive_unexpandable(
            b"nullfont",
            UnexpandableCommand::Prefixable(PrefixableCommand::SetFont(NULL_FONT)),
        );
        let cmd = self.control_sequences.get(cs).clone();
        self.control_sequences.set(ControlSequence::FontId(0), cmd);
        self.control_sequences
            .set_text(ControlSequence::FontId(0), b"nullfont");
        self.variable_levels
            .set(ControlSequence::FontId(0).to_variable(), 0);

        // See 780.
        self.primitive_unexpandable(b"span", UnexpandableCommand::Span);
        let cs = self.primitive_unexpandable(b"cr", UnexpandableCommand::CarRet { weak: false });
        let cmd = self.control_sequences.get(cs).clone();
        self.control_sequences.set(ControlSequence::FrozenCr, cmd);
        self.control_sequences
            .set_text(ControlSequence::FrozenCr, b"cr");
        self.variable_levels
            .set(ControlSequence::FrozenCr.to_variable(), 0);
        self.primitive_unexpandable(b"crcr", UnexpandableCommand::CarRet { weak: true });
        let command = Command::Expandable(ExpandableCommand::EndTemplate);
        self.control_sequences
            .set(ControlSequence::FrozenEndTemplate, command);
        self.variable_levels
            .set(ControlSequence::FrozenEndTemplate.to_variable(), 0);
        self.control_sequences
            .set_text(ControlSequence::FrozenEndTemplate, b"endtemplate");
        let command = Command::Unexpandable(UnexpandableCommand::Endv);
        self.control_sequences
            .set(ControlSequence::FrozenEndv, command);
        self.variable_levels
            .set(ControlSequence::FrozenEndv.to_variable(), 0);
        self.control_sequences
            .set_text(ControlSequence::FrozenEndv, b"endtemplate");

        // See 983.
        self.primitive_unexpandable(
            b"pagegoal",
            UnexpandableCommand::Prefixable(PrefixableCommand::PageDimen(PageDimension::PageGoal)),
        );
        self.primitive_unexpandable(
            b"pagetotal",
            UnexpandableCommand::Prefixable(PrefixableCommand::PageDimen(PageDimension::Height)),
        );
        self.primitive_unexpandable(
            b"pagestretch",
            UnexpandableCommand::Prefixable(PrefixableCommand::PageDimen(PageDimension::Stretch)),
        );
        self.primitive_unexpandable(
            b"pagefilstretch",
            UnexpandableCommand::Prefixable(PrefixableCommand::PageDimen(
                PageDimension::FilStretch,
            )),
        );
        self.primitive_unexpandable(
            b"pagefillstretch",
            UnexpandableCommand::Prefixable(PrefixableCommand::PageDimen(
                PageDimension::FillStretch,
            )),
        );
        self.primitive_unexpandable(
            b"pagefilllstretch",
            UnexpandableCommand::Prefixable(PrefixableCommand::PageDimen(
                PageDimension::FilllStretch,
            )),
        );
        self.primitive_unexpandable(
            b"pageshrink",
            UnexpandableCommand::Prefixable(PrefixableCommand::PageDimen(PageDimension::Shrink)),
        );
        self.primitive_unexpandable(
            b"pagedepth",
            UnexpandableCommand::Prefixable(PrefixableCommand::PageDimen(PageDimension::Depth)),
        );

        // See 1052.
        self.primitive_unexpandable(b"end", UnexpandableCommand::End { dumping: false });
        self.primitive_unexpandable(b"dump", UnexpandableCommand::End { dumping: true });

        // See 1058.
        self.primitive_unexpandable(b"hskip", UnexpandableCommand::Hskip(Hskip::Hskip));
        self.primitive_unexpandable(b"hfil", UnexpandableCommand::Hskip(Hskip::Hfil));
        self.primitive_unexpandable(b"hfill", UnexpandableCommand::Hskip(Hskip::Hfill));
        self.primitive_unexpandable(b"hss", UnexpandableCommand::Hskip(Hskip::Hss));
        self.primitive_unexpandable(b"hfilneg", UnexpandableCommand::Hskip(Hskip::Hfilneg));

        self.primitive_unexpandable(b"vskip", UnexpandableCommand::Vskip(Vskip::Vskip));
        self.primitive_unexpandable(b"vfil", UnexpandableCommand::Vskip(Vskip::Vfil));
        self.primitive_unexpandable(b"vfill", UnexpandableCommand::Vskip(Vskip::Vfill));
        self.primitive_unexpandable(b"vss", UnexpandableCommand::Vskip(Vskip::Vss));
        self.primitive_unexpandable(b"vfilneg", UnexpandableCommand::Vskip(Vskip::Vfilneg));

        self.primitive_unexpandable(b"mskip", UnexpandableCommand::Math(MathCommand::MSkip));
        self.primitive_unexpandable(b"kern", UnexpandableCommand::Kern);
        self.primitive_unexpandable(b"mkern", UnexpandableCommand::Math(MathCommand::MKern));

        // See 1071.
        self.primitive_unexpandable(b"moveleft", UnexpandableCommand::MoveLeft);
        self.primitive_unexpandable(b"moveright", UnexpandableCommand::MoveRight);
        self.primitive_unexpandable(b"raise", UnexpandableCommand::Raise);
        self.primitive_unexpandable(b"lower", UnexpandableCommand::Lower);
        self.primitive_unexpandable(b"box", UnexpandableCommand::MakeBox(MakeBox::Box));
        self.primitive_unexpandable(b"copy", UnexpandableCommand::MakeBox(MakeBox::Copy));
        self.primitive_unexpandable(b"lastbox", UnexpandableCommand::MakeBox(MakeBox::LastBox));
        self.primitive_unexpandable(b"vsplit", UnexpandableCommand::MakeBox(MakeBox::VSplit));
        self.primitive_unexpandable(b"vtop", UnexpandableCommand::MakeBox(MakeBox::VTop));
        self.primitive_unexpandable(b"vbox", UnexpandableCommand::MakeBox(MakeBox::VBox));
        self.primitive_unexpandable(b"hbox", UnexpandableCommand::MakeBox(MakeBox::HBox));
        self.primitive_unexpandable(b"shipout", UnexpandableCommand::ShipOut);
        self.primitive_unexpandable(
            b"leaders",
            UnexpandableCommand::Leaders(LeaderKind::ALeaders),
        );
        self.primitive_unexpandable(
            b"cleaders",
            UnexpandableCommand::Leaders(LeaderKind::CLeaders),
        );
        self.primitive_unexpandable(
            b"xleaders",
            UnexpandableCommand::Leaders(LeaderKind::XLeaders),
        );

        // See 1088.
        self.primitive_unexpandable(
            b"indent",
            UnexpandableCommand::StartPar { is_indented: true },
        );
        self.primitive_unexpandable(
            b"noindent",
            UnexpandableCommand::StartPar { is_indented: false },
        );

        // See 1107.
        self.primitive_unexpandable(
            b"unpenalty",
            UnexpandableCommand::RemoveItem(RemoveItem::Penalty),
        );
        self.primitive_unexpandable(b"unkern", UnexpandableCommand::RemoveItem(RemoveItem::Kern));
        self.primitive_unexpandable(b"unskip", UnexpandableCommand::RemoveItem(RemoveItem::Glue));
        self.primitive_unexpandable(b"unhbox", UnexpandableCommand::UnHbox { copy: false });
        self.primitive_unexpandable(b"unhcopy", UnexpandableCommand::UnHbox { copy: true });
        self.primitive_unexpandable(b"unvbox", UnexpandableCommand::UnVbox { copy: false });
        self.primitive_unexpandable(b"unvcopy", UnexpandableCommand::UnVbox { copy: true });

        // See 1114.
        self.primitive_unexpandable(b"-", UnexpandableCommand::HyphenBreak);
        self.primitive_unexpandable(b"discretionary", UnexpandableCommand::Discretionary);

        // See 1141.
        self.primitive_unexpandable(b"eqno", UnexpandableCommand::EqNo { is_left: false });
        self.primitive_unexpandable(b"leqno", UnexpandableCommand::EqNo { is_left: true });

        // See 1156.
        self.primitive_unexpandable(
            b"mathord",
            UnexpandableCommand::Math(MathCommand::NormalNoad(NoadType::Ord)),
        );
        self.primitive_unexpandable(
            b"mathop",
            UnexpandableCommand::Math(MathCommand::NormalNoad(NoadType::Op {
                limit_type: LimitType::DisplayLimits,
            })),
        );
        self.primitive_unexpandable(
            b"mathbin",
            UnexpandableCommand::Math(MathCommand::NormalNoad(NoadType::Bin)),
        );
        self.primitive_unexpandable(
            b"mathrel",
            UnexpandableCommand::Math(MathCommand::NormalNoad(NoadType::Rel)),
        );
        self.primitive_unexpandable(
            b"mathopen",
            UnexpandableCommand::Math(MathCommand::NormalNoad(NoadType::Open)),
        );
        self.primitive_unexpandable(
            b"mathclose",
            UnexpandableCommand::Math(MathCommand::NormalNoad(NoadType::Close)),
        );
        self.primitive_unexpandable(
            b"mathpunct",
            UnexpandableCommand::Math(MathCommand::NormalNoad(NoadType::Punct)),
        );
        self.primitive_unexpandable(
            b"mathinner",
            UnexpandableCommand::Math(MathCommand::NormalNoad(NoadType::Inner)),
        );
        self.primitive_unexpandable(
            b"underline",
            UnexpandableCommand::Math(MathCommand::Underline),
        );
        self.primitive_unexpandable(
            b"overline",
            UnexpandableCommand::Math(MathCommand::Overline),
        );
        self.primitive_unexpandable(
            b"displaylimits",
            UnexpandableCommand::Math(MathCommand::Limits(LimitType::DisplayLimits)),
        );
        self.primitive_unexpandable(
            b"limits",
            UnexpandableCommand::Math(MathCommand::Limits(LimitType::Limits)),
        );
        self.primitive_unexpandable(
            b"nolimits",
            UnexpandableCommand::Math(MathCommand::Limits(LimitType::NoLimits)),
        );

        // See 1169.
        self.primitive_unexpandable(
            b"displaystyle",
            UnexpandableCommand::Math(MathCommand::Style(MathStyle::Display { cramped: false })),
        );
        self.primitive_unexpandable(
            b"textstyle",
            UnexpandableCommand::Math(MathCommand::Style(MathStyle::Text { cramped: false })),
        );
        self.primitive_unexpandable(
            b"scriptstyle",
            UnexpandableCommand::Math(MathCommand::Style(MathStyle::Script { cramped: false })),
        );
        self.primitive_unexpandable(
            b"scriptscriptstyle",
            UnexpandableCommand::Math(MathCommand::Style(MathStyle::ScriptScript {
                cramped: false,
            })),
        );

        // See 1178.
        self.primitive_unexpandable(
            b"above",
            UnexpandableCommand::Math(MathCommand::Fraction(FractionCommand {
                typ: FractionType::Above,
                is_delimited: false,
            })),
        );
        self.primitive_unexpandable(
            b"over",
            UnexpandableCommand::Math(MathCommand::Fraction(FractionCommand {
                typ: FractionType::Over,
                is_delimited: false,
            })),
        );
        self.primitive_unexpandable(
            b"atop",
            UnexpandableCommand::Math(MathCommand::Fraction(FractionCommand {
                typ: FractionType::Atop,
                is_delimited: false,
            })),
        );
        self.primitive_unexpandable(
            b"abovewithdelims",
            UnexpandableCommand::Math(MathCommand::Fraction(FractionCommand {
                typ: FractionType::Above,
                is_delimited: true,
            })),
        );
        self.primitive_unexpandable(
            b"overwithdelims",
            UnexpandableCommand::Math(MathCommand::Fraction(FractionCommand {
                typ: FractionType::Over,
                is_delimited: true,
            })),
        );
        self.primitive_unexpandable(
            b"atopwithdelims",
            UnexpandableCommand::Math(MathCommand::Fraction(FractionCommand {
                typ: FractionType::Atop,
                is_delimited: true,
            })),
        );

        // See 1188.
        self.primitive_unexpandable(b"left", UnexpandableCommand::Math(MathCommand::Left));
        self.primitive_unexpandable(b"right", UnexpandableCommand::Math(MathCommand::Right));
        self.control_sequences.set(
            ControlSequence::FrozenRight,
            Command::Unexpandable(UnexpandableCommand::Math(MathCommand::Right)),
        );
        self.control_sequences
            .set_text(ControlSequence::FrozenRight, b"right");
        self.variable_levels
            .set(ControlSequence::FrozenRight.to_variable(), 0);

        // See 1208.
        self.primitive_unexpandable(
            b"long",
            UnexpandableCommand::Prefixable(PrefixableCommand::Prefix(Prefix::Long)),
        );
        self.primitive_unexpandable(
            b"outer",
            UnexpandableCommand::Prefixable(PrefixableCommand::Prefix(Prefix::Outer)),
        );
        self.primitive_unexpandable(
            b"global",
            UnexpandableCommand::Prefixable(PrefixableCommand::Prefix(Prefix::Global)),
        );
        self.primitive_unexpandable(
            b"def",
            UnexpandableCommand::Prefixable(PrefixableCommand::Def(DefCommand {
                global: false,
                expand: false,
            })),
        );
        self.primitive_unexpandable(
            b"gdef",
            UnexpandableCommand::Prefixable(PrefixableCommand::Def(DefCommand {
                global: true,
                expand: false,
            })),
        );
        self.primitive_unexpandable(
            b"edef",
            UnexpandableCommand::Prefixable(PrefixableCommand::Def(DefCommand {
                global: false,
                expand: true,
            })),
        );
        self.primitive_unexpandable(
            b"xdef",
            UnexpandableCommand::Prefixable(PrefixableCommand::Def(DefCommand {
                global: true,
                expand: true,
            })),
        );

        // See 1219.
        self.primitive_unexpandable(
            b"let",
            UnexpandableCommand::Prefixable(PrefixableCommand::Let),
        );
        self.primitive_unexpandable(
            b"futurelet",
            UnexpandableCommand::Prefixable(PrefixableCommand::FutureLet),
        );

        // See 1222.
        self.primitive_unexpandable(
            b"chardef",
            UnexpandableCommand::Prefixable(PrefixableCommand::ShorthandDef(ShorthandDef::CharDef)),
        );
        self.primitive_unexpandable(
            b"mathchardef",
            UnexpandableCommand::Prefixable(PrefixableCommand::ShorthandDef(
                ShorthandDef::MathCharDef,
            )),
        );
        self.primitive_unexpandable(
            b"countdef",
            UnexpandableCommand::Prefixable(PrefixableCommand::ShorthandDef(
                ShorthandDef::CountDef,
            )),
        );
        self.primitive_unexpandable(
            b"dimendef",
            UnexpandableCommand::Prefixable(PrefixableCommand::ShorthandDef(
                ShorthandDef::DimenDef,
            )),
        );
        self.primitive_unexpandable(
            b"skipdef",
            UnexpandableCommand::Prefixable(PrefixableCommand::ShorthandDef(ShorthandDef::SkipDef)),
        );
        self.primitive_unexpandable(
            b"muskipdef",
            UnexpandableCommand::Prefixable(PrefixableCommand::ShorthandDef(
                ShorthandDef::MuSkipDef,
            )),
        );
        self.primitive_unexpandable(
            b"toksdef",
            UnexpandableCommand::Prefixable(PrefixableCommand::ShorthandDef(ShorthandDef::ToksDef)),
        );

        // See 1230.
        self.primitive_unexpandable(
            b"catcode",
            UnexpandableCommand::Prefixable(PrefixableCommand::CatCode),
        );
        self.primitive_unexpandable(
            b"mathcode",
            UnexpandableCommand::Prefixable(PrefixableCommand::Code(CodeType::MathCode)),
        );
        self.primitive_unexpandable(
            b"lccode",
            UnexpandableCommand::Prefixable(PrefixableCommand::Code(CodeType::LcCode)),
        );
        self.primitive_unexpandable(
            b"uccode",
            UnexpandableCommand::Prefixable(PrefixableCommand::Code(CodeType::UcCode)),
        );
        self.primitive_unexpandable(
            b"sfcode",
            UnexpandableCommand::Prefixable(PrefixableCommand::Code(CodeType::SfCode)),
        );
        self.primitive_unexpandable(
            b"delcode",
            UnexpandableCommand::Prefixable(PrefixableCommand::Code(CodeType::DelCode)),
        );
        self.primitive_unexpandable(
            b"textfont",
            UnexpandableCommand::Prefixable(PrefixableCommand::DefFamily(MathFontSize::Text)),
        );
        self.primitive_unexpandable(
            b"scriptfont",
            UnexpandableCommand::Prefixable(PrefixableCommand::DefFamily(MathFontSize::Script)),
        );
        self.primitive_unexpandable(
            b"scriptscriptfont",
            UnexpandableCommand::Prefixable(PrefixableCommand::DefFamily(
                MathFontSize::ScriptScript,
            )),
        );

        // See 1250.
        self.primitive_unexpandable(
            b"hyphenation",
            UnexpandableCommand::Prefixable(PrefixableCommand::Hyphenation),
        );
        self.primitive_unexpandable(
            b"patterns",
            UnexpandableCommand::Prefixable(PrefixableCommand::Patterns),
        );

        // See 1254.
        self.primitive_unexpandable(
            b"hyphenchar",
            UnexpandableCommand::Prefixable(PrefixableCommand::HyphenChar),
        );
        self.primitive_unexpandable(
            b"skewchar",
            UnexpandableCommand::Prefixable(PrefixableCommand::SkewChar),
        );

        // See 1262.
        self.primitive_unexpandable(
            b"batchmode",
            UnexpandableCommand::Prefixable(PrefixableCommand::SetInteraction(
                InteractionMode::Batch,
            )),
        );
        self.primitive_unexpandable(
            b"nonstopmode",
            UnexpandableCommand::Prefixable(PrefixableCommand::SetInteraction(
                InteractionMode::Nonstop,
            )),
        );
        self.primitive_unexpandable(
            b"scrollmode",
            UnexpandableCommand::Prefixable(PrefixableCommand::SetInteraction(
                InteractionMode::Scroll,
            )),
        );
        self.primitive_unexpandable(
            b"errorstopmode",
            UnexpandableCommand::Prefixable(PrefixableCommand::SetInteraction(
                InteractionMode::ErrorStop,
            )),
        );

        // See 1272.
        self.primitive_unexpandable(b"openin", UnexpandableCommand::OpenIn);
        self.primitive_unexpandable(b"closein", UnexpandableCommand::CloseIn);

        // See 1277.
        self.primitive_unexpandable(b"message", UnexpandableCommand::Message { is_err: false });
        self.primitive_unexpandable(b"errmessage", UnexpandableCommand::Message { is_err: true });

        // See 1286.
        self.primitive_unexpandable(b"lowercase", UnexpandableCommand::LowerCase);
        self.primitive_unexpandable(b"uppercase", UnexpandableCommand::UpperCase);

        // See 1291.
        self.primitive_unexpandable(b"show", UnexpandableCommand::Show(ShowCommand::Show));
        self.primitive_unexpandable(b"showbox", UnexpandableCommand::Show(ShowCommand::ShowBox));
        self.primitive_unexpandable(b"showthe", UnexpandableCommand::Show(ShowCommand::ShowThe));
        self.primitive_unexpandable(
            b"showlists",
            UnexpandableCommand::Show(ShowCommand::ShowLists),
        );

        // See 1344.
        self.primitive_unexpandable(b"openout", UnexpandableCommand::OpenOut);
        let write_cs = self.primitive_unexpandable(b"write", UnexpandableCommand::Write);
        self.primitive_unexpandable(b"closeout", UnexpandableCommand::CloseOut);
        self.primitive_unexpandable(b"special", UnexpandableCommand::Special);
        self.primitive_unexpandable(b"immediate", UnexpandableCommand::Immediate);
        self.primitive_unexpandable(b"setlanguage", UnexpandableCommand::SetLanguage);

        PermanentPrimitiveAddresses { par_cs, write_cs }
    }
}
