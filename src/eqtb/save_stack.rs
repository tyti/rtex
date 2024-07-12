use crate::box_building::BoxContext;
use crate::dimension::Dimension;
use crate::eqtb::{Definition, RegisterIndex};
use crate::packaging::TargetSpec;
use crate::token::Token;

use super::Level;

pub struct Group {
    pub typ: GroupType,
    pub saved_definitions: Vec<SaveEntry>,
    pub after_tokens: Vec<Token>,
}

// From 268.
pub struct SaveEntry {
    pub definition: Definition,
    pub level: Level,
}

// From 269.
#[derive(Debug, Clone, Copy)]
pub enum GroupType {
    /// The global group.
    BottomLevel,
    /// A scope created by standalone braces.
    Simple,
    /// An \hbox.
    Hbox {
        context: BoxContext,
        spec: TargetSpec,
    },
    /// An \hbox in vertical mode.
    AdjustedHbox {
        shift_amount: Dimension,
        spec: TargetSpec,
    },
    /// A \vbox.
    Vbox {
        context: BoxContext,
        spec: TargetSpec,
    },
    /// A \vtop.
    Vtop {
        context: BoxContext,
        spec: TargetSpec,
    },
    /// An \halign or \valign.
    Align { spec: TargetSpec },
    /// A single entry (cell) in an alignment.
    AlignEntry,
    /// A \noalign.
    NoAlign,
    /// An output routine.
    Output,
    /// A group within math.
    Math { subformula_type: SubformulaType },
    /// A discretionary.
    Disc { typ: DiscGroupType },
    /// An \insert or \vadjust.
    Insert { number: RegisterIndex },
    /// A \vcenter.
    Vcenter { spec: TargetSpec },
    /// A \mathchoice.
    MathChoice { t: u8 },
    /// Between \begingroup and \endgroup.
    SemiSimple,
    /// Between $ and $.
    MathShift { kind: MathShiftKind },
    /// Between \left and \right.
    MathLeft,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SubformulaType {
    Nucleus,
    Subscript,
    Superscript,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DiscGroupType {
    PreBreak,
    PostBreak,
    NoBreak,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MathShiftKind {
    Display,
    Inline,
    LeftEqNo,
    RightEqNo,
}
