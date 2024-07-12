use super::box_dimension::BoxDimension;
use super::page_dimension::PageDimension;
use crate::eqtb::{
    CodeType, DimensionVariable, FontIndex, IntegerVariable, MathFontSize, SkipVariable,
    TokenListVariable,
};
use crate::scan_internal::ValueType;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InternalCommand {
    CharGiven(u8),
    MathCharGiven(u16),
    LastPenalty,
    LastKern,
    LastSkip,
    Toks(ToksCommand),
    Badness,
    InputLineNumber,
    Integer(IntegerVariable),
    Dimension(DimensionVariable),
    Glue(SkipVariable),
    MuGlue(SkipVariable),
    FontDimen,
    HyphenChar,
    SkewChar,
    SpaceFactor,
    PrevDepth,
    PrevGraf,
    PageDimen(PageDimension),
    DeadCycles,
    InsertPenalties,
    BoxDimen(BoxDimension),
    ParShape,
    CatCode,
    Code(CodeType),
    Register(ValueType),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ToksCommand {
    TokenListRegister,
    TokenList(TokenListVariable),
    DefFamily(MathFontSize),
    SetFont(FontIndex),
    DefFont,
}
