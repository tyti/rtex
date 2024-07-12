use crate::command::{BoxDimension, InternalCommand, PageDimension, ToksCommand};
use crate::dimension::{Dimension, MAX_DIMEN};
use crate::eqtb::Eqtb;
use crate::eqtb::{
    CodeType, DimensionVariable, FontIndex, IntegerVariable, LastNodeInfo, ParShapeVariable,
    SkipVariable, TokenListVariable,
};
use crate::error::mu_error;
use crate::fonts::{find_font_dimen, scan_font_ident};
use crate::format::{Dumpable, FormatError};
use crate::input::Scanner;
use crate::logger::Logger;
use crate::nodes::GlueSpec;
use crate::page_breaking::PageContents;
use crate::print::Printer;
use crate::semantic_nest::Mode;
use crate::token::Token;

use std::io::Write;
use std::rc::Rc;

/// See 410.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueType {
    Int,
    Dimen,
    Glue,
    Mu,
}

/// See 410.
pub enum InternalValue {
    Int(i32),
    Dimen(Dimension),
    Glue(Rc<GlueSpec>),
    MuGlue(Rc<GlueSpec>),
    Ident(FontIndex),
    TokenList(Vec<Token>),
}

/// See 413.
pub fn scan_internal_toks(
    internal_command: InternalCommand,
    token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> InternalValue {
    scan_something_internal(internal_command, token, true, scanner, eqtb, logger)
}

/// See 413.
pub fn scan_internal_integer(
    internal_command: InternalCommand,
    token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> i32 {
    let value = scan_something_internal(internal_command, token, false, scanner, eqtb, logger);
    // Coerce internal value to integer.
    match value {
        InternalValue::MuGlue(glue_spec) => {
            mu_error(scanner, eqtb, logger);
            glue_spec.width
        }
        InternalValue::Glue(glue_spec) => glue_spec.width,
        InternalValue::Dimen(dimen) => dimen,
        InternalValue::Int(integer) => integer,
        InternalValue::Ident(_) | InternalValue::TokenList(_) => panic!("Should not be possible"),
    }
}

/// See 413.
pub fn scan_internal_dimension(
    internal_command: InternalCommand,
    token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> InternalValue {
    let mut value = scan_something_internal(internal_command, token, false, scanner, eqtb, logger);
    // Coerce internal value to dimension.
    if let InternalValue::MuGlue(glue_spec) = &value {
        mu_error(scanner, eqtb, logger);
        value = InternalValue::Glue(glue_spec.clone());
    }
    if let InternalValue::Glue(glue_spec) = &value {
        value = InternalValue::Dimen(glue_spec.width);
    }
    value
}

/// See 413.
pub fn scan_internal_glue(
    internal_command: InternalCommand,
    token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> InternalValue {
    let mut value = scan_something_internal(internal_command, token, false, scanner, eqtb, logger);
    // Coerce internal value to Glue.
    if let InternalValue::MuGlue(glue_spec) = value {
        mu_error(scanner, eqtb, logger);
        value = InternalValue::Glue(glue_spec);
    }
    value
}

/// See 413.
pub fn scan_internal_mu_glue(
    internal_command: InternalCommand,
    token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> InternalValue {
    scan_something_internal(internal_command, token, false, scanner, eqtb, logger)
}

/// Returns the value and value level.
/// See 413.
fn scan_something_internal(
    internal_command: InternalCommand,
    token: Token,
    toks_allowed: bool,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> InternalValue {
    let value = match internal_command {
        InternalCommand::CharGiven(c) => InternalValue::Int(c as i32),
        InternalCommand::MathCharGiven(c) => InternalValue::Int(c as i32),
        InternalCommand::LastPenalty => fetch_last_penalty(scanner, eqtb),
        InternalCommand::LastKern => fetch_last_kern(scanner, eqtb),
        InternalCommand::LastSkip => fetch_last_skip(scanner, eqtb),
        InternalCommand::Badness => InternalValue::Int(eqtb.last_badness),
        InternalCommand::InputLineNumber => InternalValue::Int(eqtb.line_number() as i32),
        InternalCommand::Toks(toks_command) => fetch_token_list_or_font_identifier(
            toks_command,
            toks_allowed,
            token,
            scanner,
            eqtb,
            logger,
        ),
        InternalCommand::Integer(int_var) => InternalValue::Int(eqtb.integer(int_var)),
        InternalCommand::Dimension(dim_var) => InternalValue::Dimen(eqtb.dimen(dim_var)),
        InternalCommand::Glue(glue_var) => InternalValue::Glue(eqtb.skips.get(glue_var).clone()),
        InternalCommand::MuGlue(mu_glue_var) => {
            InternalValue::MuGlue(eqtb.skips.get(mu_glue_var).clone())
        }
        InternalCommand::FontDimen => fetch_font_dimension(scanner, eqtb, logger),
        InternalCommand::HyphenChar => {
            let font_index = scan_font_ident(scanner, eqtb, logger);
            InternalValue::Int(eqtb.fonts[font_index as usize].hyphen_char)
        }
        InternalCommand::SkewChar => {
            let font_index = scan_font_ident(scanner, eqtb, logger);
            InternalValue::Int(eqtb.fonts[font_index as usize].skew_char)
        }
        InternalCommand::SpaceFactor => fetch_space_factor(toks_allowed, scanner, eqtb, logger),
        InternalCommand::PrevDepth => fetch_prev_depth(toks_allowed, scanner, eqtb, logger),
        InternalCommand::PrevGraf => fetch_prev_graf(scanner, eqtb),
        InternalCommand::PageDimen(page_dimension) => {
            fetch_something_on_page_so_far(page_dimension, eqtb)
        }
        InternalCommand::DeadCycles => InternalValue::Int(eqtb.dead_cycles),
        InternalCommand::InsertPenalties => InternalValue::Int(eqtb.insert_penalties),
        InternalCommand::BoxDimen(box_dimension) => {
            fetch_box_dimension(box_dimension, scanner, eqtb, logger)
        }
        InternalCommand::ParShape => fetch_par_shape_size(eqtb),
        InternalCommand::CatCode => fetch_category_code(scanner, eqtb, logger),
        InternalCommand::Code(code) => fetch_character_code(code, scanner, eqtb, logger),
        InternalCommand::Register(value_type) => fetch_register(value_type, scanner, eqtb, logger),
    };
    value
}

/// See 414.
fn fetch_category_code(
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> InternalValue {
    let chr = scanner.scan_char_num(eqtb, logger);
    let cat_code = *eqtb.cat_codes.get(chr);
    InternalValue::Int(cat_code as i32)
}

/// See 414.
fn fetch_character_code(
    code: CodeType,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> InternalValue {
    let n = scanner.scan_char_num(eqtb, logger) as usize;
    let code_var = code.to_variable(n);
    let value = *eqtb.codes.get(code_var);
    InternalValue::Int(value)
}

/// See 415.
fn fetch_token_list_or_font_identifier(
    toks_command: ToksCommand,
    toks_allowed: bool,
    token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> InternalValue {
    if toks_allowed {
        match toks_command {
            ToksCommand::TokenListRegister => {
                let register = scanner.scan_register_index(eqtb, logger);
                let toks_var = TokenListVariable::Toks(register);
                if let Some(list) = eqtb.token_lists.get(toks_var) {
                    // Clone the underlying vector
                    InternalValue::TokenList((**list).clone())
                } else {
                    InternalValue::TokenList(Vec::new())
                }
            }
            ToksCommand::TokenList(toks_var) => {
                if let Some(list) = eqtb.token_lists.get(toks_var) {
                    // Clone the underlying vector
                    InternalValue::TokenList((**list).clone())
                } else {
                    InternalValue::TokenList(Vec::new())
                }
            }
            ToksCommand::DefFamily(_) | ToksCommand::DefFont | ToksCommand::SetFont(_) => {
                scanner.back_input(token, eqtb, logger);
                let font_index = scan_font_ident(scanner, eqtb, logger);
                InternalValue::Ident(font_index)
            }
        }
    } else {
        logger.print_err("Missing number, treated as zero");
        let help = &[
            "A number should have been here; I inserted `0'.",
            "(If you can't figure out why I needed to see a number,",
            "look up `weird error' in the index to The TeXbook.)",
        ];
        scanner.back_error(token, help, eqtb, logger);
        InternalValue::Dimen(0)
    }
}

/// See 418.
fn fetch_space_factor(
    toks_allowed: bool,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> InternalValue {
    match eqtb.mode().base() {
        Mode::Horizontal if !scanner.scanning_write_tokens => {
            InternalValue::Int(eqtb.space_factor as i32)
        }
        _ => {
            logger.print_err("Improper ");
            logger.print_esc_str(b"spacefactor");
            let help = &[
                "You can refer to \\spacefactor only in horizontal mode;",
                "you can refer to \\prevdepth only in vertical mode; and",
                "neither of these is meaningful inside \\write. So",
                "I'm forgetting what you said and using zero instead.",
            ];
            logger.error(help, scanner, eqtb);
            if !toks_allowed {
                InternalValue::Dimen(0)
            } else {
                InternalValue::Int(0)
            }
        }
    }
}

/// See 418.
fn fetch_prev_depth(
    toks_allowed: bool,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> InternalValue {
    if let Mode::Vertical = eqtb.mode().base() {
        InternalValue::Dimen(eqtb.prev_depth)
    } else {
        logger.print_err("Improper ");
        logger.print_esc_str(b"prevdepth");
        let help = &[
            "You can refer to \\spacefactor only in horizontal mode;",
            "you can refer to \\prevdepth only in vertical mode; and",
            "neither of these is meaningful inside \\write. So",
            "I'm forgetting what you said and using zero instead.",
        ];
        logger.error(help, scanner, eqtb);
        if !toks_allowed {
            InternalValue::Dimen(0)
        } else {
            InternalValue::Int(0)
        }
    }
}

/// See 420.
fn fetch_box_dimension(
    box_dimension: BoxDimension,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> InternalValue {
    let register = scanner.scan_register_index(eqtb, logger);
    let boks = eqtb.boks(register);
    let value = match boks {
        None => 0,
        Some(list_node) => match box_dimension {
            BoxDimension::Width => list_node.width,
            BoxDimension::Height => list_node.height,
            BoxDimension::Depth => list_node.depth,
        },
    };
    InternalValue::Dimen(value)
}

/// See 421.
fn fetch_something_on_page_so_far(page_dimension: PageDimension, eqtb: &Eqtb) -> InternalValue {
    let value = if eqtb.page_contents == PageContents::Empty && !eqtb.output_active {
        if let PageDimension::PageGoal = page_dimension {
            MAX_DIMEN
        } else {
            0
        }
    } else {
        match page_dimension {
            PageDimension::PageGoal => eqtb.page_dims.page_goal,
            PageDimension::Height => eqtb.page_dims.height,
            PageDimension::Stretch => eqtb.page_dims.stretch.normal,
            PageDimension::FilStretch => eqtb.page_dims.stretch.fil,
            PageDimension::FillStretch => eqtb.page_dims.stretch.fill,
            PageDimension::FilllStretch => eqtb.page_dims.stretch.filll,
            PageDimension::Shrink => eqtb.page_dims.shrink,
            PageDimension::Depth => eqtb.page_dims.depth,
        }
    };
    InternalValue::Dimen(value)
}

/// See 422.
fn fetch_prev_graf(scanner: &Scanner, eqtb: &Eqtb) -> InternalValue {
    if scanner.scanning_write_tokens {
        InternalValue::Int(0)
    } else {
        InternalValue::Int(eqtb.prev_graf)
    }
}

/// See 423.
fn fetch_par_shape_size(eqtb: &Eqtb) -> InternalValue {
    let par_shape = eqtb.par_shape.get(ParShapeVariable);
    InternalValue::Int(par_shape.len() as i32)
}

/// See 424.
fn fetch_last_penalty(scanner: &Scanner, eqtb: &Eqtb) -> InternalValue {
    if scanner.scanning_write_tokens {
        InternalValue::Int(0)
    } else if let LastNodeInfo::Penalty(penalty) = eqtb.last_node_info {
        InternalValue::Int(penalty)
    } else {
        InternalValue::Int(0)
    }
}

/// See 424.
fn fetch_last_kern(scanner: &Scanner, eqtb: &Eqtb) -> InternalValue {
    if scanner.scanning_write_tokens {
        InternalValue::Dimen(0)
    } else if let LastNodeInfo::Kern(dimen) = eqtb.last_node_info {
        InternalValue::Dimen(dimen)
    } else {
        InternalValue::Dimen(0)
    }
}

/// See 424.
fn fetch_last_skip(scanner: &Scanner, eqtb: &Eqtb) -> InternalValue {
    if scanner.scanning_write_tokens {
        InternalValue::Glue(GlueSpec::zero_glue())
    } else {
        match &eqtb.last_node_info {
            LastNodeInfo::Glue(glue_spec) => InternalValue::Glue(glue_spec.clone()),
            LastNodeInfo::MuGlue(glue_spec) => InternalValue::MuGlue(glue_spec.clone()),
            _ => InternalValue::Glue(GlueSpec::zero_glue()),
        }
    }
}

/// See 425.
fn fetch_font_dimension(
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> InternalValue {
    match find_font_dimen(false, scanner, eqtb, logger) {
        Some((font_index, param_index)) => {
            InternalValue::Dimen(eqtb.fonts[font_index as usize].params[param_index])
        }
        None => InternalValue::Dimen(0),
    }
}

/// See 427.
fn fetch_register(
    value_type: ValueType,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> InternalValue {
    let register = scanner.scan_register_index(eqtb, logger);
    match value_type {
        ValueType::Int => InternalValue::Int(eqtb.integer(IntegerVariable::Count(register))),
        ValueType::Dimen => InternalValue::Dimen(eqtb.dimen(DimensionVariable::Dimen(register))),
        ValueType::Glue => {
            InternalValue::Glue(eqtb.skips.get(SkipVariable::Skip(register)).clone())
        }
        ValueType::Mu => {
            InternalValue::MuGlue(eqtb.skips.get(SkipVariable::MuSkip(register)).clone())
        }
    }
}

impl Dumpable for ValueType {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::Int => writeln!(target, "Int")?,
            Self::Dimen => writeln!(target, "Dimen")?,
            Self::Glue => writeln!(target, "Glue")?,
            Self::Mu => writeln!(target, "Mu")?,
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "Int" => Ok(Self::Int),
            "Dimen" => Ok(Self::Dimen),
            "Glue" => Ok(Self::Glue),
            "Mu" => Ok(Self::Mu),
            _ => Err(FormatError::ParseError),
        }
    }
}
