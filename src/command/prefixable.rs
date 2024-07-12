use super::arithmetic::ArithCommand;
use super::box_dimension::BoxDimension;
use super::def::DefCommand;
use super::page_dimension::PageDimension;
use super::prefix::Prefix;
use super::shorthand_def::ShorthandDef;
use super::{
    Command, ExpandableCommand, InternalCommand, MacroCall, MathCommand, ToksCommand,
    UnexpandableCommand,
};

use crate::box_building::BoxContext;
use crate::dimension::scan_normal_dimen;
use crate::eqtb::{
    CatCode, CodeType, CodeVariable, ControlSequence, DimensionVariable, Eqtb, FontIndex,
    FontVariable, IntegerVariable, MathFontSize, ParagraphShape, SkipVariable, TokenListVariable,
    NULL_FONT,
};
use crate::fonts::FontInfo;
use crate::fonts::{find_font_dimen, scan_font_ident, SizeIndicator, TfmError};
use crate::format::{Dumpable, FormatError};
use crate::glue::scan_glue;
use crate::hyphenation::Hyphenator;
use crate::input::Scanner;
use crate::input_streams::read_toks;
use crate::integer::{Integer, IntegerExt};
use crate::logger::InteractionMode;
use crate::logger::Logger;
use crate::main_control::report_illegal_case;
use crate::output::Output;
use crate::page_breaking::PageBuilder;
use crate::print::string::StringPrinter;
use crate::print::Printer;
use crate::scaled::{xn_over_d, Scaled, UNITY};
use crate::scan_boxes::scan_box;
use crate::scan_internal::ValueType;
use crate::semantic_nest::{RichMode, SemanticState};
use crate::token::Token;
use crate::INIT;

use std::io::Write;
use std::path::{Path, PathBuf};
use std::rc::Rc;

const MAX_CAT_CODE: i32 = 15;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrefixableCommand {
    TokenListRegister,
    TokenList(TokenListVariable),
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
    DefFamily(MathFontSize),
    SetFont(FontIndex),
    DefFont,
    Register(ValueType),
    Arith(ArithCommand),
    Prefix(Prefix),
    Let,
    FutureLet,
    ShorthandDef(ShorthandDef),
    ReadToCs,
    Def(DefCommand),
    SetBox,
    Hyphenation,
    Patterns,
    SetInteraction(InteractionMode),
}

impl PrefixableCommand {
    /// If the command is "internal", return the corresponding InternalCommand.
    pub fn try_to_internal(&self) -> Option<InternalCommand> {
        match self {
            &Self::TokenListRegister => Some(InternalCommand::Toks(ToksCommand::TokenListRegister)),
            &Self::TokenList(toks_var) => {
                Some(InternalCommand::Toks(ToksCommand::TokenList(toks_var)))
            }
            &Self::Integer(int_var) => Some(InternalCommand::Integer(int_var)),
            &Self::Dimension(dim_var) => Some(InternalCommand::Dimension(dim_var)),
            &Self::Glue(glue_var) => Some(InternalCommand::Glue(glue_var)),
            &Self::MuGlue(mu_glue_var) => Some(InternalCommand::MuGlue(mu_glue_var)),
            &Self::FontDimen => Some(InternalCommand::FontDimen),
            &Self::HyphenChar => Some(InternalCommand::HyphenChar),
            &Self::SkewChar => Some(InternalCommand::SkewChar),
            &Self::SpaceFactor => Some(InternalCommand::SpaceFactor),
            &Self::PrevDepth => Some(InternalCommand::PrevDepth),
            &Self::PrevGraf => Some(InternalCommand::PrevGraf),
            &Self::PageDimen(page_dimension) => Some(InternalCommand::PageDimen(page_dimension)),
            &Self::DeadCycles => Some(InternalCommand::DeadCycles),
            &Self::InsertPenalties => Some(InternalCommand::InsertPenalties),
            &Self::BoxDimen(box_dimension) => Some(InternalCommand::BoxDimen(box_dimension)),
            &Self::ParShape => Some(InternalCommand::ParShape),
            &Self::CatCode => Some(InternalCommand::CatCode),
            &Self::Code(code) => Some(InternalCommand::Code(code)),
            &Self::DefFamily(math_font_size) => Some(InternalCommand::Toks(
                ToksCommand::DefFamily(math_font_size),
            )),
            &Self::SetFont(font_index) => {
                Some(InternalCommand::Toks(ToksCommand::SetFont(font_index)))
            }
            &Self::DefFont => Some(InternalCommand::Toks(ToksCommand::DefFont)),
            &Self::Register(value_type) => Some(InternalCommand::Register(value_type)),
            Self::Arith(_)
            | Self::Prefix(_)
            | Self::Let
            | Self::FutureLet
            | Self::ShorthandDef(_)
            | Self::ReadToCs
            | Self::Def(_)
            | Self::SetBox
            | Self::Hyphenation
            | Self::Patterns
            | Self::SetInteraction(_) => None,
        }
    }

    /// See 298.
    pub fn display(&self, fonts: &Vec<FontInfo>, printer: &mut impl Printer) {
        match self {
            Self::TokenListRegister => printer.print_esc_str(b"toks"),
            Self::TokenList(toks_var) => printer.print_esc_str(&toks_var.to_string()),
            Self::Integer(int_var) => printer.print_esc_str(&int_var.to_string()),
            Self::Dimension(dim_var) => printer.print_esc_str(&dim_var.to_string()),
            Self::Glue(glue_var) => printer.print_esc_str(&glue_var.to_string()),
            Self::MuGlue(mu_glue_var) => printer.print_esc_str(&mu_glue_var.to_string()),
            Self::FontDimen => printer.print_esc_str(b"fontdimen"),
            Self::HyphenChar => printer.print_esc_str(b"hyphenchar"),
            Self::SkewChar => printer.print_esc_str(b"skewchar"),
            Self::SpaceFactor => printer.print_esc_str(b"spacefactor"),
            Self::PrevDepth => printer.print_esc_str(b"prevdepth"),
            Self::PrevGraf => printer.print_esc_str(b"prevgraf"),
            Self::PageDimen(page_dimension) => page_dimension.display(printer),
            Self::DeadCycles => printer.print_esc_str(b"deadcycles"),
            Self::InsertPenalties => printer.print_esc_str(b"insertpenalties"),
            Self::BoxDimen(box_dimension) => box_dimension.display(printer),
            Self::ParShape => printer.print_esc_str(b"parshape"),
            Self::CatCode => printer.print_esc_str(b"catcode"),
            Self::Code(code) => printer.print_esc_str(code.as_str()),
            &Self::DefFamily(math_font_size) => {
                printer.print_esc_str(math_font_size.as_str().as_bytes())
            }
            &Self::SetFont(font_index) => print_font(&fonts[font_index as usize], printer),
            Self::DefFont => printer.print_esc_str(b"font"),
            Self::Register(value_type) => match value_type {
                ValueType::Int => printer.print_esc_str(b"count"),
                ValueType::Dimen => printer.print_esc_str(b"dimen"),
                ValueType::Glue => printer.print_esc_str(b"skip"),
                ValueType::Mu => printer.print_esc_str(b"muskip"),
            },
            Self::Arith(arith_command) => arith_command.display(printer),
            Self::Prefix(prefix) => prefix.display(printer),
            Self::Let => printer.print_esc_str(b"let"),
            Self::FutureLet => printer.print_esc_str(b"futurelet"),
            Self::ShorthandDef(shorthand_def) => shorthand_def.display(printer),
            Self::ReadToCs => printer.print_esc_str(b"read"),
            Self::Def(def_command) => def_command.display(printer),
            Self::SetBox => printer.print_esc_str(b"setbox"),
            Self::Hyphenation => printer.print_esc_str(b"hyphenation"),
            Self::Patterns => printer.print_esc_str(b"patterns"),
            Self::SetInteraction(interaction) => printer.print_esc_str(interaction.as_str()),
        }
    }
}

/// See 1261.
fn print_font(font: &FontInfo, printer: &mut impl Printer) {
    printer.print_str("select font ");
    printer.slow_print_str(&font.name);
    if font.size != font.dsize {
        printer.print_str(" at ");
        printer.print_scaled(font.size);
        printer.print_str("pt");
    }
}

impl Dumpable for PrefixableCommand {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::TokenListRegister => writeln!(target, "TokenListRegister")?,
            Self::TokenList(toks_var) => {
                writeln!(target, "TokenList")?;
                toks_var.dump(target)?;
            }
            Self::Integer(int_var) => {
                writeln!(target, "Integer")?;
                int_var.dump(target)?;
            }
            Self::Dimension(dim_var) => {
                writeln!(target, "Dimension")?;
                dim_var.dump(target)?;
            }
            Self::Glue(glue_var) => {
                writeln!(target, "Glue")?;
                glue_var.dump(target)?;
            }
            Self::MuGlue(mu_glue_var) => {
                writeln!(target, "MuGlue")?;
                mu_glue_var.dump(target)?;
            }
            Self::FontDimen => writeln!(target, "FontDimen")?,
            Self::HyphenChar => writeln!(target, "HyphenChar")?,
            Self::SkewChar => writeln!(target, "SkewChar")?,
            Self::SpaceFactor => writeln!(target, "SpaceFactor")?,
            Self::PrevDepth => writeln!(target, "PrevDepth")?,
            Self::PrevGraf => writeln!(target, "PrevGraf")?,
            Self::PageDimen(page_dimension) => {
                writeln!(target, "PageDimen")?;
                page_dimension.dump(target)?;
            }
            Self::DeadCycles => writeln!(target, "DeadCycles")?,
            Self::InsertPenalties => writeln!(target, "InsertPenalties")?,
            Self::BoxDimen(box_dimension) => {
                writeln!(target, "BoxDimen")?;
                box_dimension.dump(target)?;
            }
            Self::ParShape => writeln!(target, "ParShape")?,
            Self::CatCode => writeln!(target, "CatCode")?,
            Self::Code(code) => {
                writeln!(target, "Code")?;
                code.dump(target)?;
            }
            Self::DefFamily(math_font_size) => {
                writeln!(target, "DefFamily")?;
                math_font_size.dump(target)?;
            }
            Self::SetFont(font_index) => {
                writeln!(target, "SetFont")?;
                font_index.dump(target)?;
            }
            Self::DefFont => writeln!(target, "DefFont")?,
            Self::Register(value_type) => {
                writeln!(target, "Register")?;
                value_type.dump(target)?;
            }
            Self::Arith(arith_command) => {
                writeln!(target, "Arith")?;
                arith_command.dump(target)?;
            }
            Self::Prefix(prefix) => {
                writeln!(target, "Prefix")?;
                prefix.dump(target)?;
            }
            Self::Let => {
                writeln!(target, "Let")?;
            }
            Self::FutureLet => {
                writeln!(target, "FutureLet")?;
            }
            Self::ShorthandDef(shorthand_def) => {
                writeln!(target, "ShorthandDef")?;
                shorthand_def.dump(target)?;
            }
            Self::ReadToCs => {
                writeln!(target, "ReadToCs")?;
            }
            Self::Def(def_command) => {
                writeln!(target, "Def")?;
                def_command.dump(target)?;
            }
            Self::SetBox => {
                writeln!(target, "SetBox")?;
            }
            Self::Hyphenation => {
                writeln!(target, "Hyphenation")?;
            }
            Self::Patterns => {
                writeln!(target, "Patterns")?;
            }
            Self::SetInteraction(interaction) => {
                writeln!(target, "SetInteraction")?;
                interaction.dump(target)?;
            }
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "TokenListRegister" => Ok(Self::TokenListRegister),
            "TokenList" => {
                let toks_var = TokenListVariable::undump(lines)?;
                Ok(Self::TokenList(toks_var))
            }
            "Integer" => {
                let int_var = IntegerVariable::undump(lines)?;
                Ok(Self::Integer(int_var))
            }
            "Dimension" => {
                let dim_var = DimensionVariable::undump(lines)?;
                Ok(Self::Dimension(dim_var))
            }
            "Glue" => {
                let glue_var = SkipVariable::undump(lines)?;
                Ok(Self::Glue(glue_var))
            }
            "MuGlue" => {
                let mu_glue_var = SkipVariable::undump(lines)?;
                Ok(Self::MuGlue(mu_glue_var))
            }
            "FontDimen" => Ok(Self::FontDimen),
            "HyphenChar" => Ok(Self::HyphenChar),
            "SkewChar" => Ok(Self::SkewChar),
            "SpaceFactor" => Ok(Self::SpaceFactor),
            "PrevDepth" => Ok(Self::PrevDepth),
            "PrevGraf" => Ok(Self::PrevGraf),
            "PageDimen" => {
                let page_dimension = PageDimension::undump(lines)?;
                Ok(Self::PageDimen(page_dimension))
            }
            "DeadCycles" => Ok(Self::DeadCycles),
            "InsertPenalties" => Ok(Self::InsertPenalties),
            "BoxDimen" => {
                let box_dimension = BoxDimension::undump(lines)?;
                Ok(Self::BoxDimen(box_dimension))
            }
            "ParShape" => Ok(Self::ParShape),
            "CatCode" => Ok(Self::CatCode),
            "Code" => {
                let code = CodeType::undump(lines)?;
                Ok(Self::Code(code))
            }
            "DefFamily" => {
                let math_font_size = MathFontSize::undump(lines)?;
                Ok(Self::DefFamily(math_font_size))
            }
            "SetFont" => {
                let font_index = FontIndex::undump(lines)?;
                Ok(Self::SetFont(font_index))
            }
            "DefFont" => Ok(Self::DefFont),
            "Register" => {
                let value_type = ValueType::undump(lines)?;
                Ok(Self::Register(value_type))
            }
            "Arith" => {
                let arith_command = ArithCommand::undump(lines)?;
                Ok(Self::Arith(arith_command))
            }
            "Prefix" => {
                let prefix = Prefix::undump(lines)?;
                Ok(Self::Prefix(prefix))
            }
            "Let" => Ok(Self::Let),
            "FutureLet" => Ok(Self::FutureLet),
            "ShorthandDef" => {
                let shorthand_def = ShorthandDef::undump(lines)?;
                Ok(Self::ShorthandDef(shorthand_def))
            }
            "ReadToCs" => Ok(Self::ReadToCs),
            "Def" => {
                let def_command = DefCommand::undump(lines)?;
                Ok(Self::Def(def_command))
            }
            "SetBox" => Ok(Self::SetBox),
            "Hyphenation" => Ok(Self::Hyphenation),
            "Patterns" => Ok(Self::Patterns),
            "SetInteraction" => {
                let interaction = InteractionMode::undump(lines)?;
                Ok(Self::SetInteraction(interaction))
            }
            _ => Err(FormatError::ParseError),
        }
    }
}

/// See 1211.
pub fn prefixed_command(
    mut prefixable_command: PrefixableCommand,
    token: Token,
    hyphenator: &mut Hyphenator,
    page_builder: &mut PageBuilder,
    output: &mut Output,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let mut long = false;
    let mut outer = false;
    let mut global = false;
    while let PrefixableCommand::Prefix(prefix) = prefixable_command {
        match prefix {
            Prefix::Long => {
                long = true;
            }
            Prefix::Outer => {
                outer = true;
            }
            Prefix::Global => {
                global = true;
            }
        }
        let (unexpandable_command, token) =
            scanner.get_next_non_blank_non_relax_non_call_token(eqtb, logger);
        match unexpandable_command {
            UnexpandableCommand::Prefixable(command) => prefixable_command = command,
            _ => {
                discard_erroneous_prefixes(unexpandable_command, token, scanner, eqtb, logger);
                return;
            }
        }
    }
    discard_prefixes_long_and_outer_if_irrelevant(
        prefixable_command,
        long,
        outer,
        scanner,
        eqtb,
        logger,
    );
    adjust_for_setting_of_globaldefs(&mut global, eqtb);
    match prefixable_command {
        PrefixableCommand::TokenListRegister => {
            let register = scanner.scan_register_index(eqtb, logger);
            let toks_var = TokenListVariable::Toks(register);
            assign_to_token_list(toks_var, token, global, scanner, eqtb, logger);
        }
        PrefixableCommand::TokenList(toks_var) => {
            assign_to_token_list(toks_var, token, global, scanner, eqtb, logger)
        }
        PrefixableCommand::Integer(int_var) => {
            scanner.scan_optional_equals(eqtb, logger);
            let value = Integer::scan_int(scanner, eqtb, logger);
            eqtb.int_define(int_var, value, global, logger);
        }
        PrefixableCommand::Dimension(dim_var) => {
            scanner.scan_optional_equals(eqtb, logger);
            let dimen = scan_normal_dimen(scanner, eqtb, logger);
            eqtb.dimen_define(dim_var, dimen, global);
        }
        PrefixableCommand::Glue(glue_var) => {
            scanner.scan_optional_equals(eqtb, logger);
            let skip = scan_glue(false, scanner, eqtb, logger);
            eqtb.skip_define(glue_var, skip, global);
        }
        PrefixableCommand::MuGlue(mu_glue_var) => {
            scanner.scan_optional_equals(eqtb, logger);
            let skip = scan_glue(true, scanner, eqtb, logger);
            eqtb.skip_define(mu_glue_var, skip, global);
        }
        PrefixableCommand::FontDimen => {
            match find_font_dimen(true, scanner, eqtb, logger) {
                Some((font_index, param_index)) => {
                    scanner.scan_optional_equals(eqtb, logger);
                    let val = scan_normal_dimen(scanner, eqtb, logger);
                    eqtb.fonts[font_index as usize].params[param_index] = val;
                }
                None => {
                    // Scan the rest but ignore it.
                    scanner.scan_optional_equals(eqtb, logger);
                    let _ = scan_normal_dimen(scanner, eqtb, logger);
                }
            }
        }
        PrefixableCommand::HyphenChar => {
            let font_index = scan_font_ident(scanner, eqtb, logger);
            scanner.scan_optional_equals(eqtb, logger);
            let value = Integer::scan_int(scanner, eqtb, logger);
            eqtb.fonts[font_index as usize].hyphen_char = value;
        }
        PrefixableCommand::SkewChar => {
            let font_index = scan_font_ident(scanner, eqtb, logger);
            scanner.scan_optional_equals(eqtb, logger);
            let value = Integer::scan_int(scanner, eqtb, logger);
            eqtb.fonts[font_index as usize].skew_char = value;
        }
        PrefixableCommand::SpaceFactor => alter_space_factor(nest, scanner, eqtb, logger),
        PrefixableCommand::PrevDepth => alter_prev_depth(nest, scanner, eqtb, logger),
        PrefixableCommand::PageDimen(page_dimension) => {
            page_dimension.alter_page_so_far(scanner, eqtb, logger)
        }
        PrefixableCommand::PrevGraf => alter_prev_graf(nest, scanner, eqtb, logger),
        PrefixableCommand::DeadCycles => alter_dead_cycles(scanner, eqtb, logger),
        PrefixableCommand::InsertPenalties => alter_insert_penalties(scanner, eqtb, logger),
        PrefixableCommand::BoxDimen(box_dimen) => box_dimen.alter_box_dimen(scanner, eqtb, logger),
        PrefixableCommand::ParShape => {
            scanner.scan_optional_equals(eqtb, logger);
            let mut shape = ParagraphShape::new();
            let line_count = Integer::scan_int(scanner, eqtb, logger);

            for _ in 0..line_count {
                let indent = scan_normal_dimen(scanner, eqtb, logger);
                let line_length = scan_normal_dimen(scanner, eqtb, logger);
                shape.push((indent, line_length));
            }
            eqtb.par_shape_define(shape, global);
        }
        PrefixableCommand::CatCode => {
            let chr = scanner.scan_char_num(eqtb, logger);
            scanner.scan_optional_equals(eqtb, logger);
            let value = Integer::scan_int(scanner, eqtb, logger);
            let cat_code = match CatCode::try_from(value) {
                Ok(cat_code) => cat_code,
                Err(_) => {
                    logger.print_err("Invalid code (");
                    logger.print_int(value);
                    logger.print_str("), should be in the range 0..");
                    logger.print_int(MAX_CAT_CODE);
                    let help = &["I'm going to use 0 instead of that illegal code value."];
                    logger.error(help, scanner, eqtb);
                    CatCode::Escape
                }
            };
            eqtb.cat_code_define(chr, cat_code, global);
        }
        PrefixableCommand::Code(code) => {
            let n = scanner.scan_char_num(eqtb, logger) as usize;
            let code_var = code.to_variable(n);
            let n = largest_legal_code_value(code);
            scanner.scan_optional_equals(eqtb, logger);
            let mut value = Integer::scan_int(scanner, eqtb, logger);
            if let CodeVariable::DelCode(_) = code_var {
                if value > n {
                    logger.print_err("Invalid code (");
                    logger.print_int(value);
                    logger.print_str("), should be at most ");
                    logger.print_int(n);
                    let help = &["I'm going to use 0 instead of that illegal code value."];
                    logger.error(help, scanner, eqtb);
                    value = 0;
                }
            } else if value < 0 || value > n {
                logger.print_err("Invalid code (");
                logger.print_int(value);
                logger.print_str("), should be in the range 0..");
                logger.print_int(n);
                let help = &["I'm going to use 0 instead of that illegal code value."];
                logger.error(help, scanner, eqtb);
                value = 0;
            }
            eqtb.code_define(code_var, value, global);
        }
        PrefixableCommand::DefFamily(size) => {
            let number = Integer::scan_four_bit_int(scanner, eqtb, logger) as usize;
            scanner.scan_optional_equals(eqtb, logger);
            let f = scan_font_ident(scanner, eqtb, logger);
            eqtb.font_define(FontVariable::MathFont { size, number }, f, global);
        }
        PrefixableCommand::SetFont(font_index) => {
            eqtb.font_define(FontVariable::CurFont, font_index, global)
        }
        PrefixableCommand::DefFont => new_font(global, scanner, eqtb, logger),
        PrefixableCommand::Register(value_type) => {
            store_in_register(value_type, global, scanner, eqtb, logger)
        }
        PrefixableCommand::Arith(arith_command) => {
            arith_command.do_register_command(global, scanner, eqtb, logger)
        }
        PrefixableCommand::Prefix(_) => panic!("Prefixes should have been filtered out further up"),
        // From 1221.
        PrefixableCommand::Let => {
            let cs = get_r_token(scanner, eqtb, logger);
            let (mut command, mut token);
            loop {
                (command, token) = scanner.get_command_and_token(eqtb, logger);
                if let Command::Unexpandable(UnexpandableCommand::Spacer) = command {
                    // Consume the spacer.
                } else {
                    break;
                }
            }
            // We allow an optional '=' plus a single optional spacer.
            if token == Token::OtherChar(b'=') {
                (command, _) = scanner.get_command_and_token(eqtb, logger);
                if let Command::Unexpandable(UnexpandableCommand::Spacer) = command {
                    (command, _) = scanner.get_command_and_token(eqtb, logger);
                }
            }
            eqtb.cs_define(cs, command, global);
        }
        // From 1221.
        PrefixableCommand::FutureLet => {
            let cs = get_r_token(scanner, eqtb, logger);
            let first_token = scanner.get_token(eqtb, logger);
            let (command, second_token) = scanner.get_command_and_token(eqtb, logger);
            scanner.back_input(second_token, eqtb, logger);
            scanner.back_input(first_token, eqtb, logger);
            eqtb.cs_define(cs, command, global);
        }
        // From 1224.
        PrefixableCommand::ShorthandDef(shorthand_def) => {
            let cs = get_r_token(scanner, eqtb, logger);
            let cmd = Command::Unexpandable(UnexpandableCommand::Relax { no_expand: false });
            eqtb.cs_define(cs, cmd, global);
            scanner.scan_optional_equals(eqtb, logger);
            match shorthand_def {
                ShorthandDef::CharDef => {
                    let c = scanner.scan_char_num(eqtb, logger);
                    let char_given = Command::Unexpandable(UnexpandableCommand::CharGiven(c));
                    eqtb.cs_define(cs, char_given, global);
                }
                ShorthandDef::MathCharDef => {
                    let char_code = Integer::scan_fifteen_bit_int(scanner, eqtb, logger) as u16;
                    let cmd = Command::Unexpandable(UnexpandableCommand::Math(
                        MathCommand::MathCharGiven(char_code),
                    ));
                    eqtb.cs_define(cs, cmd, global);
                }
                _ => {
                    let register = scanner.scan_register_index(eqtb, logger);
                    match shorthand_def {
                        ShorthandDef::CountDef => eqtb.cs_define(
                            cs,
                            Command::Unexpandable(UnexpandableCommand::Prefixable(
                                PrefixableCommand::Integer(IntegerVariable::Count(register)),
                            )),
                            global,
                        ),
                        ShorthandDef::DimenDef => eqtb.cs_define(
                            cs,
                            Command::Unexpandable(UnexpandableCommand::Prefixable(
                                PrefixableCommand::Dimension(DimensionVariable::Dimen(register)),
                            )),
                            global,
                        ),
                        ShorthandDef::SkipDef => eqtb.cs_define(
                            cs,
                            Command::Unexpandable(UnexpandableCommand::Prefixable(
                                PrefixableCommand::Glue(SkipVariable::Skip(register)),
                            )),
                            global,
                        ),
                        ShorthandDef::MuSkipDef => eqtb.cs_define(
                            cs,
                            Command::Unexpandable(UnexpandableCommand::Prefixable(
                                PrefixableCommand::MuGlue(SkipVariable::MuSkip(register)),
                            )),
                            global,
                        ),
                        ShorthandDef::ToksDef => eqtb.cs_define(
                            cs,
                            Command::Unexpandable(UnexpandableCommand::Prefixable(
                                PrefixableCommand::TokenList(TokenListVariable::Toks(register)),
                            )),
                            global,
                        ),
                        _ => panic!("Impossible"),
                    }
                }
            }
        }
        // From 1225.
        PrefixableCommand::ReadToCs => {
            let stream_number = Integer::scan_int(scanner, eqtb, logger);
            if !scanner.scan_keyword(b"to", eqtb, logger) {
                logger.print_err("Missing `to' inserted");
                let help = &[
                    "You should have said `\\read<number> to \\cs'.",
                    "I'm going to look for the \\cs now.",
                ];
                logger.error(help, scanner, eqtb);
            }
            let cs = get_r_token(scanner, eqtb, logger);
            let macro_def = read_toks(stream_number, cs, scanner, eqtb, logger);
            let macro_call = MacroCall {
                long: false,
                outer: false,
                macro_def: Rc::new(macro_def),
            };
            let command = Command::Expandable(ExpandableCommand::Macro(macro_call));
            eqtb.cs_define(cs, command, global);
        }
        // From 1218.
        PrefixableCommand::Def(def_command) => {
            if def_command.global && eqtb.integer(IntegerVariable::GlobalDefs) >= 0 {
                global = true;
            }
            let cs = get_r_token(scanner, eqtb, logger);
            let macro_def = scanner.scan_macro_definition(cs, def_command.expand, eqtb, logger);
            let macro_call = MacroCall {
                long,
                outer,
                macro_def: Rc::new(macro_def),
            };
            let command = Command::Expandable(ExpandableCommand::Macro(macro_call));
            eqtb.cs_define(cs, command, global);
        }
        // From 1241.
        PrefixableCommand::SetBox => {
            let box_number = scanner.scan_register_index(eqtb, logger);
            scanner.scan_optional_equals(eqtb, logger);
            if logger.set_box_allowed {
                scan_box(
                    BoxContext::SetBox { box_number, global },
                    page_builder,
                    output,
                    nest,
                    scanner,
                    eqtb,
                    logger,
                );
            } else {
                logger.print_err("Improper ");
                logger.print_esc_str(b"setbox");
                let help = &[
                    "Sorry, \\setbox is not allowed after \\halign in a display,",
                    "or between \\accent and an accented character.",
                ];
                logger.error(help, scanner, eqtb);
            }
        }
        PrefixableCommand::Hyphenation => {
            hyphenator.new_hyph_exceptions(scanner, eqtb, logger);
        }
        // From 1252.
        PrefixableCommand::Patterns => {
            if INIT {
                hyphenator.new_patterns(token, scanner, eqtb, logger);
            } else {
                logger.print_err("Patterns can be loaded only by INITEX");
                // See 89.
                let help = &[
                    "Sorry, I don't know how to help in this situation.",
                    "Maybe you should try asking a human?",
                ];
                logger.error(help, scanner, eqtb);
                loop {
                    let (command, _) = scanner.get_command_and_token(eqtb, logger);
                    if let Command::Unexpandable(UnexpandableCommand::RightBrace(_)) = command {
                        break;
                    }
                }
                return;
            }
        }
        PrefixableCommand::SetInteraction(interaction) => logger.new_interaction(interaction),
    }
    scanner.insert_token_saved_by_afterassignment_if_any(eqtb, logger);
}

/// See 1212.
fn discard_erroneous_prefixes(
    unexpandable_command: UnexpandableCommand,
    token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    logger.print_err("You can't use a prefix with `");
    unexpandable_command.display(&eqtb.fonts, logger);
    logger.print_char(b'\'');
    let help = &["I'll pretend you didn't say \\long or \\outer or \\global."];
    scanner.back_error(token, help, eqtb, logger)
}

/// See 1213.
fn discard_prefixes_long_and_outer_if_irrelevant(
    prefixable_command: PrefixableCommand,
    long: bool,
    outer: bool,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    match prefixable_command {
        PrefixableCommand::Def(_) => {}
        _ => {
            if long || outer {
                logger.print_err("You can't use `");
                logger.print_esc_str(b"long");
                logger.print_str("' or `");
                logger.print_esc_str(b"outer");
                logger.print_str("' with `");
                prefixable_command.display(&eqtb.fonts, logger);
                logger.print_char(b'\'');
                let help = &["I'll pretend you didn't say \\long or \\outer here."];
                logger.error(help, scanner, eqtb);
            }
        }
    }
}

/// See 1214.
fn adjust_for_setting_of_globaldefs(global: &mut bool, eqtb: &Eqtb) {
    if eqtb.integer(IntegerVariable::GlobalDefs) != 0 {
        *global = eqtb.integer(IntegerVariable::GlobalDefs) >= 0;
    }
}

/// See 1226. and 1227.
fn assign_to_token_list(
    lhs: TokenListVariable,
    token: Token,
    global: bool,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let Token::CSToken { cs } = token else {
        panic!("Impossible")
    };
    scanner.scan_optional_equals(eqtb, logger);
    let (unexpandable_command, token) =
        scanner.get_next_non_blank_non_relax_non_call_token(eqtb, logger);

    match unexpandable_command {
        UnexpandableCommand::Prefixable(PrefixableCommand::TokenListRegister) => {
            let register = scanner.scan_register_index(eqtb, logger);
            let rhs_tok_list = TokenListVariable::Toks(register);
            let rhs = eqtb.token_lists.get(rhs_tok_list);
            eqtb.token_list_define(lhs, rhs.clone(), global);
            scanner.insert_token_saved_by_afterassignment_if_any(eqtb, logger);
            return;
        }
        UnexpandableCommand::Prefixable(PrefixableCommand::TokenList(toks_var)) => {
            let rhs = eqtb.token_lists.get(toks_var);
            eqtb.token_list_define(lhs, rhs.clone(), global);
            scanner.insert_token_saved_by_afterassignment_if_any(eqtb, logger);
            return;
        }
        _ => scanner.back_input(token, eqtb, logger),
    }
    let mut token_list = scanner.scan_toks(cs, false, eqtb, logger);
    if token_list.is_empty() {
        eqtb.token_list_define(lhs, None, global);
    } else {
        if lhs == TokenListVariable::OutputRoutine {
            let mut wrapped_token_list = Vec::new();
            // Prepend a left brace token
            wrapped_token_list.push(Token::LEFT_BRACE_TOKEN);
            wrapped_token_list.append(&mut token_list);
            // Append a right brace token
            wrapped_token_list.push(Token::RIGHT_BRACE_TOKEN);

            token_list = wrapped_token_list;
        }
        let new_value = Some(std::rc::Rc::new(token_list));
        eqtb.token_list_define(lhs, new_value, global);
    }
}

/// See 1236.
fn store_in_register(
    value_type: ValueType,
    global: bool,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let register = scanner.scan_register_index(eqtb, logger);
    scanner.scan_optional_equals(eqtb, logger);

    match value_type {
        ValueType::Int => {
            let target = IntegerVariable::Count(register);
            let value = Integer::scan_int(scanner, eqtb, logger);
            eqtb.int_define(target, value, global, logger);
        }
        ValueType::Dimen => {
            let target = DimensionVariable::Dimen(register);
            let value = scan_normal_dimen(scanner, eqtb, logger);
            eqtb.dimen_define(target, value, global);
        }
        ValueType::Glue => {
            let target = SkipVariable::Skip(register);
            let glue_spec = scan_glue(false, scanner, eqtb, logger);
            eqtb.skip_define(target, glue_spec, global);
        }
        ValueType::Mu => {
            let target = SkipVariable::MuSkip(register);
            let glue_spec = scan_glue(true, scanner, eqtb, logger);
            eqtb.skip_define(target, glue_spec, global);
        }
    }
}

/// See 1243.
fn alter_space_factor(
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    match nest.mode_mut() {
        RichMode::Horizontal(hmode) => {
            scanner.scan_optional_equals(eqtb, logger);
            let factor = Integer::scan_int(scanner, eqtb, logger);
            if factor <= 0 || factor > 32767 {
                logger.print_err("Bad space factor");
                let help = &["I allow only values in the range 1..32767 here."];
                logger.int_error(factor, help, scanner, eqtb);
            } else {
                hmode.set_space_factor(factor as u16, eqtb);
            }
        }
        _ => {
            report_illegal_case(
                UnexpandableCommand::Prefixable(PrefixableCommand::SpaceFactor),
                scanner,
                eqtb,
                logger,
            );
        }
    }
}

/// See 1243.
fn alter_prev_depth(
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    match nest.mode_mut() {
        RichMode::Vertical(vmode) => {
            scanner.scan_optional_equals(eqtb, logger);
            let dimen = scan_normal_dimen(scanner, eqtb, logger);
            vmode.set_prev_depth(dimen, eqtb);
        }
        _ => report_illegal_case(
            UnexpandableCommand::Prefixable(PrefixableCommand::PrevDepth),
            scanner,
            eqtb,
            logger,
        ),
    }
}

/// See 1244.
fn alter_prev_graf(
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    scanner.scan_optional_equals(eqtb, logger);
    let new_prev_graf = Integer::scan_int(scanner, eqtb, logger);
    if new_prev_graf < 0 {
        logger.print_err("Bad ");
        logger.print_esc_str(b"prevgraf");
        let help = &["I allow only nonnegative values here."];
        logger.int_error(new_prev_graf, help, scanner, eqtb);
    } else {
        nest.set_prev_graf(new_prev_graf, eqtb);
    }
}

/// See 1246.
fn alter_dead_cycles(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) {
    scanner.scan_optional_equals(eqtb, logger);
    let integer = Integer::scan_int(scanner, eqtb, logger);
    eqtb.dead_cycles = integer;
}

/// See 1246.
fn alter_insert_penalties(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) {
    scanner.scan_optional_equals(eqtb, logger);
    let integer = Integer::scan_int(scanner, eqtb, logger);
    eqtb.insert_penalties = integer;
}

/// See 1233.
fn largest_legal_code_value(code: CodeType) -> i32 {
    match code {
        CodeType::LcCode => 255,
        CodeType::UcCode => 255,
        CodeType::SfCode => 0o77777,
        CodeType::MathCode => 0o100000,
        CodeType::DelCode => 0o77777777,
    }
}

/// See 1257.
fn new_font(global: bool, scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) {
    if logger.job_name.is_none() {
        logger.open_log_file(&scanner.input_stack, eqtb);
    }
    let cs = get_r_token(scanner, eqtb, logger);
    let t = match cs {
        ControlSequence::NullCs => b"FONT".to_vec(),
        ControlSequence::Single(c) => vec![c],
        ControlSequence::Active(c) => {
            let mut string_printer = StringPrinter::new(eqtb.get_current_escape_character());
            string_printer.print_str("FONT");
            string_printer.print(c);
            string_printer.into_string()
        }
        _ => eqtb.control_sequences.text(cs).to_vec(),
    };
    let command = Command::Unexpandable(UnexpandableCommand::Prefixable(
        PrefixableCommand::SetFont(NULL_FONT),
    ));
    eqtb.cs_define(cs, command, global);
    scanner.scan_optional_equals(eqtb, logger);
    let file_name = scanner.scan_file_name(eqtb, logger);
    let mut path = PathBuf::from(String::from_utf8(file_name).unwrap());
    path.set_extension("");
    let s = scan_font_size_specification(scanner, eqtb, logger);
    let font_index = if let Some(font_index) = font_had_already_been_loaded(&path, s, &eqtb.fonts) {
        font_index
    } else {
        read_font_info(cs, &path, s, scanner, eqtb, logger)
    };

    let command = Command::Unexpandable(UnexpandableCommand::Prefixable(
        PrefixableCommand::SetFont(font_index),
    ));
    eqtb.control_sequences.set(cs, command);
    let font_id = ControlSequence::FontId(font_index);
    let command = Command::Unexpandable(UnexpandableCommand::Prefixable(
        PrefixableCommand::SetFont(font_index),
    ));
    let level = eqtb.variable_levels.get(cs.to_variable());

    eqtb.control_sequences.set(font_id, command);
    eqtb.variable_levels.set(font_id.to_variable(), level);
    eqtb.control_sequences.set_text(font_id, &t);
}

/// See 1258.
fn scan_font_size_specification(
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Scaled {
    let mut s;
    if scanner.scan_keyword(b"at", eqtb, logger) {
        s = put_positive_at_size(scanner, eqtb, logger)
    } else if scanner.scan_keyword(b"scaled", eqtb, logger) {
        let factor = Integer::scan_int(scanner, eqtb, logger);
        s = -factor;
        if factor <= 0 || factor > 32768 {
            logger.print_err("Illegal magnification has been changed to 1000");
            let help = &["The magnification ratio must be between 1 and 32768."];
            logger.int_error(factor, help, scanner, eqtb);
            s = -1000
        }
    } else {
        s = -1000;
    }
    s
}

/// See 1259.
fn put_positive_at_size(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) -> Scaled {
    let s = scan_normal_dimen(scanner, eqtb, logger);
    if s <= 0 || s >= 0o1000000000 {
        logger.print_err("Improper `at' size (");
        logger.print_scaled(s);
        logger.print_str("pt), replaced by 10pt");
        let help = &[
            "I can only handle fonts at positive sizes that are",
            "less than 2048pt, so I've changed what you said to 10pt.",
        ];
        logger.error(help, scanner, eqtb);
        10 * UNITY
    } else {
        s
    }
}

/// Returns the font number if the requested font has been loaded before; else None.
/// See 1260.
fn font_had_already_been_loaded(path: &Path, s: Scaled, fonts: &[FontInfo]) -> Option<FontIndex> {
    for f in 1..fonts.len() {
        // If `f` has the same path as the currently requested font
        let mut font_path = PathBuf::from(std::str::from_utf8(&fonts[f].area).unwrap());
        font_path.push(std::str::from_utf8(&fonts[f].name).unwrap());
        if path == font_path {
            // A positive s denotes an "at" size.
            if s > 0 {
                if s == fonts[f].size {
                    return Some(f as FontIndex);
                }
            // A negative s denotes a "scaled" size.
            } else if fonts[f].size
                == xn_over_d(fonts[f].dsize, -s, 1000)
                    .expect("An already large font has been scaled too large")
            {
                return Some(f as FontIndex);
            }
        }
    }
    None
}

/// See 560.
fn read_font_info(
    cs: ControlSequence,
    path: &Path,
    s: Scaled,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> FontIndex {
    if eqtb.fonts.len() == FontIndex::MAX as usize {
        start_font_error_messsage(cs, path, s, eqtb, logger);
        logger.print_str(" not loaded: Not enough room left");
        let help = &[
            "I'm afraid I won't be able to make use of this font,",
            "because my memory for character-size data is too small.",
            "If you're really stuck, ask a wizard to enlarge me.",
            "Or maybe try `I\\font<same font id>=<name of loaded font>'.",
        ];
        logger.error(help, scanner, eqtb);
        return NULL_FONT;
    }
    let size_indicator = if s > 0 {
        SizeIndicator::AtSize(s)
    } else {
        SizeIndicator::Factor((-s) as u32)
    };
    let hyphen_char = *eqtb.integers.get(IntegerVariable::DefaultHyphenChar);
    let skew_char = *eqtb.integers.get(IntegerVariable::DefaultSkewChar);
    let font_info = match FontInfo::from_file(path, size_indicator, hyphen_char, skew_char) {
        Ok(font_info) => font_info,
        Err(err) => {
            match err {
                TfmError::FileNotFound => {
                    report_font_wont_be_loaded(cs, path, s, false, scanner, eqtb, logger)
                }
                TfmError::BadFormat => {
                    report_font_wont_be_loaded(cs, path, s, true, scanner, eqtb, logger)
                }
            }
            return NULL_FONT;
        }
    };
    eqtb.fonts.push(font_info);
    (eqtb.fonts.len() - 1) as FontIndex
}

/// See 561.
fn start_font_error_messsage(
    cs: ControlSequence,
    path: &Path,
    s: Scaled,
    eqtb: &Eqtb,
    logger: &mut Logger,
) {
    logger.print_err("Font ");
    cs.sprint_cs(eqtb, logger);
    logger.print_char(b'=');
    logger.print_file_name(path);
    if s > 0 {
        logger.print_str(" at ");
        logger.print_scaled(s);
        logger.print_str("pt");
    } else if s != -1000 {
        logger.print_str(" scaled ");
        logger.print_int(-s);
    }
}

/// See 561.
fn report_font_wont_be_loaded(
    cs: ControlSequence,
    path: &Path,
    s: Scaled,
    file_opened: bool,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    start_font_error_messsage(cs, path, s, eqtb, logger);
    if file_opened {
        logger.print_str(" not loadable: Bad metric (TFM) file");
    } else {
        logger.print_str(" not loadable: Metric (TFM) file not found");
    }
    let help = &[
        "I wasn't able to read the size data for this font,",
        "so I will ignore the font specification.",
        "[Wizards can fix TFM files using TFtoPL/PLtoTF.]",
        "You might try inserting a different font spec;",
        "e.g., type `I\\font<same font id>=<substitute font name>'.",
    ];
    logger.error(help, scanner, eqtb)
}

/// Return a redefinable ControlSequence from the token stream. Skips spaces.
/// If the next non-space token is not a redefinable ControlSequence, a dummy
/// ControlSequence is returned instead.
/// See 1215.
fn get_r_token(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) -> ControlSequence {
    loop {
        let mut token;
        loop {
            token = scanner.get_token(eqtb, logger);
            if token != Token::SPACE_TOKEN {
                break;
            }
        }
        match token {
            Token::LeftBrace(_)
            | Token::RightBrace(_)
            | Token::MathShift(_)
            | Token::TabMark(_)
            | Token::MacParam(_)
            | Token::SuperMark(_)
            | Token::SubMark(_)
            | Token::Spacer(_)
            | Token::Letter(_)
            | Token::OtherChar(_)
            | Token::CSToken {
                cs:
                    ControlSequence::FrozenCr
                    | ControlSequence::FrozenEndGroup
                    | ControlSequence::FrozenRight
                    | ControlSequence::FrozenFi
                    | ControlSequence::FrozenEndTemplate
                    | ControlSequence::FrozenEndv
                    | ControlSequence::FrozenRelax
                    | ControlSequence::EndWrite
                    | ControlSequence::FontId(_)
                    | ControlSequence::Undefined,
            } => {
                logger.print_err("Missing control sequence inserted");
                let help = &[
                    "Please don't say `\\def cs{...}', say `\\def\\cs{...}'.",
                    "I've inserted an inaccessible control sequence so that your",
                    "definition will be completed without mixing me up too badly.",
                    "You can recover graciously from this error, if you're",
                    "careful; see exercise 27.2 in The TeXbook.",
                ];
                match token {
                    Token::LeftBrace(_)
                    | Token::RightBrace(_)
                    | Token::MathShift(_)
                    | Token::TabMark(_)
                    | Token::MacParam(_)
                    | Token::SuperMark(_)
                    | Token::SubMark(_)
                    | Token::Spacer(_)
                    | Token::Letter(_)
                    | Token::OtherChar(_) => scanner.back_input(token, eqtb, logger),
                    Token::CSToken { .. } => {}
                    Token::Null => {
                        panic!("Should not appear here")
                    }
                }
                let frozen_protection = Token::CSToken {
                    cs: ControlSequence::FrozenProtection,
                };
                scanner.ins_error(frozen_protection, help, eqtb, logger);
            }
            Token::CSToken { cs } => return cs,
            Token::Null => {
                panic!("Should not appear here")
            }
        }
    }
}
