mod boxes;
mod catcodes;
mod codes;
mod control_sequences;
mod dimensions;
mod fonts;
mod integers;
mod levels;
mod parshape;
mod primitives;
pub mod save_stack;
mod skips;
mod tokenlists;

use crate::command::{Command, ExpandableCommand, MacroCall, UnexpandableCommand};
use crate::dimension::Dimension;
use crate::error::overflow;
use crate::fonts::FontInfo;
use crate::format::{Dumpable, FormatError};
use crate::input::{InputStack, Scanner};
use crate::integer::Integer;
use crate::logger::Logger;
use crate::macros::show_macro_def;
use crate::nodes::{show_list_node, GlueSpec, GlueType, ListNode, Node};
use crate::page_breaking::{Marks, PageContents, PageDimensions};
use crate::print::Printer;
use crate::semantic_nest::Mode;
use crate::token::Token;
use crate::token_lists::{show_token_list, RcTokenList};
use crate::vertical_mode::IGNORE_DEPTH;

use save_stack::{Group, GroupType, SaveEntry};

use boxes::BoxParameters;
pub use boxes::BoxVariable;
pub use catcodes::CatCode;
use catcodes::CatCodes;
use catcodes::CARRIAGE_RETURN;
use codes::CodeParameters;
pub use codes::{CodeType, CodeVariable, VAR_CODE};
use control_sequences::ControlSequenceStore;
pub use control_sequences::{ControlSequence, ControlSequenceId};
use dimensions::DimensionParameters;
pub use dimensions::DimensionVariable;
use fonts::FontParameters;
pub use fonts::{FontIndex, FontVariable, MathFontSize, NULL_FONT};
use integers::IntegerParameters;
pub use integers::IntegerVariable;
use levels::{Level, VariableLevels};
use parshape::ParShapeParameter;
pub use parshape::{ParShapeVariable, ParagraphShape};
pub use skips::SkipVariable;
use skips::{Skip, SkipParameters};
use tokenlists::TokenListParameters;
pub use tokenlists::TokenListVariable;

use std::convert::TryFrom;
use std::io::Write;
use std::rc::Rc;

const MAX_GROUPING_DEPTH: usize = 255;

pub type RegisterIndex = u8;

/// See Part 19.
pub struct Eqtb {
    pub cur_level: Level,
    pub cur_group: Group,
    pub save_stack: Vec<Group>,
    pub max_save_stack: usize,

    pub variable_levels: VariableLevels,

    // Regions 1 and 2
    pub control_sequences: ControlSequenceStore,

    // Region 3
    pub skips: SkipParameters,

    // Region 4
    pub par_shape: ParShapeParameter,
    pub token_lists: TokenListParameters,
    pub boxes: BoxParameters,
    pub font_params: FontParameters,
    pub cat_codes: CatCodes,
    pub codes: CodeParameters,

    // Region 5
    pub integers: IntegerParameters,

    // Region 6
    pub dimensions: DimensionParameters,

    // Store permanently the ControlSequence and Token corresponding to \par.
    // See 334.
    pub par_cs: ControlSequence,
    pub par_token: Token,
    // Store permanently the ControlSequence to \write.
    // See 1344.
    pub write_cs: ControlSequence,

    // See 286.
    mag_set: i32,

    // See 549.
    pub fonts: Vec<FontInfo>,

    /// See 646.
    pub last_badness: i32,

    /// See 592.
    pub dead_cycles: i32,
    /// 989.
    pub output_active: bool,

    /// See 982.
    pub insert_penalties: i32,

    pub page_dims: PageDimensions,
    pub page_contents: PageContents,
    pub last_node_on_page: LastNodeInfo,
    pub marks: Marks,

    // The following members are copies of internal variables that need to be accessible when
    // scanning for internal values. We copy that we can passing the corresponding data structures
    // around. They need to be kept in sync with the originals.
    pub line_number: usize,
    pub mode_type: Mode,
    pub prev_graf: i32,
    pub prev_depth: Dimension,
    pub space_factor: u16,
    pub last_node_info: LastNodeInfo,
}

impl Eqtb {
    pub fn new() -> Self {
        Self {
            cur_level: 0,
            cur_group: Group {
                typ: GroupType::BottomLevel,
                saved_definitions: Vec::new(),
                after_tokens: Vec::new(),
            },
            save_stack: Vec::new(),
            max_save_stack: 0,
            variable_levels: VariableLevels::new(),

            control_sequences: ControlSequenceStore::new(),
            skips: SkipParameters::new(),
            par_shape: ParShapeParameter::new(),
            token_lists: TokenListParameters::new(),
            boxes: BoxParameters::new(),
            font_params: FontParameters::new(),
            cat_codes: CatCodes::new(),
            codes: CodeParameters::new(),
            integers: IntegerParameters::new(),
            dimensions: DimensionParameters::new(),
            par_cs: ControlSequence::Undefined,
            write_cs: ControlSequence::Undefined,
            par_token: Token::CSToken {
                cs: ControlSequence::Undefined,
            },
            mag_set: 0,
            fonts: vec![FontInfo::null_font()],
            last_badness: 0,
            dead_cycles: 0,
            output_active: false,
            insert_penalties: 0,
            page_dims: PageDimensions::new(),
            page_contents: PageContents::Empty,
            last_node_on_page: LastNodeInfo::Other,
            marks: Marks {
                top: None,
                first: None,
                bot: None,
                split_first: None,
                split_bot: None,
            },
            line_number: 0,
            mode_type: Mode::Vertical,
            prev_graf: 0,
            prev_depth: IGNORE_DEPTH,
            space_factor: 0,
            last_node_info: LastNodeInfo::Other,
        }
    }

    /// Store the primitives in the ControlSequenceStore and store the addresses
    /// of \par and \write internally.
    /// See 1336.
    pub fn init_prim(&mut self) {
        let permanent_adresses = self.put_primitives_into_hash_table();
        self.par_cs = permanent_adresses.par_cs;
        self.write_cs = permanent_adresses.write_cs;
        self.par_token = Token::CSToken { cs: self.par_cs };
    }

    /// Sets the current time and date. This is used to fix the time point that a
    /// format file was created. That time is used when starting up and printing the
    /// banner. It is then updated so that the actual time of the run can be used in
    /// the DVI file comment.
    /// NOTE Does not currently use the right date and time.
    /// See 241.
    pub fn fix_date_and_time(&mut self) {
        self.integers.set(IntegerVariable::Time, 12 * 60);
        self.integers.set(IntegerVariable::Day, 4);
        self.integers.set(IntegerVariable::Month, 7);
        self.integers.set(IntegerVariable::Year, 1776);
    }

    /// See 236.
    pub fn end_line_char(&self) -> i32 {
        self.integer(IntegerVariable::EndLineChar)
    }

    /// See 360.
    pub fn end_line_char_inactive(&self) -> bool {
        self.end_line_char() < 0 || self.end_line_char() > 255
    }

    /// See 236.
    pub fn error_context_lines(&self) -> i32 {
        self.integer(IntegerVariable::ErrorContextLines)
    }

    /// See 236.
    pub fn pausing(&self) -> i32 {
        self.integer(IntegerVariable::Pausing)
    }

    pub fn tracing_online(&self) -> i32 {
        self.integer(IntegerVariable::TracingOnline)
    }

    pub fn tracing_output(&self) -> i32 {
        self.integer(IntegerVariable::TracingOutput)
    }

    pub fn tracing_paragraphs(&self) -> i32 {
        self.integer(IntegerVariable::TracingParagraphs)
    }

    pub fn tracing_restores(&self) -> i32 {
        self.integer(IntegerVariable::TracingRestores)
    }

    // We need to avoid the keyword `box`
    pub fn boks(&self, n: RegisterIndex) -> &Option<ListNode> {
        self.boxes.get(BoxVariable(n))
    }

    pub fn cur_font(&self) -> FontIndex {
        *self.font_params.get(FontVariable::CurFont)
    }

    pub fn fam_fnt(&self, size: MathFontSize, number: usize) -> FontIndex {
        let font_var = FontVariable::MathFont { size, number };
        *self.font_params.get(font_var)
    }

    /// See 558.
    pub fn em_width_for_cur_font(&self) -> Dimension {
        self.fonts[self.cur_font() as usize].quad()
    }

    /// See 559.
    pub fn x_height_for_cur_font(&self) -> Dimension {
        self.fonts[self.cur_font() as usize].x_height()
    }

    pub fn cat_code(&self, chr: u8) -> CatCode {
        *self.cat_codes.get(chr)
    }

    pub fn lc_code(&self, c: usize) -> i32 {
        *self.codes.get(CodeVariable::LcCode(c))
    }

    pub fn uc_code(&self, c: usize) -> i32 {
        *self.codes.get(CodeVariable::UcCode(c))
    }

    pub fn sf_code(&self, c: usize) -> i32 {
        *self.codes.get(CodeVariable::SfCode(c))
    }

    pub fn math_code(&self, c: usize) -> i32 {
        *self.codes.get(CodeVariable::MathCode(c))
    }

    pub fn del_code(&self, n: usize) -> i32 {
        *self.codes.get(CodeVariable::DelCode(n))
    }

    pub fn left_skip(&self) -> &Rc<GlueSpec> {
        self.skips.get(SkipVariable::LeftSkip)
    }

    pub fn right_skip(&self) -> &Rc<GlueSpec> {
        self.skips.get(SkipVariable::RightSkip)
    }

    pub fn integer(&self, int_var: IntegerVariable) -> Integer {
        *self.integers.get(int_var)
    }

    pub fn dimen(&self, dim_var: DimensionVariable) -> Dimension {
        *self.dimensions.get(dim_var)
    }

    /// See 243.
    pub fn get_current_escape_character(&self) -> Option<u8> {
        let c = self.integer(IntegerVariable::EscapeChar);
        if c >= 0 && c < 256 {
            Some(c as u8)
        } else {
            None
        }
    }

    /// See 244.
    pub fn get_current_newline_character(&self) -> Option<u8> {
        let c = self.integer(IntegerVariable::NewLineChar);
        if c >= 0 && c < 256 {
            Some(c as u8)
        } else {
            None
        }
    }

    /// The current input line.
    pub fn line_number(&self) -> usize {
        self.line_number
    }

    /// The current mode.
    pub fn mode(&self) -> Mode {
        self.mode_type
    }

    /// A `ControlSequence` version of `define`.
    /// See 1214., 277., and 279.
    pub fn cs_define(&mut self, cs: ControlSequence, command: Command, global: bool) {
        self.define(Definition::ControlSequence(cs, command), global);
    }

    /// A skip version of `define`.
    /// See 1214., 277., and 279.
    pub fn skip_define(&mut self, skip_var: SkipVariable, value: Skip, global: bool) {
        self.define(Definition::Skip(skip_var, value), global);
    }

    /// A paragraph shape version of `define`.
    /// See 1214., 277., and 279.
    pub fn par_shape_define(&mut self, value: ParagraphShape, global: bool) {
        self.define(Definition::ParShape(value), global);
    }

    /// A token list version of `define`.
    /// See 1214., 277., and 279.
    pub fn token_list_define(
        &mut self,
        tok_list_var: TokenListVariable,
        value: Option<RcTokenList>,
        global: bool,
    ) {
        self.define(Definition::TokenList(tok_list_var, value), global);
    }

    /// A box version of `define`.
    /// See 1214., 277., and 279.
    pub fn box_define(&mut self, register: RegisterIndex, value: Option<ListNode>, global: bool) {
        self.define(
            Definition::BoxRegister(BoxVariable(register), value),
            global,
        );
    }

    /// A font version of `define`.
    /// See 1214., 277., and 279.
    pub fn font_define(&mut self, font_var: FontVariable, font_index: FontIndex, global: bool) {
        self.define(Definition::Font(font_var, font_index), global);
    }

    /// A category code version of `define`.
    /// See 1214., 277., and 279.
    pub fn cat_code_define(&mut self, chr: u8, cat_code: CatCode, global: bool) {
        self.define(Definition::CatCode(chr, cat_code), global);
    }

    /// A code version of `define`.
    /// See 1214., 277., and 279.
    pub fn code_define(&mut self, code_var: CodeVariable, value: i32, global: bool) {
        self.define(Definition::Code(code_var, value), global);
    }

    /// An integer version of `word_define`.
    /// See 1214., 278., and 279.
    pub fn int_define(
        &mut self,
        int_var: IntegerVariable,
        value: i32,
        global: bool,
        logger: &mut Logger,
    ) {
        // We update the Logger's copies of escapechar and newlinechar here.
        if let IntegerVariable::EscapeChar = int_var {
            logger.escape_char = match u8::try_from(value) {
                Ok(c) => Some(c),
                Err(_) => None,
            };
        } else if let IntegerVariable::NewLineChar = int_var {
            logger.newline_char = match u8::try_from(value) {
                Ok(c) => Some(c),
                Err(_) => None,
            };
        }
        self.define(Definition::Integer(int_var, value), global);
    }

    /// A dimen version of `word_define`.
    /// See 1214., 278., and 279.
    pub fn dimen_define(&mut self, dimen_var: DimensionVariable, value: i32, global: bool) {
        self.define(Definition::Dimen(dimen_var, value), global);
    }

    /// See 1214., 278., and 279.
    fn define(&mut self, definition: Definition, global: bool) {
        let variable = definition.to_variable();
        if global {
            self.apply_definition(definition);
            self.variable_levels.set(variable, 0);
        } else {
            let prev_definition = self.apply_definition(definition);
            let prev_level = self.variable_levels.set(variable, self.cur_level);
            if prev_level != self.cur_level {
                self.cur_group.saved_definitions.push(SaveEntry {
                    definition: prev_definition,
                    level: prev_level,
                });
            }
        }
    }

    fn apply_definition(&mut self, definition: Definition) -> Definition {
        match definition {
            Definition::ControlSequence(control_sequence, command) => {
                let prev_command = self.control_sequences.set(control_sequence, command);
                Definition::ControlSequence(control_sequence, prev_command)
            }
            Definition::Skip(skip_variable, skip) => {
                let prev_skip = self.skips.set(skip_variable, skip);
                Definition::Skip(skip_variable, prev_skip)
            }
            Definition::ParShape(paragraph_shape) => {
                let prev_paragraph_shape = self.par_shape.set(ParShapeVariable, paragraph_shape);
                Definition::ParShape(prev_paragraph_shape)
            }
            Definition::TokenList(token_list_variable, token_list) => {
                let prev_token_list = self.token_lists.set(token_list_variable, token_list);
                Definition::TokenList(token_list_variable, prev_token_list)
            }
            Definition::BoxRegister(box_variable, list_node) => {
                let prev_list_node = self.boxes.set(box_variable, list_node);
                Definition::BoxRegister(box_variable, prev_list_node)
            }
            Definition::Font(font_variable, font_index) => {
                let prev_font_index = self.font_params.set(font_variable, font_index);
                Definition::Font(font_variable, prev_font_index)
            }
            Definition::CatCode(chr, cat_code) => {
                let prev_cat_code = self.cat_codes.set(chr, cat_code);
                Definition::CatCode(chr, prev_cat_code)
            }
            Definition::Code(code_variable, val) => {
                let prev_val = self.codes.set(code_variable, val);
                Definition::Code(code_variable, prev_val)
            }
            Definition::Integer(integer_variable, val) => {
                let prev_val = self.integers.set(integer_variable, val);
                Definition::Integer(integer_variable, prev_val)
            }
            Definition::Dimen(dimension_variable, dimension) => {
                let prev_dimension = self.dimensions.set(dimension_variable, dimension);
                Definition::Dimen(dimension_variable, prev_dimension)
            }
        }
    }

    /// See 284.
    #[cfg(feature = "stats")]
    fn restore_trace(&self, variable: Variable, s: &str, tracing_online: i32, logger: &mut Logger) {
        logger.begin_diagnostic(tracing_online);
        logger.print_char(b'{');
        logger.print_str(s);
        logger.print_char(b' ');
        self.show_equivalent_of_variable(variable, logger);
        logger.print_char(b'}');
        logger.end_diagnostic(false);
    }

    /// See 274.
    pub fn new_save_level(&mut self, c: GroupType, input_stack: &InputStack, logger: &mut Logger) {
        if self.cur_level == MAX_GROUPING_DEPTH {
            overflow(
                "grouping levels",
                MAX_GROUPING_DEPTH,
                input_stack,
                self,
                logger,
            );
        }
        let new_group = Group {
            typ: c,
            saved_definitions: Vec::new(),
            after_tokens: Vec::new(),
        };
        let saved_group = std::mem::replace(&mut self.cur_group, new_group);
        self.save_stack.push(saved_group);
        self.cur_level += 1;
    }

    /// See 280.
    pub fn save_for_after(&mut self, token: Token) {
        if self.cur_level > 0 {
            self.cur_group.after_tokens.push(token);
        }
    }

    /// Closes the current save level, restores definitions where appropriate and inserts
    /// AfterTokens.
    /// See 281. and 282.
    pub fn unsave(&mut self, scanner: &mut Scanner, logger: &mut Logger) {
        if self.cur_level > 0 {
            self.cur_level -= 1;
            let prev_group = self
                .save_stack
                .pop()
                .expect("Save stack should not be empty here");
            let finished_group = std::mem::replace(&mut self.cur_group, prev_group);

            for entry in finished_group.saved_definitions.into_iter().rev() {
                self.restore_or_discard_saved_definition(entry.definition, entry.level, logger);
            }
            for token in finished_group.after_tokens.into_iter().rev() {
                scanner.insert_token_into_input(token, self, logger);
            }
        } else {
            panic!("Unsave should never be called if we are at base level");
        }
    }

    /// See 283.
    fn restore_or_discard_saved_definition(
        &mut self,
        definition: Definition,
        level: Level,
        logger: &mut Logger,
    ) {
        let variable = definition.to_variable();
        if let Definition::Integer(int_var, value) = definition {
            // We update the Logger's copies of escapechar and newlinechar here.
            if let IntegerVariable::EscapeChar = int_var {
                let cur_level = self.variable_levels.get(variable);
                // If the value will be restored.
                if cur_level != 0 {
                    logger.escape_char = match u8::try_from(value) {
                        Ok(c) => Some(c),
                        Err(_) => None,
                    };
                }
            } else if let IntegerVariable::NewLineChar = int_var {
                let cur_level = self.variable_levels.get(variable);
                // If the value will be restored.
                if cur_level != 0 {
                    logger.newline_char = match u8::try_from(value) {
                        Ok(c) => Some(c),
                        Err(_) => None,
                    };
                }
            }
        }
        let restoring = self.unsave_definition(definition, level);
        if cfg!(feature = "stats") {
            if self.tracing_restores() > 0 {
                if restoring {
                    self.restore_trace(variable, "restoring", self.tracing_online(), logger);
                } else {
                    self.restore_trace(variable, "retaining", self.tracing_online(), logger);
                }
            }
        }
    }

    /// See 283.
    fn unsave_definition(&mut self, definition: Definition, level: Level) -> bool {
        let variable = definition.to_variable();
        let cur_level = self.variable_levels.get(variable);

        if cur_level != 0 {
            self.apply_definition(definition);
            self.variable_levels.set(variable, level);
            true
        } else {
            false
        }
    }

    /// See 288.
    pub fn prepare_mag(&mut self, scanner: &mut Scanner, logger: &mut Logger) {
        let mag = self.integer(IntegerVariable::Mag);
        if self.mag_set > 0 && mag != self.mag_set {
            logger.print_err("Incompatible magnification (");
            logger.print_int(mag);
            logger.print_str(");");
            logger.print_nl_str(" the previous value will be retained");
            let help = &[
                "I can handle only one magnification ratio per job. So I've",
                "reverted to the magnification you used earlier on this run.",
            ];
            logger.int_error(self.mag_set, help, scanner, self);
            self.int_define(IntegerVariable::Mag, self.mag_set, true, logger);
        }
        if mag <= 0 || mag > 32768 {
            logger.print_err("Illegal magnification has been changed to 1000");
            let help = &["The magnification ratio must be between 1 and 32768."];
            logger.int_error(mag, help, scanner, self);
            self.int_define(IntegerVariable::Mag, 1000, true, logger);
        }
        self.mag_set = mag;
    }

    /// See 252.
    #[cfg(feature = "stats")]
    fn show_equivalent_of_variable(&self, variable: Variable, logger: &mut Logger) {
        match variable {
            Variable::ControlSequence(control_sequence) => {
                self.show_equivalent_of_control_sequence(control_sequence, logger)
            }
            Variable::Skip(skip_var) => self.show_equivalent_of_skip_variable(skip_var, logger),
            Variable::ParShape => self.show_equivalent_of_par_shape(logger),
            Variable::TokenList(token_list_var) => {
                self.show_equivalent_of_token_list_variable(token_list_var, logger)
            }
            Variable::BoxRegister(box_var) => self.show_equivalent_of_box_variable(box_var, logger),
            Variable::Font(font_var) => self.show_equivalent_of_font_variable(font_var, logger),
            Variable::CatCode(chr) => self.show_equivalent_of_cat_code_variable(chr, logger),
            Variable::Code(code_var) => self.show_equivalent_of_code_variable(code_var, logger),
            Variable::Integer(int_var) => self.show_equivalent_of_integer_variable(int_var, logger),
            Variable::Dimen(dimen_var) => {
                self.show_equivalent_of_dimension_variable(dimen_var, logger)
            }
        }
    }

    /// See 223.
    fn show_equivalent_of_control_sequence(&self, cs: ControlSequence, logger: &mut Logger) {
        cs.sprint_cs(self, logger);
        logger.print_char(b'=');
        let command = self.control_sequences.get(cs);
        command.display(&self.fonts, logger);
        if let Command::Expandable(ExpandableCommand::Macro(MacroCall { macro_def, .. })) = command
        {
            logger.print_char(b':');
            show_macro_def(macro_def, 32, logger, self);
        }
        // NOTE We keep this for now for compatibility.
        if let Command::Expandable(ExpandableCommand::EndTemplate) = command {
            logger.print_char(b':');
        }
    }

    /// See 229.
    fn show_equivalent_of_skip_variable(&self, skip_var: SkipVariable, logger: &mut Logger) {
        logger.print_esc_str(&skip_var.to_string());
        logger.print_char(b'=');
        let glue_spec = self.skips.get(skip_var);
        if let SkipVariable::ThinMuSkip
        | SkipVariable::MedMuSkip
        | SkipVariable::ThickMuSkip
        | SkipVariable::MuSkip(_) = skip_var
        {
            glue_spec.print_spec(Some("mu"), logger);
        } else {
            glue_spec.print_spec(Some("pt"), logger);
        }
    }

    /// See 233.
    fn show_equivalent_of_par_shape(&self, logger: &mut Logger) {
        logger.print_esc_str(&ParShapeVariable.to_string());
        logger.print_char(b'=');
        let par_shape = self.par_shape.get(ParShapeVariable);
        logger.print_int(par_shape.len() as i32);
    }

    /// See 233.
    fn show_equivalent_of_token_list_variable(
        &self,
        tok_list_var: TokenListVariable,
        logger: &mut Logger,
    ) {
        logger.print_esc_str(&tok_list_var.to_string());
        logger.print_char(b'=');
        let list = self.token_lists.get(tok_list_var);
        if let Some(list) = list {
            show_token_list(list, 32, logger, self);
        }
    }

    /// See 233.
    fn show_equivalent_of_box_variable(&self, box_var: BoxVariable, logger: &mut Logger) {
        logger.print_esc_str(&box_var.to_string());
        logger.print_char(b'=');
        let node_list = self.boxes.get(box_var);
        match node_list {
            None => {
                logger.print_str("void");
            }
            Some(list_node) => {
                let depth_max = 0;
                let breadth_max = 1;
                show_list_node(list_node, b"", depth_max, breadth_max, self, logger);
            }
        }
    }

    /// See 233. and 234.
    fn show_equivalent_of_font_variable(&self, font_var: FontVariable, logger: &mut Logger) {
        // The current font should print only "current font" without
        // the escape character.
        match font_var {
            FontVariable::CurFont => logger.print_str("current font"),
            FontVariable::MathFont { size, number } => {
                logger.print_esc_str(format!("{}{}", size.as_str(), number).as_bytes())
            }
        }
        logger.print_char(b'=');
        let f = *self.font_params.get(font_var);
        logger.print_esc_str(self.control_sequences.text(ControlSequence::FontId(f)));
    }

    /// See 236.
    pub fn get_max_depth_and_breadth(&self) -> (i32, usize) {
        let max_depth = self.integer(IntegerVariable::ShowBoxDepth);
        let show_box_breadth = self.integer(IntegerVariable::ShowBoxBreadth);
        let max_breadth = if show_box_breadth <= 0 {
            5
        } else {
            show_box_breadth as usize
        };
        (max_depth, max_breadth)
    }

    /// See 242.
    fn show_equivalent_of_integer_variable(&self, int_var: IntegerVariable, logger: &mut Logger) {
        logger.print_esc_str(&int_var.to_string());
        logger.print_char(b'=');
        let int_val = *self.integers.get(int_var);
        logger.print_int(int_val);
    }

    /// See 233., 235. and 242.
    fn show_equivalent_of_cat_code_variable(&self, chr: u8, logger: &mut Logger) {
        let name = format!("catcode{}", chr).as_bytes().to_vec();
        logger.print_esc_str(&name);
        logger.print_char(b'=');
        let cat_code = *self.cat_codes.get(chr);
        logger.print_int(cat_code as i32);
    }

    /// See 233., 235. and 242.
    fn show_equivalent_of_code_variable(&self, code_var: CodeVariable, logger: &mut Logger) {
        logger.print_esc_str(&code_var.to_string());
        logger.print_char(b'=');
        let code_val = *self.codes.get(code_var);
        logger.print_int(code_val);
    }

    /// See 251.
    fn show_equivalent_of_dimension_variable(
        &self,
        dimen_var: DimensionVariable,
        logger: &mut Logger,
    ) {
        logger.print_esc_str(&dimen_var.to_string());
        logger.print_char(b'=');
        logger.print_scaled(*self.dimensions.get(dimen_var));
        logger.print_str("pt");
    }

    /// See 700.
    fn mathsy(&self, size: MathFontSize, param: usize) -> Dimension {
        // We remove the 1-indexing here.
        self.fonts[self.fam_fnt(size, 2) as usize].params[param - 1]
    }

    /// See 700.
    pub fn math_x_height(&self, size: MathFontSize) -> Dimension {
        self.mathsy(size, 5)
    }

    /// See 700.
    pub fn math_quad(&self, size: MathFontSize) -> Dimension {
        self.mathsy(size, 6)
    }

    /// See 700.
    pub fn num1(&self, size: MathFontSize) -> Dimension {
        self.mathsy(size, 8)
    }

    /// See 700.
    pub fn num2(&self, size: MathFontSize) -> Dimension {
        self.mathsy(size, 9)
    }

    /// See 700.
    pub fn num3(&self, size: MathFontSize) -> Dimension {
        self.mathsy(size, 10)
    }

    /// See 700.
    pub fn denom1(&self, size: MathFontSize) -> Dimension {
        self.mathsy(size, 11)
    }

    /// See 700.
    pub fn denom2(&self, size: MathFontSize) -> Dimension {
        self.mathsy(size, 12)
    }

    /// See 700.
    pub fn sup1(&self, size: MathFontSize) -> Dimension {
        self.mathsy(size, 13)
    }

    /// See 700.
    pub fn sup2(&self, size: MathFontSize) -> Dimension {
        self.mathsy(size, 14)
    }

    /// See 700.
    pub fn sup3(&self, size: MathFontSize) -> Dimension {
        self.mathsy(size, 15)
    }

    /// See 700.
    pub fn sub1(&self, size: MathFontSize) -> Dimension {
        self.mathsy(size, 16)
    }

    /// See 700.
    pub fn sub2(&self, size: MathFontSize) -> Dimension {
        self.mathsy(size, 17)
    }

    /// See 700.
    pub fn sup_drop(&self, size: MathFontSize) -> Dimension {
        self.mathsy(size, 18)
    }

    /// See 700.
    pub fn sub_drop(&self, size: MathFontSize) -> Dimension {
        self.mathsy(size, 19)
    }

    /// See 700.
    pub fn delim1(&self, size: MathFontSize) -> Dimension {
        self.mathsy(size, 20)
    }

    /// See 700.
    pub fn delim2(&self, size: MathFontSize) -> Dimension {
        self.mathsy(size, 21)
    }

    /// See 700.
    pub fn axis_height(&self, size: MathFontSize) -> Dimension {
        self.mathsy(size, 22)
    }

    /// See 701.
    pub fn mathex(&self, size: MathFontSize, param: usize) -> Dimension {
        // We remove the 1-indexing here.
        self.fonts[self.fam_fnt(size, 3) as usize].params[param - 1]
    }

    /// See 701.
    pub fn default_rule_thickness(&self, size: MathFontSize) -> Dimension {
        self.mathex(size, 8)
    }

    /// See 701.
    pub fn big_op_spacing1(&self, size: MathFontSize) -> Dimension {
        self.mathex(size, 9)
    }

    /// See 701.
    pub fn big_op_spacing2(&self, size: MathFontSize) -> Dimension {
        self.mathex(size, 10)
    }

    /// See 701.
    pub fn big_op_spacing3(&self, size: MathFontSize) -> Dimension {
        self.mathex(size, 11)
    }

    /// See 701.
    pub fn big_op_spacing4(&self, size: MathFontSize) -> Dimension {
        self.mathex(size, 12)
    }

    /// See 701.
    pub fn big_op_spacing5(&self, size: MathFontSize) -> Dimension {
        self.mathex(size, 13)
    }

    /// Gets the `ControlSequence` corresponding to the name or returns None.
    /// See 374.
    pub fn lookup(&self, name: &[u8]) -> Option<ControlSequence> {
        match name.len() {
            0 => Some(ControlSequence::NullCs),
            1 => Some(ControlSequence::Single(name[0])),
            _ => self
                .control_sequences
                .id_lookup(name)
                .map(|n| ControlSequence::Escaped(n)),
        }
    }

    /// Gets the `ControlSequence` corresponding to the name or creates a new
    /// one for this name.
    /// See 374.
    pub fn lookup_or_create(&mut self, name: &[u8]) -> Result<ControlSequence, ()> {
        let cs = match name.len() {
            0 => ControlSequence::NullCs,
            1 => ControlSequence::Single(name[0]),
            _ => match self.control_sequences.id_lookup(name) {
                Some(n) => ControlSequence::Escaped(n),
                None => {
                    let n = self
                        .control_sequences
                        .add_command(name, &mut self.variable_levels)?;
                    ControlSequence::Escaped(n)
                }
            },
        };
        Ok(cs)
    }

    /// See 264.
    pub fn primitive_unexpandable(
        &mut self,
        name: &[u8],
        unexpandable_command: UnexpandableCommand,
    ) -> ControlSequence {
        let cs = self
            .lookup_or_create(name)
            .expect("There should always be enough space for the primitives");
        let command = Command::Unexpandable(unexpandable_command);
        // There should not be a previous definition.
        self.control_sequences.set(cs, command);
        self.control_sequences.set_text(cs, name);

        cs
    }

    /// See 264.
    pub fn primitive_expandable(
        &mut self,
        name: &[u8],
        expandable_command: ExpandableCommand,
    ) -> ControlSequence {
        let cs = self
            .lookup_or_create(name)
            .expect("There should always be enough space for the primitives");
        let command = Command::Expandable(expandable_command);
        // There should not be a previous definition.
        self.control_sequences.set(cs, command);
        self.control_sequences.set_text(cs, name);

        cs
    }

    /// Updates the condensed info about the last node of the current list.
    /// NOTE: We need to ensure that this is called whenever the last node of the current list has
    /// been changed.
    pub fn update_last_node_info(&mut self, last_node: Option<&Node>) {
        self.last_node_info = if let Some(last_node) = last_node {
            match last_node {
                Node::Penalty(penalty_node) => LastNodeInfo::Penalty(penalty_node.penalty),
                Node::Kern(kern_node) => LastNodeInfo::Kern(kern_node.width),
                Node::Disc(disc_node) => {
                    if let Some(Node::Kern(kern_node)) = disc_node.no_break.last() {
                        LastNodeInfo::Kern(kern_node.width)
                    } else {
                        LastNodeInfo::Other
                    }
                }
                Node::Glue(glue_node) => {
                    if let GlueType::MuGlue = glue_node.subtype {
                        LastNodeInfo::MuGlue(glue_node.glue_spec.clone())
                    } else {
                        LastNodeInfo::Glue(glue_node.glue_spec.clone())
                    }
                }
                _ => LastNodeInfo::Other,
            }
        } else {
            LastNodeInfo::Other
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Variable {
    ControlSequence(ControlSequence),
    Skip(SkipVariable),
    ParShape,
    TokenList(TokenListVariable),
    BoxRegister(BoxVariable),
    Font(FontVariable),
    CatCode(u8),
    Code(CodeVariable),
    Integer(IntegerVariable),
    Dimen(DimensionVariable),
}

/// A variable together with its definition.
#[derive(Debug)]
pub enum Definition {
    ControlSequence(ControlSequence, Command),
    Skip(SkipVariable, Skip),
    ParShape(ParagraphShape),
    TokenList(TokenListVariable, Option<RcTokenList>),
    BoxRegister(BoxVariable, Option<ListNode>),
    Font(FontVariable, FontIndex),
    CatCode(u8, CatCode),
    Code(CodeVariable, i32),
    Integer(IntegerVariable, Integer),
    Dimen(DimensionVariable, Dimension),
}

impl Definition {
    fn to_variable(&self) -> Variable {
        match *self {
            Self::ControlSequence(control_sequence, _) => {
                Variable::ControlSequence(control_sequence)
            }
            Self::Skip(skip_variable, _) => Variable::Skip(skip_variable),
            Self::ParShape(_) => Variable::ParShape,
            Self::TokenList(token_list_variable, _) => Variable::TokenList(token_list_variable),
            Self::BoxRegister(box_variable, _) => Variable::BoxRegister(box_variable),
            Self::Font(font_variable, _) => Variable::Font(font_variable),
            Self::CatCode(chr, _) => Variable::CatCode(chr),
            Self::Code(code_variable, _) => Variable::Code(code_variable),
            Self::Integer(integer_variable, _) => Variable::Integer(integer_variable),
            Self::Dimen(dimension_variable, _) => Variable::Dimen(dimension_variable),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LastNodeInfo {
    Penalty(i32),
    Kern(Dimension),
    Glue(Rc<GlueSpec>),
    MuGlue(Rc<GlueSpec>),
    Other,
}

impl Dumpable for Variable {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::ControlSequence(control_sequence) => {
                writeln!(target, "ControlSequence")?;
                control_sequence.dump(target)?;
            }
            Self::Skip(skip_variable) => {
                writeln!(target, "Skip")?;
                skip_variable.dump(target)?;
            }
            Self::ParShape => {
                writeln!(target, "ParShape")?;
            }
            Self::TokenList(token_list_variable) => {
                writeln!(target, "TokenList")?;
                token_list_variable.dump(target)?;
            }
            Self::BoxRegister(box_variable) => {
                writeln!(target, "BoxRegister")?;
                box_variable.dump(target)?;
            }
            Self::Font(font_variable) => {
                writeln!(target, "Font")?;
                font_variable.dump(target)?;
            }
            Self::CatCode(chr) => {
                writeln!(target, "CatCode")?;
                chr.dump(target)?;
            }
            Self::Code(code_variable) => {
                writeln!(target, "Code")?;
                code_variable.dump(target)?;
            }
            Self::Integer(integer_variable) => {
                writeln!(target, "Integer")?;
                integer_variable.dump(target)?;
            }
            Self::Dimen(dimension_variable) => {
                writeln!(target, "Dimen")?;
                dimension_variable.dump(target)?;
            }
        }

        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "ControlSequence" => {
                let control_sequence = ControlSequence::undump(lines)?;
                Ok(Self::ControlSequence(control_sequence))
            }
            "Skip" => {
                let skip_variable = SkipVariable::undump(lines)?;
                Ok(Self::Skip(skip_variable))
            }
            "ParShape" => Ok(Self::ParShape),
            "TokenList" => {
                let token_list_variable = TokenListVariable::undump(lines)?;
                Ok(Self::TokenList(token_list_variable))
            }
            "BoxRegister" => {
                let box_variable = BoxVariable::undump(lines)?;
                Ok(Self::BoxRegister(box_variable))
            }
            "Font" => {
                let font_variable = FontVariable::undump(lines)?;
                Ok(Self::Font(font_variable))
            }
            "CatCode" => {
                let chr = u8::undump(lines)?;
                Ok(Self::CatCode(chr))
            }
            "Code" => {
                let code_variable = CodeVariable::undump(lines)?;
                Ok(Self::Code(code_variable))
            }
            "Integer" => {
                let integer_variable = IntegerVariable::undump(lines)?;
                Ok(Self::Integer(integer_variable))
            }
            "Dimen" => {
                let dimension_variable = DimensionVariable::undump(lines)?;
                Ok(Self::Dimen(dimension_variable))
            }
            _ => Err(FormatError::ParseError),
        }
    }
}

impl Dumpable for Eqtb {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.variable_levels.dump(target)?;

        self.control_sequences.dump(target)?;
        self.skips.dump(target)?;
        self.par_shape.dump(target)?;
        self.token_lists.dump(target)?;
        self.boxes.dump(target)?;
        self.font_params.dump(target)?;
        self.cat_codes.dump(target)?;
        self.codes.dump(target)?;
        self.integers.dump(target)?;
        self.dimensions.dump(target)?;

        self.par_cs.dump(target)?;
        self.par_token.dump(target)?;
        self.write_cs.dump(target)?;

        self.fonts.dump(target)?;

        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variable_levels = VariableLevels::undump(lines)?;

        let control_sequences = ControlSequenceStore::undump(lines)?;
        let skips = SkipParameters::undump(lines)?;
        let par_shape = ParShapeParameter::undump(lines)?;
        let token_lists = TokenListParameters::undump(lines)?;
        let boxes = BoxParameters::undump(lines)?;
        let font_params = FontParameters::undump(lines)?;
        let cat_codes = CatCodes::undump(lines)?;
        let codes = CodeParameters::undump(lines)?;
        let integers = IntegerParameters::undump(lines)?;
        let dimensions = DimensionParameters::undump(lines)?;

        let par_cs = ControlSequence::undump(lines)?;
        let par_token = Token::undump(lines)?;
        let write_cs = ControlSequence::undump(lines)?;

        let fonts = Vec::undump(lines)?;

        Ok(Self {
            cur_level: 0,
            cur_group: Group {
                typ: GroupType::BottomLevel,
                saved_definitions: Vec::new(),
                after_tokens: Vec::new(),
            },
            save_stack: Vec::new(),
            max_save_stack: 0,
            variable_levels,

            control_sequences,
            skips,
            par_shape,
            token_lists,
            boxes,
            font_params,
            cat_codes,
            codes,
            integers,
            dimensions,
            par_cs,
            write_cs,
            par_token,
            mag_set: 0,
            fonts,
            last_badness: 0,
            dead_cycles: 0,
            output_active: false,
            insert_penalties: 0,
            page_dims: PageDimensions::new(),
            page_contents: PageContents::Empty,
            last_node_on_page: LastNodeInfo::Other,
            marks: Marks {
                top: None,
                first: None,
                bot: None,
                split_first: None,
                split_bot: None,
            },
            line_number: 0,
            mode_type: Mode::Vertical,
            prev_graf: 0,
            prev_depth: IGNORE_DEPTH,
            space_factor: 0,
            last_node_info: LastNodeInfo::Other,
        })
    }
}
