use crate::eqtb::Eqtb;
use crate::error::overflow;
use crate::horizontal_mode::{HorizontalMode, HorizontalModeType, LanguageData};
use crate::input::InputStack;
use crate::logger::Logger;
use crate::math_mode::MathMode;
use crate::nodes::noads::{ChoiceNode, Noad};
use crate::nodes::{show_box_content, DiscNode, Node};
use crate::page_breaking::PageBuilder;
use crate::print::Printer;
use crate::vertical_mode::VerticalMode;

// Maximum number of semantic levels simultaneously active
const NEST_SIZE: usize = 40;

/// See 213.
pub struct SemanticState {
    nest: Vec<SemanticLevel>,
    pub max_nest_stack: usize,
    base_level: SemanticLevel,
}

impl SemanticState {
    /// See 216.
    pub fn push_nest(
        &mut self,
        mode: RichMode,
        input_stack: &InputStack,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        // We update the copies in Eqtb.
        eqtb.mode_type = mode.to_mode_type();
        match &mode {
            RichMode::Vertical(vmode) => {
                eqtb.prev_graf = vmode.prev_graf;
                eqtb.prev_depth = vmode.prev_depth;
            }
            RichMode::Horizontal(hmode) => {
                eqtb.space_factor = hmode.space_factor;
            }
            _ => {}
        }

        if self.nest.len() > self.max_nest_stack {
            self.max_nest_stack = self.nest.len();
            if self.nest.len() == NEST_SIZE {
                overflow("semantic nest size", NEST_SIZE, input_stack, eqtb, logger);
            }
        }
        let new_semantic_level = SemanticLevel {
            mode,
            mode_line: eqtb.line_number() as i32,
        };
        self.nest.push(new_semantic_level);

        // We update the copies in Eqtb.
        self.update_last_node_info(eqtb);
    }

    /// See 217.
    pub fn pop_nest(&mut self, eqtb: &mut Eqtb) -> SemanticLevel {
        let old_level = self.nest.pop().unwrap();

        // We update the copies in Eqtb.
        if let RichMode::Vertical(_) = old_level.mode {
            eqtb.prev_graf = self.last_vertical().prev_graf;
        }
        eqtb.mode_type = self.mode().to_mode_type();
        match self.mode() {
            RichMode::Vertical(vmode) => {
                eqtb.prev_depth = vmode.prev_depth;
            }
            RichMode::Horizontal(hmode) => {
                eqtb.space_factor = hmode.space_factor;
            }
            _ => {}
        }
        self.update_last_node_info(eqtb);

        old_level
    }

    /// See 214.
    pub fn tail_push(&mut self, node: Node, eqtb: &mut Eqtb) {
        self.cur_list_mut().push(node);
        self.update_last_node_info(eqtb);
    }

    /// See 214.
    pub fn tail_append(&mut self, mut list: Vec<Node>, eqtb: &mut Eqtb) {
        self.cur_list_mut().append(&mut list);
        self.update_last_node_info(eqtb)
    }

    pub fn pop_last(&mut self, eqtb: &mut Eqtb) -> Option<Node> {
        let node = self.cur_list_mut().pop();
        self.update_last_node_info(eqtb);
        node
    }

    pub fn cur_list_is_empty(&self) -> bool {
        self.cur_list().is_empty()
    }

    /// Updates the condensed info about the last node of the current list that we keep in the
    /// Eqtb.
    pub fn update_last_node_info(&mut self, eqtb: &mut Eqtb) {
        if self.depth() == 0 && self.base_list().is_empty() {
            eqtb.last_node_info = eqtb.last_node_on_page.clone();
        } else {
            eqtb.update_last_node_info(self.cur_list().last());
        }
    }

    pub const fn new() -> Self {
        Self {
            nest: Vec::new(),
            max_nest_stack: 0,
            base_level: SemanticLevel {
                mode: RichMode::Vertical(VerticalMode::new_base()),
                mode_line: 0,
            },
        }
    }

    pub fn depth(&self) -> usize {
        self.nest.len()
    }

    pub fn mode(&self) -> &RichMode {
        if let Some(semantic_level) = self.nest.last() {
            &semantic_level.mode
        } else {
            &self.base_level.mode
        }
    }

    pub fn mode_mut(&mut self) -> &mut RichMode {
        if let Some(semantic_level) = self.nest.last_mut() {
            &mut semantic_level.mode
        } else {
            &mut self.base_level.mode
        }
    }

    pub fn cur_list(&self) -> &Vec<Node> {
        if let Some(semantic_level) = self.nest.last() {
            match &semantic_level.mode {
                RichMode::Vertical(vmode) => &vmode.list,
                RichMode::Horizontal(hmode) => &hmode.list,
                RichMode::Math(mmode) => &mmode.list,
            }
        } else {
            self.base_list()
        }
    }

    pub fn cur_list_mut(&mut self) -> &mut Vec<Node> {
        if let Some(semantic_level) = self.nest.last_mut() {
            match &mut semantic_level.mode {
                RichMode::Vertical(vmode) => &mut vmode.list,
                RichMode::Horizontal(hmode) => &mut hmode.list,
                RichMode::Math(mmode) => &mut mmode.list,
            }
        } else {
            let RichMode::Vertical(base_mode) = &mut self.base_level.mode else {
                panic!("The base level must be vertical");
            };
            &mut base_mode.list
        }
    }

    pub fn mode_line(&self) -> i32 {
        if let Some(semantic_level) = self.nest.last() {
            semantic_level.mode_line
        } else {
            self.base_level.mode_line
        }
    }

    pub fn mode_line_mut(&mut self) -> &mut i32 {
        if let Some(semantic_level) = self.nest.last_mut() {
            &mut semantic_level.mode_line
        } else {
            &mut self.base_level.mode_line
        }
    }

    pub fn outer_mode(&self) -> &RichMode {
        debug_assert!(!self.nest.is_empty());
        if self.nest.len() > 1 {
            &self.nest[self.nest.len() - 2].mode
        } else {
            &self.base_level.mode
        }
    }

    pub fn base_list(&self) -> &Vec<Node> {
        let RichMode::Vertical(base_mode) = &self.base_level.mode else {
            panic!("The base level must be vertical");
        };
        &base_mode.list
    }

    pub fn base_list_mut(&mut self) -> &mut Vec<Node> {
        let RichMode::Vertical(base_mode) = &mut self.base_level.mode else {
            panic!("The base level must be vertical");
        };
        &mut base_mode.list
    }

    /// Return the innermost vertical mode.
    pub fn last_vertical(&self) -> &VerticalMode {
        for semantic_level in self.nest.iter().rev() {
            if let RichMode::Vertical(vmode) = &semantic_level.mode {
                return vmode;
            }
        }
        let RichMode::Vertical(vmode) = &self.base_level.mode else {
            panic!("The base level is always vertical");
        };
        vmode
    }

    pub fn last_noad_mut(&mut self) -> Option<&mut Noad> {
        if let Some(Node::Noad(last_noad)) = self.cur_list_mut().last_mut() {
            Some(last_noad)
        } else {
            None
        }
    }

    pub fn last_disc_mut(&mut self) -> Option<&mut DiscNode> {
        if let Some(Node::Disc(last_disc)) = self.cur_list_mut().last_mut() {
            Some(last_disc)
        } else {
            None
        }
    }

    pub fn last_choice_mut(&mut self) -> Option<&mut ChoiceNode> {
        if let Some(Node::Choice(last_choice)) = self.cur_list_mut().last_mut() {
            Some(last_choice)
        } else {
            None
        }
    }

    /// Set the prev_graf value for the innermost vertical mode.
    pub fn set_prev_graf(&mut self, new_prev_graf: i32, eqtb: &mut Eqtb) {
        // Get innermost vertical level.
        for semantic_level in self.nest.iter_mut().rev() {
            if let RichMode::Vertical(vmode) = &mut semantic_level.mode {
                vmode.set_prev_graf(new_prev_graf, eqtb);
                return;
            }
        }
        let RichMode::Vertical(base_mode) = &mut self.base_level.mode else {
            panic!("The base level is always vertical");
        };
        base_mode.set_prev_graf(new_prev_graf, eqtb);
    }

    /// See 218.
    pub fn show_activities(&self, page_builder: &PageBuilder, eqtb: &Eqtb, logger: &mut Logger) {
        logger.print_nl_str("").print_ln();

        for layer in self.nest.iter().rev() {
            show_layer_info(layer, logger);
            let list = match &layer.mode {
                RichMode::Vertical(vmode) => &vmode.list,
                RichMode::Horizontal(hmode) => &hmode.list,
                RichMode::Math(mmode) => &mmode.list,
            };
            show_box_content(list, eqtb, logger);
            show_auxiliary_field(layer, eqtb, logger);
        }

        show_layer_info(&self.base_level, logger);
        page_builder.show_status_of_current_page(eqtb, logger);
        if !self.base_list().is_empty() {
            logger.print_nl_str("### recent contributions:");
        }
        show_box_content(self.base_list(), eqtb, logger);
        show_auxiliary_field(&self.base_level, eqtb, logger);
    }
}

/// See 212.
pub struct SemanticLevel {
    pub mode: RichMode,
    pub mode_line: i32,
}

/// See 211.
#[derive(Debug)]
pub enum RichMode {
    Vertical(VerticalMode),
    Horizontal(HorizontalMode),
    Math(MathMode),
}

impl RichMode {
    pub fn to_mode_type(&self) -> Mode {
        match self {
            Self::Vertical(vmode) => {
                if vmode.internal {
                    Mode::InternalVertical
                } else {
                    Mode::Vertical
                }
            }
            Self::Horizontal(hmode) => match hmode.subtype {
                HorizontalModeType::Restricted => Mode::RestrictedHorizontal,
                HorizontalModeType::Unrestricted { .. } => Mode::Horizontal,
            },
            Self::Math(mmode) => {
                if mmode.display {
                    Mode::DisplayMath
                } else {
                    Mode::Math
                }
            }
        }
    }
}

/// See 211.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    Vertical,
    Horizontal,
    DisplayMath,
    InternalVertical,
    RestrictedHorizontal,
    Math,
}

impl Mode {
    pub fn base(&self) -> Self {
        match *self {
            Mode::InternalVertical => Mode::Vertical,
            Mode::RestrictedHorizontal => Mode::Horizontal,
            Mode::Math => Mode::DisplayMath,
            mode => mode,
        }
    }

    /// See 211.
    pub fn display(&self, printer: &mut impl Printer) {
        let s = match self {
            Mode::Vertical => "vertical",
            Mode::Horizontal => "horizontal",
            Mode::DisplayMath => "display math",
            Mode::InternalVertical => "internal vertical",
            Mode::RestrictedHorizontal => "restricted horizontal",
            Mode::Math => "math",
        };
        printer.print_str(s);
        printer.print_str(" mode");
    }
}

/// See 218.
fn show_layer_info(layer: &SemanticLevel, logger: &mut Logger) {
    logger.print_nl_str("### ");
    layer.mode.to_mode_type().display(logger);
    logger.print_str(" entered at line ");
    logger.print_int(layer.mode_line.abs());
    if let RichMode::Horizontal(HorizontalMode {
        subtype: HorizontalModeType::Unrestricted { lang_data, .. },
        ..
    }) = layer.mode
    {
        if lang_data
            != (LanguageData {
                left_hyphen_min: 2,
                right_hyphen_min: 3,
                language: 0,
            })
        {
            logger.print_str(" (language");
            logger.print_int(lang_data.language as i32);
            logger.print_str(":hyphenmin");
            logger.print_int(lang_data.left_hyphen_min as i32);
            logger.print_char(b',');
            logger.print_int(lang_data.right_hyphen_min as i32);
            logger.print_char(b')');
        }
    }
    if layer.mode_line < 0 {
        logger.print_str(" (\\output routine)");
    }
}

/// See 219.
fn show_auxiliary_field(layer: &SemanticLevel, eqtb: &Eqtb, logger: &mut Logger) {
    match &layer.mode {
        RichMode::Vertical(vmode) => vmode.show_fields(logger),
        RichMode::Horizontal(hmode) => hmode.show_fields(logger),
        RichMode::Math(mmode) => mmode.show_fields(eqtb, logger),
    }
}
