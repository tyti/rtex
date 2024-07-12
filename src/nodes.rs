pub mod noads;

use crate::dimension::{is_running, scan_normal_dimen, Dimension, NULL_FLAG};
use crate::eqtb::Eqtb;
use crate::eqtb::{ControlSequence, FontIndex, RegisterIndex, SkipVariable};
use crate::input::Scanner;
use crate::logger::Logger;
use crate::print::{Printer, MAX_PRINT_LINE};
use crate::round;
use crate::scaled::{nx_plus_y, ArithError, Scaled, UNITY};
use crate::token_lists::{show_token_list, RcTokenList};
use noads::{ChoiceNode, Noad, StyleNode};

use std::ops::{AddAssign, Neg};
use std::path::PathBuf;
use std::rc::Rc;

pub type GlueRatio = f64;

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Node {
    Char(CharNode),
    List(ListNode),
    Rule(RuleNode),
    Ins(InsNode),
    Mark(MarkNode),
    Adjust(AdjustNode),
    Ligature(LigatureNode),
    Disc(DiscNode),
    Whatsit(WhatsitNode),
    Math(MathNode),
    Glue(GlueNode),
    Kern(KernNode),
    Penalty(PenaltyNode),
    Unset(UnsetNode),
    Style(StyleNode),
    Choice(ChoiceNode),
    Noad(Noad),
}

impl Node {
    /// See 148.
    pub fn precedes_break(&self) -> bool {
        match self {
            Self::Char(_)
            | Self::List(_)
            | Self::Rule(_)
            | Self::Ins(_)
            | Self::Mark(_)
            | Self::Adjust(_)
            | Self::Ligature(_)
            | Self::Disc(_)
            | Self::Whatsit(_)
            // NOTE: As implicit kern nodes are treated as non-discardable, we make the distinction here.
            | Self::Kern(KernNode { subtype: KernSubtype::Normal | KernSubtype::AccKern | KernSubtype::MuGlue, .. })
            => true,
            _ => false,
        }
    }

    /// This is the vlist version of `precedes_break`.
    /// See 148.
    pub fn precedes_vertical_break(&self) -> bool {
        match self {
            Self::Char(_)
            | Self::List(_)
            | Self::Rule(_)
            | Self::Ins(_)
            | Self::Mark(_)
            | Self::Adjust(_)
            | Self::Ligature(_)
            | Self::Disc(_)
            | Self::Whatsit(_) => true,
            _ => false,
        }
    }

    /// Expects an item from an hlist.
    /// See 148.
    pub fn is_discardable(&self) -> bool {
        match self {
            Self::Char(_)
            | Self::List(_)
            | Self::Rule(_)
            | Self::Ins(_)
            | Self::Mark(_)
            | Self::Adjust(_)
            | Self::Ligature(_)
            | Self::Disc(_)
            | Self::Whatsit(_)
            // NOTE: As implicit kern nodes are treated as non-discardable, we make the distinction here.
            | Self::Kern(KernNode { subtype: KernSubtype::Normal | KernSubtype::AccKern | KernSubtype::MuGlue, .. })
            => false,
            Self::Glue(_)
            | Self::Kern(KernNode { subtype: KernSubtype::Explicit, .. })
            | Self::Penalty(_)
            | Self::Math(_)
                => true,
            _ => panic!("Should not happen"),
        }
    }

    /// See 183.
    fn display(
        &self,
        n: &mut usize,
        indent_str: &[u8],
        depth_max: i32,
        breadth_max: usize,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        match self {
            Node::Char(char_node) => char_node.display(eqtb, logger),
            Node::List(list_node) => {
                list_node.display(indent_str, depth_max, breadth_max, eqtb, logger)
            }
            Node::Unset(unset_node) => {
                unset_node.display(indent_str, depth_max, breadth_max, eqtb, logger)
            }
            Node::Rule(rule_node) => rule_node.display(logger),
            Node::Ins(ins_node) => {
                ins_node.display(indent_str, depth_max, breadth_max, eqtb, logger)
            }
            Node::Whatsit(whatsit_node) => whatsit_node.display(eqtb, logger),
            Node::Glue(glue_node) => {
                glue_node.display(indent_str, depth_max, breadth_max, eqtb, logger)
            }
            Node::Kern(kern_node) => kern_node.display(logger),
            Node::Math(math_node) => math_node.display(logger),
            Node::Ligature(ligature_node) => ligature_node.display(eqtb, logger),
            Node::Penalty(penalty_node) => penalty_node.display(logger),
            Node::Disc(disc_node) => {
                disc_node.display(n, indent_str, depth_max, breadth_max, eqtb, logger)
            }
            Node::Mark(mark_node) => mark_node.display(eqtb, logger),
            Node::Adjust(adjust_node) => {
                adjust_node.display(indent_str, depth_max, breadth_max, eqtb, logger)
            }
            Node::Style(style_node) => style_node.display(logger),
            Node::Choice(choice_node) => {
                choice_node.display(indent_str, depth_max, breadth_max, eqtb, logger)
            }
            Node::Noad(noad) => noad.display(indent_str, depth_max, breadth_max, eqtb, logger),
        }
    }
}

/// See 134.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CharNode {
    pub font_index: FontIndex,
    pub character: u8,
    pub width: Dimension,
    pub height: Dimension,
    pub depth: Dimension,
    pub italic: Dimension,
}

impl CharNode {
    /// See 176.
    fn display(&self, eqtb: &Eqtb, logger: &mut Logger) {
        print_font_and_char(self.font_index, self.character, eqtb, logger);
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub enum HlistOrVlist {
    Hlist(Vec<Node>),
    Vlist(Vec<Node>),
}

impl HlistOrVlist {
    pub fn is_hlist(&self) -> bool {
        match self {
            Self::Hlist(_) => true,
            Self::Vlist(_) => false,
        }
    }

    pub fn is_vlist(&self) -> bool {
        match self {
            Self::Hlist(_) => false,
            Self::Vlist(_) => true,
        }
    }
}

/// An Hlist or Vlist node. The distinction is made based on the type of list it contains.
/// See 135. and 137.
#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct ListNode {
    pub width: Dimension,
    pub height: Dimension,
    pub depth: Dimension,
    pub shift_amount: Dimension,
    pub list: HlistOrVlist,
    pub glue_set: GlueRatio,
    pub glue_sign: GlueSign,
    pub glue_order: DimensionOrder,
}

impl ListNode {
    /// See 136.
    pub fn new_empty_hbox() -> Self {
        Self {
            width: 0,
            height: 0,
            depth: 0,
            shift_amount: 0,
            list: HlistOrVlist::Hlist(Vec::new()),
            glue_set: 0.0,
            glue_sign: GlueSign::Normal,
            glue_order: DimensionOrder::Normal,
        }
    }

    /// See 136.
    pub fn new_empty_vbox() -> Self {
        Self {
            width: 0,
            height: 0,
            depth: 0,
            shift_amount: 0,
            list: HlistOrVlist::Vlist(Vec::new()),
            glue_set: 0.0,
            glue_sign: GlueSign::Normal,
            glue_order: DimensionOrder::Normal,
        }
    }

    /// See 184.
    fn display(
        &self,
        indent_str: &[u8],
        depth_max: i32,
        breadth_max: usize,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        let list = match self.list {
            HlistOrVlist::Hlist(ref list) => {
                logger.print_esc_str(b"hbox(");
                list
            }
            HlistOrVlist::Vlist(ref list) => {
                logger.print_esc_str(b"vbox(");
                list
            }
        };
        logger.print_scaled(self.height);
        logger.print_char(b'+');
        logger.print_scaled(self.depth);
        logger.print_str(")x");
        logger.print_scaled(self.width);
        display_value_of_glue_set(self.glue_set, self.glue_sign, self.glue_order, logger);
        if self.shift_amount != 0 {
            logger.print_str(", shifted ");
            logger.print_scaled(self.shift_amount);
        }
        node_list_display(list, indent_str, depth_max, breadth_max, eqtb, logger);
    }
}

/// See 138.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuleNode {
    pub width: Dimension,
    pub height: Dimension,
    pub depth: Dimension,
}

impl RuleNode {
    /// Corresponds to just below 0.4 pt.
    const DEFAULT_RULE: i32 = 26214;

    /// See 139.
    pub fn new() -> Self {
        RuleNode {
            width: NULL_FLAG,
            height: NULL_FLAG,
            depth: NULL_FLAG,
        }
    }

    /// See 176.
    fn print_rule_dimen(d: Dimension, logger: &mut Logger) {
        if is_running(d) {
            logger.print_char(b'*');
        } else {
            logger.print_scaled(d);
        }
    }

    /// See 187.
    fn display(&self, logger: &mut Logger) {
        logger.print_esc_str(b"rule(");
        Self::print_rule_dimen(self.height, logger);
        logger.print_char(b'+');
        Self::print_rule_dimen(self.depth, logger);
        logger.print_str(")x");
        Self::print_rule_dimen(self.width, logger);
    }

    /// See 463.
    pub fn scan_vrule_spec(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) -> Self {
        let mut rule_node = RuleNode::new();
        rule_node.width = Self::DEFAULT_RULE;
        loop {
            if scanner.scan_keyword(b"width", eqtb, logger) {
                let value = scan_normal_dimen(scanner, eqtb, logger);
                rule_node.width = value;
                continue;
            }
            if scanner.scan_keyword(b"height", eqtb, logger) {
                let value = scan_normal_dimen(scanner, eqtb, logger);
                rule_node.height = value;
                continue;
            }
            if scanner.scan_keyword(b"depth", eqtb, logger) {
                let value = scan_normal_dimen(scanner, eqtb, logger);
                rule_node.depth = value;
                continue;
            }
            break;
        }
        rule_node
    }

    /// See 463.
    pub fn scan_hrule_spec(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) -> Self {
        let mut rule_node = RuleNode::new();
        rule_node.height = Self::DEFAULT_RULE;
        rule_node.depth = 0;
        loop {
            if scanner.scan_keyword(b"width", eqtb, logger) {
                let value = scan_normal_dimen(scanner, eqtb, logger);
                rule_node.width = value;
                continue;
            }
            if scanner.scan_keyword(b"height", eqtb, logger) {
                let value = scan_normal_dimen(scanner, eqtb, logger);
                rule_node.height = value;
                continue;
            }
            if scanner.scan_keyword(b"depth", eqtb, logger) {
                let value = scan_normal_dimen(scanner, eqtb, logger);
                rule_node.depth = value;
                continue;
            }
            break;
        }
        rule_node
    }
}

/// See 140.
#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct InsNode {
    pub number: RegisterIndex,
    /// The natural height plus depth of the contained vertical list.
    pub size: Dimension,
    pub split_max_depth: Dimension,
    pub split_top_skip: std::rc::Rc<GlueSpec>,
    pub insertion: Vec<Node>,
    pub float_cost: i32,
}

impl InsNode {
    /// See 188.
    fn display(
        &self,
        indent_str: &[u8],
        depth_max: i32,
        breadth_max: usize,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        logger.print_esc_str(b"insert");
        logger.print_int(self.number as i32);
        logger.print_str(", natural size ");
        logger.print_scaled(self.size);
        logger.print_str("; split(");
        self.split_top_skip.print_spec(None, logger);
        logger.print_char(b',');
        logger.print_scaled(self.split_max_depth);
        logger.print_str("); float cost ");
        logger.print_int(self.float_cost);
        node_list_display(
            &self.insertion,
            indent_str,
            depth_max,
            breadth_max,
            eqtb,
            logger,
        );
    }
}

/// See 141.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MarkNode {
    pub mark: RcTokenList,
}

impl MarkNode {
    /// See 196.
    fn display(&self, eqtb: &Eqtb, logger: &mut Logger) {
        logger.print_esc_str(b"mark");
        print_mark(&self.mark, eqtb, logger);
    }
}

/// See 142.
#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct AdjustNode {
    pub adjustment: Vec<Node>,
}

impl AdjustNode {
    /// See 197.
    fn display(
        &self,
        indent_str: &[u8],
        depth_max: i32,
        breadth_max: usize,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        logger.print_esc_str(b"vadjust");
        node_list_display(
            &self.adjustment,
            indent_str,
            depth_max,
            breadth_max,
            eqtb,
            logger,
        );
    }
}

/// See 143.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LigatureNode {
    pub font_index: FontIndex,
    pub character: u8,
    pub width: Dimension,
    pub height: Dimension,
    pub depth: Dimension,
    pub italic: Dimension,
    pub lig: Vec<u8>,
    pub left_boundary: bool,
    pub right_boundary: bool,
}

impl LigatureNode {
    /// See 193.
    fn display(&self, eqtb: &Eqtb, logger: &mut Logger) {
        print_font_and_char(self.font_index, self.character, eqtb, logger);
        logger.print_str(" (ligature ");
        if self.left_boundary {
            logger.print_char(b'|');
        }
        for &character in &self.lig {
            logger.print(character);
        }
        if self.right_boundary {
            logger.print_char(b'|');
        }
        logger.print_char(b')');
    }
}

/// See 145.
#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct DiscNode {
    pub pre_break: Vec<Node>,
    pub post_break: Vec<Node>,
    pub no_break: Vec<Node>,
}

impl DiscNode {
    /// See 145.
    pub fn new() -> Self {
        Self {
            pre_break: Vec::new(),
            post_break: Vec::new(),
            no_break: Vec::new(),
        }
    }

    /// See 195.
    fn display(
        &self,
        n: &mut usize,
        indent_str: &[u8],
        depth_max: i32,
        breadth_max: usize,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        logger.print_esc_str(b"discretionary");
        if !self.no_break.is_empty() {
            logger.print_str(" replacing ");
            logger.print_int(self.no_break.len() as i32);
        }
        node_list_display(
            &self.pre_break,
            indent_str,
            depth_max,
            breadth_max,
            eqtb,
            logger,
        );
        let mut post_indent_str = indent_str.to_vec();
        post_indent_str.push(b'|');
        show_node_list(
            &self.post_break,
            &post_indent_str,
            depth_max,
            breadth_max,
            eqtb,
            logger,
        );
        for node in &self.no_break {
            logger.print_ln();
            print_indentation(indent_str, logger);
            *n += 1;
            node.display(n, indent_str, depth_max, breadth_max, eqtb, logger);
            if *n == breadth_max {
                return;
            }
        }
    }
}

/// See 146. and 1341.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WhatsitNode {
    Open(OpenNode),
    Write(WriteNode),
    Close(CloseNode),
    Special(SpecialNode),
    Language(LanguageNode),
}

impl WhatsitNode {
    /// See 1356.
    fn display(&self, eqtb: &Eqtb, logger: &mut Logger) {
        match self {
            Self::Open(open_node) => open_node.display(logger),
            Self::Write(write_node) => write_node.display(eqtb, logger),
            Self::Close(close_node) => close_node.display(logger),
            Self::Special(special_node) => special_node.display(eqtb, logger),
            Self::Language(language_node) => language_node.display(logger),
        }
    }
}

/// See 147.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MathNode {
    pub kind: MathNodeKind,
    pub width: Dimension,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MathNodeKind {
    Before,
    After,
}

impl MathNode {
    /// See 192.
    fn display(&self, logger: &mut Logger) {
        logger.print_esc_str(b"math");
        match self.kind {
            MathNodeKind::Before => logger.print_str("on"),
            MathNodeKind::After => logger.print_str("off"),
        }
        if self.width != 0 {
            logger.print_str(", surrounded ");
            logger.print_scaled(self.width);
        }
    }
}

/// See 149.
#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct GlueNode {
    pub subtype: GlueType,
    pub glue_spec: std::rc::Rc<GlueSpec>,
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub enum GlueType {
    Normal,
    Skip(SkipVariable),
    NonScript,
    MuGlue,
    Leaders {
        leader_kind: LeaderKind,
        leader_node: Box<Node>,
    },
}

/// See 149.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LeaderKind {
    ALeaders,
    XLeaders,
    CLeaders,
}

impl LeaderKind {
    pub fn display(&self, printer: &mut impl Printer) {
        match self {
            Self::ALeaders => printer.print_esc_str(b"leaders"),
            Self::XLeaders => printer.print_esc_str(b"xleaders"),
            Self::CLeaders => printer.print_esc_str(b"cleaders"),
        }
    }
}

impl GlueNode {
    /// See 153.
    pub fn new(glue_spec: std::rc::Rc<GlueSpec>) -> Self {
        Self {
            subtype: GlueType::Normal,
            glue_spec,
        }
    }

    /// Returns a GlueNode with the GlueSpec of the corresponding variable.
    /// See 152.
    pub fn new_param(skip_var: SkipVariable, eqtb: &Eqtb) -> Self {
        let glue_spec = eqtb.skips.get(skip_var).clone();
        Self {
            subtype: GlueType::Skip(skip_var),
            glue_spec,
        }
    }
    /// Returns a GlueNode with a copy of the GlueSpec of the corresponding variable changed to the
    /// given width.
    /// See 154.
    pub fn new_param_with_width(skip_var: SkipVariable, width: Dimension, eqtb: &Eqtb) -> Self {
        let mut glue_spec = (**eqtb.skips.get(skip_var)).clone();
        glue_spec.width = width;
        Self {
            subtype: GlueType::Skip(skip_var),
            glue_spec: Rc::new(glue_spec),
        }
    }

    /// See 190.
    fn display_leaders(
        &self,
        leader_kind: LeaderKind,
        leader_node: &Node,
        indent_str: &[u8],
        depth_max: i32,
        breadth_max: usize,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        logger.print_esc_str(b"");
        if leader_kind == LeaderKind::CLeaders {
            logger.print_char(b'c');
        } else if leader_kind == LeaderKind::XLeaders {
            logger.print_char(b'x');
        }
        logger.print_str("leaders ");
        self.glue_spec.print_spec(None, logger);
        node_list_display(
            std::slice::from_ref(leader_node),
            indent_str,
            depth_max,
            breadth_max,
            eqtb,
            logger,
        );
    }

    /// See 189.
    fn display(
        &self,
        indent_str: &[u8],
        depth_max: i32,
        breadth_max: usize,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        match self.subtype {
            GlueType::Normal => {
                logger.print_esc_str(b"glue");
                logger.print_char(b' ');
                self.glue_spec.print_spec(None, logger);
            }
            GlueType::Skip(skip_var) => {
                logger.print_esc_str(b"glue");
                logger.print_char(b'(');
                logger.print_esc_str(&skip_var.to_string());
                logger.print_char(b')');
                logger.print_char(b' ');
                self.glue_spec.print_spec(None, logger);
            }
            GlueType::NonScript => {
                logger.print_esc_str(b"glue");
                logger.print_char(b'(');
                logger.print_esc_str(b"nonscript");
                logger.print_char(b')');
            }
            GlueType::MuGlue => {
                logger.print_esc_str(b"glue");
                logger.print_char(b'(');
                logger.print_esc_str(b"mskip");
                logger.print_char(b')');
                logger.print_char(b' ');
                self.glue_spec.print_spec(Some("mu"), logger);
            }
            GlueType::Leaders {
                leader_kind,
                ref leader_node,
            } => self.display_leaders(
                leader_kind,
                leader_node,
                indent_str,
                depth_max,
                breadth_max,
                eqtb,
                logger,
            ),
        }
    }
}

/// See 150.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HigherOrderDimension {
    pub order: DimensionOrder,
    pub value: Scaled,
}

impl Default for HigherOrderDimension {
    fn default() -> Self {
        Self {
            order: DimensionOrder::Normal,
            value: 0,
        }
    }
}

/// See 150.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DimensionOrder {
    Normal,
    Fil,
    Fill,
    Filll,
}

impl HigherOrderDimension {
    const DEFAULT: Self = Self {
        order: DimensionOrder::Normal,
        value: 0,
    };

    pub const fn is_zero(&self) -> bool {
        self.value == 0
    }

    pub fn is_infinite(&self) -> bool {
        self.order != DimensionOrder::Normal && self.value != 0
    }

    fn normalize(&mut self) {
        if self.value == 0 {
            self.order = DimensionOrder::Normal;
        }
    }

    /// See 1240.
    pub fn multiply(&mut self, factor: i32) -> Result<(), ArithError> {
        self.value = nx_plus_y(self.value, factor, 0)?;
        Ok(())
    }

    /// Prints a glue scalar with units.
    /// We use &[u8] instead of string numbers as in the TeX82 implementation
    /// and hope that it won't come haunting us.
    /// See 177.
    fn display(&self, unit: Option<&str>, printer: &mut impl Printer) {
        printer.print_scaled(self.value);
        match self.order {
            DimensionOrder::Normal => {
                if let Some(unit) = unit {
                    printer.print_str(unit);
                }
            }
            DimensionOrder::Fil => {
                printer.print_str("fil");
            }
            DimensionOrder::Fill => {
                printer.print_str("fill");
            }
            DimensionOrder::Filll => {
                printer.print_str("filll");
            }
        }
    }
}

impl Neg for HigherOrderDimension {
    type Output = Self;
    fn neg(self) -> Self {
        Self {
            value: -self.value,
            order: self.order,
        }
    }
}

impl AddAssign for HigherOrderDimension {
    fn add_assign(&mut self, mut rhs: Self) {
        self.normalize();
        rhs.normalize();
        match (self.order, rhs.order) {
            (DimensionOrder::Normal, DimensionOrder::Normal)
            | (DimensionOrder::Fil, DimensionOrder::Fil)
            | (DimensionOrder::Fill, DimensionOrder::Fill)
            | (DimensionOrder::Filll, DimensionOrder::Filll) => self.value += rhs.value,
            (DimensionOrder::Normal, _)
            | (DimensionOrder::Fil, DimensionOrder::Fill)
            | (DimensionOrder::Fil, DimensionOrder::Filll)
            | (DimensionOrder::Fill, DimensionOrder::Filll) => {
                *self = rhs;
            }
            _ => {}
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum GlueSign {
    Normal,
    Stretching,
    Shrinking,
}

// NOTE The current handling of zero_glue is not entirely conformant with TeX82.
// Not every all-zero glue in TeX82 is necessarily zero_glue which leads to different
// treatment in e.g. printing and can cause deviating output/log files.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct GlueSpec {
    pub width: Dimension,
    pub stretch: HigherOrderDimension,
    pub shrink: HigherOrderDimension,
}

impl GlueSpec {
    pub const ZERO_GLUE: Self = Self {
        width: 0,
        stretch: HigherOrderDimension::DEFAULT,
        shrink: HigherOrderDimension::DEFAULT,
    };
    pub const FIL_GLUE: Self = Self {
        width: 0,
        stretch: HigherOrderDimension {
            order: DimensionOrder::Fil,
            value: UNITY,
        },
        shrink: HigherOrderDimension::DEFAULT,
    };
    pub const FILL_GLUE: Self = Self {
        width: 0,
        stretch: HigherOrderDimension {
            order: DimensionOrder::Fill,
            value: UNITY,
        },
        shrink: HigherOrderDimension::DEFAULT,
    };
    pub const SS_GLUE: Self = Self {
        width: 0,
        stretch: HigherOrderDimension {
            order: DimensionOrder::Fil,
            value: UNITY,
        },
        shrink: HigherOrderDimension {
            order: DimensionOrder::Fil,
            value: UNITY,
        },
    };
    pub const FIL_NEG_GLUE: Self = Self {
        width: 0,
        stretch: HigherOrderDimension {
            order: DimensionOrder::Fil,
            value: -UNITY,
        },
        shrink: HigherOrderDimension::DEFAULT,
    };

    /// Returns a copy of the GlueSpec where the shrinkage has been made finite.
    pub fn finite_shrink_copy(&self) -> Rc<Self> {
        Rc::new(Self {
            width: self.width,
            stretch: self.stretch,
            shrink: HigherOrderDimension {
                order: DimensionOrder::Normal,
                value: self.shrink.value,
            },
        })
    }

    /// See 431.
    pub fn negate(&self) -> Self {
        Self {
            width: -self.width,
            stretch: -self.stretch,
            shrink: -self.shrink,
        }
    }

    /// Prints a glue specification.
    /// See 178.
    pub fn print_spec(&self, unit: Option<&str>, printer: &mut impl Printer) {
        printer.print_scaled(self.width);
        if let Some(unit) = unit {
            printer.print_str(unit);
        }
        if !self.stretch.is_zero() {
            printer.print_str(" plus ");
            self.stretch.display(unit, printer);
        }
        if !self.shrink.is_zero() {
            printer.print_str(" minus ");
            self.shrink.display(unit, printer);
        }
    }

    pub fn zero_glue() -> std::rc::Rc<Self> {
        std::rc::Rc::new(Self::ZERO_GLUE.clone())
    }

    pub fn fil_glue() -> std::rc::Rc<Self> {
        std::rc::Rc::new(Self::FIL_GLUE.clone())
    }

    pub fn fill_glue() -> std::rc::Rc<Self> {
        std::rc::Rc::new(Self::FILL_GLUE.clone())
    }

    pub fn ss_glue() -> std::rc::Rc<Self> {
        std::rc::Rc::new(Self::SS_GLUE.clone())
    }

    pub fn fil_neg_glue() -> std::rc::Rc<Self> {
        std::rc::Rc::new(Self::FIL_NEG_GLUE.clone())
    }
}

/// See 155.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct KernNode {
    pub subtype: KernSubtype,
    pub width: Dimension,
}

impl KernNode {
    /// See 156.
    pub fn new(width: Dimension) -> Self {
        Self {
            subtype: KernSubtype::Normal,
            width,
        }
    }

    /// See 191.
    fn display(&self, logger: &mut Logger) {
        match self.subtype {
            KernSubtype::Normal => {
                logger.print_esc_str(b"kern");
                logger.print_scaled(self.width);
            }
            KernSubtype::Explicit => {
                logger.print_esc_str(b"kern");
                logger.print_char(b' ');
                logger.print_scaled(self.width);
            }
            KernSubtype::AccKern => {
                logger.print_esc_str(b"kern");
                logger.print_char(b' ');
                logger.print_scaled(self.width);
                logger.print_str(" (for accent)");
            }
            KernSubtype::MuGlue => {
                logger.print_esc_str(b"mkern");
                logger.print_scaled(self.width);
                logger.print_str("mu");
            }
        }
    }
}

/// See 155.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KernSubtype {
    Normal,
    Explicit,
    AccKern,
    MuGlue,
}

/// See 157.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PenaltyNode {
    pub penalty: i32,
}

impl PenaltyNode {
    /// See 158.
    pub fn new(penalty: i32) -> Self {
        Self { penalty }
    }

    /// See 194.
    fn display(&self, logger: &mut Logger) {
        logger.print_esc_str(b"penalty ");
        logger.print_int(self.penalty);
    }
}

/// See 159.
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct UnsetNode {
    pub width: Dimension,
    pub height: Dimension,
    pub depth: Dimension,
    pub list: Vec<Node>,
    pub stretch: HigherOrderDimension,
    pub shrink: HigherOrderDimension,
    pub span_count: usize,
}

impl UnsetNode {
    pub fn from_list_node(list_node: ListNode) -> Self {
        let list = match list_node.list {
            HlistOrVlist::Hlist(list) => list,
            HlistOrVlist::Vlist(list) => list,
        };
        Self {
            width: list_node.width,
            height: list_node.height,
            depth: list_node.depth,
            list,
            stretch: HigherOrderDimension::default(),
            shrink: HigherOrderDimension::default(),
            span_count: 0,
        }
    }

    /// See 184. and 185.
    fn display(
        &self,
        indent_str: &[u8],
        depth_max: i32,
        breadth_max: usize,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        logger.print_esc_str(b"unsetbox(");
        logger.print_scaled(self.height);
        logger.print_char(b'+');
        logger.print_scaled(self.depth);
        logger.print_str(")x");
        logger.print_scaled(self.width);
        // From 185.
        if self.span_count != 0 {
            logger.print_str(" (");
            logger.print_int(self.span_count as i32 + 1);
            logger.print_str(" columns)");
        }
        if self.stretch.value != 0 {
            logger.print_str(", stretch ");
            print_glue(self.stretch.value, self.stretch.order, None, logger);
        }
        if self.shrink.value != 0 {
            logger.print_str(", shrink ");
            print_glue(self.shrink.value, self.shrink.order, None, logger);
        }

        node_list_display(&self.list, indent_str, depth_max, breadth_max, eqtb, logger);
    }
}

// NOTE This is only needed so that we can implement Clone for Node but it should be
// impossible that cloning is every invoked on this type of Node.
impl Clone for UnsetNode {
    fn clone(&self) -> Self {
        panic!("It should not happen that these get copied in copy_node_list")
    }
}

/// See 174. and 175.
pub fn short_display(node_list: &[Node], eqtb: &Eqtb, logger: &mut Logger) {
    for node in node_list {
        match node {
            Node::Char(char_node) => {
                if char_node.font_index != logger.font_in_short_display {
                    print_font_identifier(char_node.font_index, eqtb, logger);
                    logger.print_char(b' ');
                    logger.font_in_short_display = char_node.font_index;
                }
                logger.print(char_node.character);
            }
            Node::List(_)
            | Node::Ins(_)
            | Node::Whatsit(_)
            | Node::Mark(_)
            | Node::Adjust(_)
            | Node::Unset(_) => {
                logger.print_str("[]");
            }
            Node::Rule(_) => {
                logger.print_char(b'|');
            }
            Node::Glue(glue_node) => {
                if *glue_node.glue_spec != GlueSpec::ZERO_GLUE {
                    logger.print_char(b' ');
                }
            }
            Node::Math(_) => {
                logger.print_char(b'$');
            }
            Node::Ligature(ligature_node) => {
                if ligature_node.font_index != logger.font_in_short_display {
                    print_font_identifier(ligature_node.font_index, eqtb, logger);
                    logger.print_char(b' ');
                    logger.font_in_short_display = ligature_node.font_index;
                }
                for &character in &ligature_node.lig {
                    logger.print(character);
                }
            }
            Node::Disc(disc_node) => {
                short_display(&disc_node.pre_break, eqtb, logger);
                short_display(&disc_node.post_break, eqtb, logger);
            }
            _ => {}
        }
    }
}

/// 176.
fn print_font_and_char(font_index: FontIndex, character: u8, eqtb: &Eqtb, logger: &mut Logger) {
    print_font_identifier(font_index, eqtb, logger);
    logger.print_char(b' ');
    logger.print(character);
}

/// See 180.
fn node_list_display(
    node_list: &[Node],
    indent_str: &[u8],
    depth_max: i32,
    breadth_max: usize,
    eqtb: &Eqtb,
    logger: &mut Logger,
) {
    let mut list_indent_str = indent_str.to_vec();
    list_indent_str.push(b'.');
    show_node_list(
        node_list,
        &list_indent_str,
        depth_max,
        breadth_max,
        eqtb,
        logger,
    );
}

/// See 182.
pub fn show_node_list(
    node_list: &[Node],
    indent_str: &[u8],
    depth_max: i32,
    breadth_max: usize,
    eqtb: &Eqtb,
    logger: &mut Logger,
) {
    if indent_str.len() as i32 > depth_max {
        if !node_list.is_empty() {
            logger.print_str(" []");
        }
        return;
    }
    let mut n = 0;
    for node in node_list {
        logger.print_ln();
        print_indentation(indent_str, logger);
        n += 1;
        if n > breadth_max {
            logger.print_str("etc.");
            return;
        }
        node.display(&mut n, indent_str, depth_max, breadth_max, eqtb, logger);
    }
}

/// A version of `show_node_list` for showing a single ListNode.
/// See 182.
pub fn show_list_node(
    list_node: &ListNode,
    indent_str: &[u8],
    depth_max: i32,
    breadth_max: usize,
    eqtb: &Eqtb,
    logger: &mut Logger,
) {
    if indent_str.len() as i32 > depth_max {
        logger.print_str(" []");
        return;
    }
    logger.print_ln();
    print_indentation(indent_str, logger);
    if 1 > breadth_max {
        logger.print_str("etc.");
        return;
    }
    list_node.display(indent_str, depth_max, breadth_max, eqtb, logger);
}

/// See 186.
fn display_value_of_glue_set(
    glue_set: f64,
    glue_sign: GlueSign,
    glue_order: DimensionOrder,
    logger: &mut Logger,
) {
    if glue_set != 0.0 && glue_sign != GlueSign::Normal {
        logger.print_str(", glue set ");
        if glue_sign == GlueSign::Shrinking {
            logger.print_str("- ");
        }
        if !glue_set.is_normal() {
            logger.print_str("?.?");
        } else {
            if glue_set.abs() > 20000.0 {
                if glue_set > 0.0 {
                    logger.print_char(b'>');
                } else {
                    logger.print_str("< -");
                }
                print_glue(20000 * UNITY, glue_order, None, logger);
            } else {
                print_glue(round(UNITY as f64 * glue_set), glue_order, None, logger);
            }
        }
    }
}

/// See 267.
fn print_font_identifier(font_index: FontIndex, eqtb: &Eqtb, logger: &mut Logger) {
    let cs = ControlSequence::FontId(font_index);
    let text = eqtb.control_sequences.text(cs);
    logger.print_esc_str(text);
}

/// See 1341.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpenNode {
    pub write_stream: usize,
    pub path: PathBuf,
}

impl OpenNode {
    /// See 1355. and 1356.
    fn display(&self, logger: &mut Logger) {
        logger.print_esc_str(b"openout");
        print_write_whatsit(self.write_stream, logger);
        logger.print_char(b'=');
        logger.print_file_name(&self.path);
    }
}

/// See 1341.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WriteNode {
    pub write_stream: usize,
    pub tokens: RcTokenList,
}

impl WriteNode {
    /// See 1355. and 1356.
    fn display(&self, eqtb: &Eqtb, logger: &mut Logger) {
        logger.print_esc_str(b"write");
        print_write_whatsit(self.write_stream, logger);
        print_mark(&self.tokens, eqtb, logger);
    }
}

/// See 1341.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CloseNode {
    pub write_stream: usize,
}

impl CloseNode {
    /// See 1355. and 1356.
    fn display(&self, logger: &mut Logger) {
        logger.print_esc_str(b"closeout");
        print_write_whatsit(self.write_stream, logger);
    }
}

/// See 1341.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpecialNode {
    pub tokens: RcTokenList,
}
impl SpecialNode {
    /// See 1355. and 1356.
    fn display(&self, eqtb: &Eqtb, logger: &mut Logger) {
        logger.print_esc_str(b"special");
        print_mark(&self.tokens, eqtb, logger);
    }
}

/// See 1341.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LanguageNode {
    pub language: usize,
    pub left_hyphen_min: usize,
    pub right_hyphen_min: usize,
}

impl LanguageNode {
    /// See 1356.
    fn display(&self, logger: &mut Logger) {
        logger.print_esc_str(b"setlanguage");
        logger.print_int(self.language as i32);
        logger.print_str(" (hyphenmin ");
        logger.print_int(self.left_hyphen_min as i32);
        logger.print_char(b',');
        logger.print_int(self.right_hyphen_min as i32);
        logger.print_char(b')');
    }
}

/// See 1355.
fn print_write_whatsit(write_stream: usize, logger: &mut Logger) {
    if write_stream < 16 {
        logger.print_int(write_stream as i32);
    } else if write_stream == 16 {
        logger.print_char(b'*');
    } else {
        logger.print_char(b'-');
    }
}

fn print_indentation(indent_str: &[u8], logger: &mut Logger) {
    for &c in indent_str {
        logger.print_char(c);
    }
}

/// See 176.
fn print_mark(token_list: &RcTokenList, eqtb: &Eqtb, logger: &mut Logger) {
    logger.print_char(b'{');
    show_token_list(token_list, MAX_PRINT_LINE - 10, logger, eqtb);
    logger.print_char(b'}');
}

/// Prints a glue scalar with units.
/// We use &[u8] instead of string numbers as in the TeX82 implementation
/// and hope that it won't come haunting us.
/// See 177.
fn print_glue(d: Scaled, order: DimensionOrder, unit: Option<&str>, printer: &mut impl Printer) {
    printer.print_scaled(d);
    match order {
        DimensionOrder::Normal => {
            if let Some(unit) = unit {
                printer.print_str(unit);
            }
        }
        DimensionOrder::Fil => printer.print_str("fil"),
        DimensionOrder::Fill => printer.print_str("fill"),
        DimensionOrder::Filll => printer.print_str("filll"),
    }
}

/// Show a list of nodes.
/// See 198.
pub fn show_box_content(content: &[Node], eqtb: &Eqtb, logger: &mut Logger) {
    let (max_depth, max_breadth) = eqtb.get_max_depth_and_breadth();
    show_node_list(content, b"", max_depth, max_breadth, eqtb, logger);
    logger.print_ln();
}

/// Show a single ListNode.
/// See 198.
pub fn show_box(list_node: &ListNode, eqtb: &Eqtb, logger: &mut Logger) {
    let (max_depth, max_breadth) = eqtb.get_max_depth_and_breadth();
    show_list_node(list_node, b"", max_depth, max_breadth, eqtb, logger);
    logger.print_ln();
}
