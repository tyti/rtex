use crate::dimension::Dimension;
use crate::eqtb::{DimensionVariable, Eqtb, SkipVariable};
use crate::logger::Logger;
use crate::nodes::Node;
use crate::nodes::{GlueNode, ListNode, UnsetNode};
use crate::print::Printer;

/// If prev_depth has this value or a lower value, the next box is exempt from baseline
/// calculations.
/// See 212.
pub const IGNORE_DEPTH: i32 = -65536000; // -1000.0pt

#[derive(Debug)]
pub struct VerticalMode {
    pub list: Vec<Node>,
    pub internal: bool,
    pub prev_graf: i32,
    pub prev_depth: Dimension,
}

impl VerticalMode {
    pub const fn new_base() -> Self {
        Self {
            list: Vec::new(),
            internal: false,
            prev_graf: 0,
            prev_depth: IGNORE_DEPTH,
        }
    }

    pub fn new_internal() -> Self {
        Self {
            list: Vec::new(),
            internal: true,
            prev_graf: 0,
            prev_depth: IGNORE_DEPTH,
        }
    }

    pub fn new_internal_with_prev_depth(prev_depth: i32) -> Self {
        Self {
            list: Vec::new(),
            internal: true,
            prev_graf: 0,
            prev_depth,
        }
    }

    /// See 214.
    pub fn append_node(&mut self, node: Node, eqtb: &mut Eqtb) {
        self.list.push(node);
        self.update_last_node_info(eqtb);
    }

    /// See 679.
    pub fn append_to_vlist(&mut self, list_node: ListNode, eqtb: &mut Eqtb) {
        if self.prev_depth > IGNORE_DEPTH {
            let baseline_skip = eqtb.skips.get(SkipVariable::BaselineSkip);
            let d = baseline_skip.width - self.prev_depth - list_node.height;
            let interline_glue = if d < eqtb.dimen(DimensionVariable::LineSkipLimit) {
                GlueNode::new_param(SkipVariable::LineSkip, eqtb)
            } else {
                GlueNode::new_param_with_width(SkipVariable::BaselineSkip, d, eqtb)
            };
            self.append_node(Node::Glue(interline_glue), eqtb);
        }
        self.set_prev_depth(list_node.depth, eqtb);
        self.append_node(Node::List(list_node), eqtb);
    }

    /// See 679.
    pub fn append_unset_node_to_vlist(&mut self, unset_node: UnsetNode, eqtb: &mut Eqtb) {
        if self.prev_depth > IGNORE_DEPTH {
            let baseline_skip = eqtb.skips.get(SkipVariable::BaselineSkip);
            let d = baseline_skip.width - self.prev_depth - unset_node.height;
            let interline_glue = if d < eqtb.dimen(DimensionVariable::LineSkipLimit) {
                GlueNode::new_param(SkipVariable::LineSkip, eqtb)
            } else {
                GlueNode::new_param_with_width(SkipVariable::BaselineSkip, d, eqtb)
            };
            self.append_node(Node::Glue(interline_glue), eqtb);
        }
        self.set_prev_depth(unset_node.depth, eqtb);
        self.append_node(Node::Unset(unset_node), eqtb);
    }

    /// See 219.
    pub fn show_fields(&self, logger: &mut Logger) {
        logger.print_nl_str("prevdepth ");
        if self.prev_depth <= IGNORE_DEPTH {
            logger.print_str("ignored");
        } else {
            logger.print_scaled(self.prev_depth);
        }
        if self.prev_graf != 0 {
            logger.print_str(", prevgraf ");
            logger.print_int(self.prev_graf);
            logger.print_str(" line");
            if self.prev_graf != 1 {
                logger.print_char(b's');
            }
        }
    }

    pub fn set_prev_graf(&mut self, value: i32, eqtb: &mut Eqtb) {
        self.prev_graf = value;
        // Keep copy in Eqtb updated
        eqtb.prev_graf = self.prev_graf;
    }

    pub fn increment_prev_graf(&mut self, increment: i32, eqtb: &mut Eqtb) {
        self.prev_graf += increment;
        // Keep copy in Eqtb updated
        eqtb.prev_graf = self.prev_graf;
    }

    pub fn set_prev_depth(&mut self, value: Dimension, eqtb: &mut Eqtb) {
        self.prev_depth = value;
        // Keep copy in Eqtb updated
        eqtb.prev_depth = self.prev_depth;
    }

    /// Updates the condensed info about the last node of the current list that we keep in the
    /// Eqtb.
    pub fn update_last_node_info(&mut self, eqtb: &mut Eqtb) {
        // If this is the base vertical mode and the contribution list is empty.
        if !self.internal && self.list.is_empty() {
            eqtb.last_node_info = eqtb.last_node_on_page.clone();
        } else {
            eqtb.update_last_node_info(self.list.last());
        }
    }
}
