use crate::box_building::{end_graf, normal_paragraph};
use crate::command::MarkCommand;
use crate::dimension::{Dimension, MAX_DIMEN};
use crate::eqtb::save_stack::GroupType;
use crate::eqtb::{
    BoxVariable, DimensionVariable, Eqtb, IntegerVariable, LastNodeInfo, RegisterIndex,
    SkipVariable, TokenListVariable,
};
use crate::hyphenation::Hyphenator;
use crate::input::token_source::TokenSourceType;
use crate::input::Scanner;
use crate::line_breaking::{AWFUL_BAD, EJECT_PENALTY, INF_PENALTY};
use crate::logger::Logger;
use crate::nodes::{
    show_box, show_box_content, DimensionOrder, GlueNode, HlistOrVlist, InsNode, KernNode,
    ListNode, MarkNode, Node, RuleNode,
};
use crate::output::Output;
use crate::packaging::{measure_vlist, vpack, vpackage, GlueCollector, TargetSpec};
use crate::print::Printer;
use crate::scaled::{calculate_badness, Scaled, INF_BAD};
use crate::semantic_nest::{RichMode, SemanticState};
use crate::token_lists::RcTokenList;
use crate::vertical_mode::VerticalMode;
use crate::vsplit::{prune_page_top, vert_break, DEPLORABLE};

use std::collections::BTreeMap;

#[derive(Debug)]
pub struct PageBuilder {
    pub page: Vec<Node>,
    page_ins_list: BTreeMap<RegisterIndex, PageIns>,
    max_depth: Dimension,
    best_break: usize,
    least_cost: i32,
    best_size: Dimension,
}

/// See 980.
#[derive(Debug, PartialEq)]
pub enum PageContents {
    Empty,
    InsertsOnly,
    BoxThere,
}

impl PageBuilder {
    pub const fn new() -> Self {
        Self {
            page: Vec::new(),
            page_ins_list: BTreeMap::new(),
            max_depth: 0,
            best_break: 0,
            least_cost: AWFUL_BAD,
            best_size: 0,
        }
    }
}

/// See 981.
#[derive(Debug)]
struct PageIns {
    typ: InsType,
    height: Dimension,
    ins_count: usize,
    best_ins_count: usize,
}

#[derive(Debug)]
enum InsType {
    Inserting,
    SplitUp {
        broken_count: usize,
        inner_break_pos: usize,
    },
}

/// See 982.
#[derive(Debug)]
pub struct PageDimensions {
    pub page_goal: Dimension,
    pub height: Dimension,
    pub depth: Dimension,
    pub stretch: GlueCollector,
    pub shrink: Dimension,
}

impl PageDimensions {
    pub const fn new() -> Self {
        Self {
            page_goal: 0,
            height: 0,
            depth: 0,
            stretch: GlueCollector::new(),
            shrink: 0,
        }
    }

    /// See 985.
    fn print_totals(&self, logger: &mut Logger) {
        logger.print_scaled(self.height);
        Self::print_plus(self.stretch.normal, "", logger);
        Self::print_plus(self.stretch.fil, "fil", logger);
        Self::print_plus(self.stretch.fill, "fill", logger);
        Self::print_plus(self.stretch.filll, "filll", logger);
        if self.shrink != 0 {
            logger.print_str(" minus ");
            logger.print_scaled(self.shrink);
        }
    }

    /// See 985.
    fn print_plus(val: i32, s: &str, logger: &mut Logger) {
        if val != 0 {
            logger.print_str(" plus ");
            logger.print_scaled(val);
            logger.print_str(s);
        }
    }
}

impl PageBuilder {
    /// See 986.
    pub fn show_status_of_current_page(&self, eqtb: &Eqtb, logger: &mut Logger) {
        if !self.page.is_empty() {
            logger.print_nl_str("### current page:");
            if eqtb.output_active {
                logger.print_str(" (held over for next output)");
            }
            show_box_content(&self.page, eqtb, logger);
            if eqtb.page_contents != PageContents::Empty {
                logger.print_nl_str("total height ");
                eqtb.page_dims.print_totals(logger);
                logger.print_nl_str(" goal height ");
                logger.print_scaled(eqtb.page_dims.page_goal);
                for (&number, page_ins) in &self.page_ins_list {
                    logger.print_ln();
                    logger.print_esc_str(b"insert");
                    logger.print_int(number as i32);
                    logger.print_str(" adds ");
                    let s = if eqtb.integer(IntegerVariable::Count(number)) == 1000 {
                        page_ins.height
                    } else {
                        page_ins.height / 1000 * eqtb.integer(IntegerVariable::Count(number))
                    };
                    logger.print_scaled(s);
                    if let InsType::SplitUp { broken_count, .. } = page_ins.typ {
                        logger.print_str(", #");
                        logger.print_int(broken_count as i32);
                        logger.print_str(" might split");
                    }
                }
            }
        }
    }

    /// See 987.
    fn freeze_page_specs(&mut self, eqtb: &mut Eqtb, logger: &mut Logger) {
        eqtb.page_dims = PageDimensions::new();
        eqtb.page_dims.page_goal = eqtb.dimen(DimensionVariable::Vsize);
        self.max_depth = eqtb.dimen(DimensionVariable::MaxDepth);
        self.least_cost = AWFUL_BAD;
        if cfg!(feature = "stats") {
            if eqtb.integer(IntegerVariable::TracingPages) > 0 {
                logger.begin_diagnostic(eqtb.tracing_online());
                logger.print_nl_str("%% goal height=");
                logger.print_scaled(eqtb.page_dims.page_goal);
                logger.print_str(", max depth=");
                logger.print_scaled(self.max_depth);
                logger.end_diagnostic(false);
            }
        }
    }

    /// See 991.
    fn start_new_current_page(&mut self, eqtb: &mut Eqtb) {
        eqtb.page_contents = PageContents::Empty;
        self.page.clear();
        eqtb.last_node_on_page = LastNodeInfo::Other;
        eqtb.page_dims.depth = 0;
        self.max_depth = 0;
    }

    /// See 992.
    fn box_error(
        list_node: ListNode,
        help: &[&str],
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        logger.error(help, scanner, eqtb);
        logger.begin_diagnostic(eqtb.tracing_online());
        logger.print_nl_str("The following box has been deleted:");
        show_box(&list_node, eqtb, logger);
        logger.end_diagnostic(true);
    }

    /// See 993.
    fn ensure_vbox(n: RegisterIndex, scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) {
        let p = eqtb.boks(n);
        if let Some(list_node) = p {
            if let HlistOrVlist::Hlist(_) = list_node.list {
                logger.print_err("Insertions can only be added to a vbox");
                let help = &[
                    "Tut tut: You're trying to \\insert into a",
                    "\\box register that now contains an \\hbox.",
                    "Proceed, and I'll discard its present contents.",
                ];
                let list_node = eqtb.boxes.set(BoxVariable(n), None).unwrap();
                Self::box_error(list_node, help, scanner, eqtb, logger);
            }
        }
    }

    /// See 994.
    pub fn build_page(
        &mut self,
        output: &mut Output,
        nest: &mut SemanticState,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        if nest.base_list().is_empty() || eqtb.output_active {
            return;
        }

        let mut contributions = get_contributions_stack(nest);
        while let Some(node) = contributions.pop() {
            self.update_values_of_last_glue_penalty_and_kern(&node, eqtb);

            if eqtb.page_contents != PageContents::BoxThere {
                self.move_only_non_discardables_to_page(
                    node,
                    &mut contributions,
                    scanner,
                    eqtb,
                    logger,
                );
            } else if !self.move_node_to_current_page(
                node,
                &mut contributions,
                output,
                nest,
                scanner,
                eqtb,
                logger,
            ) {
                move_contributions_back(contributions, nest);
                break;
            }
        }
        // We need to update the last node info as the value of the penalty of a breaking penalty node
        // is changed to INF_PENALTY and this might be the last node.
        nest.update_last_node_info(eqtb);
    }
}

impl PageBuilder {
    /// See 996.
    fn update_values_of_last_glue_penalty_and_kern(&mut self, node: &Node, eqtb: &mut Eqtb) {
        eqtb.last_node_on_page = match node {
            Node::Glue(glue_node) => LastNodeInfo::Glue(glue_node.glue_spec.clone()),
            Node::Penalty(penalty_node) => LastNodeInfo::Penalty(penalty_node.penalty),
            Node::Kern(kern_node) => LastNodeInfo::Kern(kern_node.width),
            _ => LastNodeInfo::Other,
        }
    }

    /// Returns false for return, else true.
    /// See 997.
    fn move_node_to_current_page(
        &mut self,
        mut node: Node,
        contributions: &mut Vec<Node>,
        output: &mut Output,
        nest: &mut SemanticState,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> bool {
        // If a KernNode is the last Node in the contribution list, we leave it there.
        if let Node::Kern(_) = node {
            if contributions.is_empty() {
                contributions.push(node);
                return false;
            }
        }

        // First check whether we have a break point.
        if let Some(penalty) = self.check_if_break_is_possible(&node, contributions) {
            let is_ready_for_output = self.update_break_points(penalty, eqtb, logger);
            if is_ready_for_output {
                self.set_value_of_output_penalty(&mut node, eqtb, logger);

                // Move node back to contributions.
                contributions.push(node);

                // Try to make a page.
                self.fire_up(contributions, output, nest, scanner, eqtb, logger);

                // Now the user-supplied output routine does its work.
                if eqtb.output_active {
                    return false;
                // The default output routine is already done.
                } else {
                    return true;
                }
            }
        }

        self.update_measurements_and_insertions(&mut node, scanner, eqtb, logger);
        self.make_sure_page_max_depth_not_exceeded(eqtb);
        self.page.push(node);
        true
    }

    /// See 1000.
    fn move_only_non_discardables_to_page(
        &mut self,
        mut node: Node,
        contributions: &mut Vec<Node>,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        match node {
            Node::List(ListNode { height, .. }) | Node::Rule(RuleNode { height, .. }) => {
                self.initialize_current_page(node, height, contributions, eqtb, logger);
            }
            Node::Ins(ref mut ins_node) => {
                self.append_insertion_to_current_page(ins_node, scanner, eqtb, logger);
                self.page.push(node);
            }
            Node::Whatsit(_) | Node::Mark(_) => {
                self.page.push(node);
            }
            Node::Glue(_) | Node::Kern(_) | Node::Penalty(_) => {}
            _ => panic!("page"),
        }
    }

    /// Checks if a break is possible at the current node.
    /// Returns Some(penalty) if possible, else None.
    /// See 1000.
    fn check_if_break_is_possible(
        &mut self,
        node: &Node,
        contributions: &Vec<Node>,
    ) -> Option<i32> {
        match node {
            Node::List(_) | Node::Rule(_) | Node::Whatsit(_) | Node::Mark(_) | Node::Ins(_) => {}
            Node::Glue(_) => {
                if let Some(prev_node) = self.page.last() {
                    if prev_node.precedes_vertical_break() {
                        return Some(0);
                    }
                }
            }
            Node::Kern(_) => {
                if let Some(Node::Glue(_)) = contributions.last() {
                    return Some(0);
                }
            }
            Node::Penalty(penalty_node) => {
                return Some(penalty_node.penalty);
            }
            _ => panic!("page"),
        }
        None
    }

    /// See 1000.
    fn update_measurements_and_insertions(
        &mut self,
        node: &mut Node,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        match node {
            &mut Node::List(ListNode { height, depth, .. })
            | &mut Node::Rule(RuleNode { height, depth, .. }) => {
                eqtb.page_dims.height += eqtb.page_dims.depth + height;
                eqtb.page_dims.depth = depth;
            }
            Node::Whatsit(_) => {}
            Node::Glue(glue_node) => {
                PageBuilder::update_current_page_measurements_glue(
                    glue_node, scanner, eqtb, logger,
                );
            }
            Node::Kern(kern_node) => {
                PageBuilder::update_current_page_measurements_kern(kern_node, eqtb);
            }
            Node::Penalty(_) => {}
            Node::Mark(_) => {}
            Node::Ins(ins_node) => {
                self.append_insertion_to_current_page(ins_node, scanner, eqtb, logger);
            }
            _ => panic!("page"),
        }
    }

    /// See 1001.
    fn initialize_current_page(
        &mut self,
        node: Node,
        height: Dimension,
        contributions: &mut Vec<Node>,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        if eqtb.page_contents == PageContents::Empty {
            self.freeze_page_specs(eqtb, logger);
        }
        eqtb.page_contents = PageContents::BoxThere;
        contributions.push(node);

        let top_skip = eqtb.skips.get(SkipVariable::TopSkip);
        let d = if top_skip.width > height {
            top_skip.width - height
        } else {
            0
        };
        let glue_node = GlueNode::new_param_with_width(SkipVariable::TopSkip, d, eqtb);
        contributions.push(Node::Glue(glue_node));
    }

    /// See 1003.
    fn make_sure_page_max_depth_not_exceeded(&self, eqtb: &mut Eqtb) {
        if eqtb.page_dims.depth > self.max_depth {
            eqtb.page_dims.height += eqtb.page_dims.depth - self.max_depth;
            eqtb.page_dims.depth = self.max_depth;
        }
    }

    /// See 1004.
    fn update_current_page_measurements_glue(
        glue_node: &mut GlueNode,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        eqtb.page_dims
            .stretch
            .add_dimen(glue_node.glue_spec.stretch);

        if glue_node.glue_spec.shrink.is_infinite() {
            logger.print_err("Infinite glue shrinkage found on current page");
            let help = &[
                "The page about to be output contains some infinitely",
                "shrinkable glue, e.g., `\\vss' or `\\vskip 0pt minus 1fil'.",
                "Such glue doesn't belong there; but you can safely proceed,",
                "since the offensive shrinkability has been made finite.",
            ];
            logger.error(help, scanner, eqtb);

            glue_node.glue_spec = glue_node.glue_spec.finite_shrink_copy();
        }

        eqtb.page_dims.shrink += glue_node.glue_spec.shrink.value;
        eqtb.page_dims.height += eqtb.page_dims.depth + glue_node.glue_spec.width;
        eqtb.page_dims.depth = 0;
    }

    /// See 1004.
    fn update_current_page_measurements_kern(kern_node: &KernNode, eqtb: &mut Eqtb) {
        eqtb.page_dims.height += eqtb.page_dims.depth + kern_node.width;
        eqtb.page_dims.depth = 0;
    }

    /// Returns true if an output routine should start, else false.
    /// See 1005.
    fn update_break_points(&mut self, penalty: i32, eqtb: &Eqtb, logger: &mut Logger) -> bool {
        if penalty < INF_PENALTY {
            let badness = PageBuilder::compute_badness_of_current_page(eqtb);
            let mut cost = if badness < AWFUL_BAD {
                if penalty <= EJECT_PENALTY {
                    penalty
                } else if badness < INF_BAD {
                    badness + penalty + eqtb.insert_penalties
                } else {
                    DEPLORABLE
                }
            } else {
                badness
            };
            if eqtb.insert_penalties >= 10_000 {
                cost = AWFUL_BAD;
            }
            if cfg!(feature = "stats") {
                if eqtb.integer(IntegerVariable::TracingPages) > 0 {
                    self.display_page_break_cost(penalty, badness, cost, eqtb, logger);
                }
            }
            if cost <= self.least_cost {
                self.best_break = self.page.len();
                self.best_size = eqtb.page_dims.page_goal;
                self.least_cost = cost;
                for page_ins in &mut self.page_ins_list.values_mut() {
                    page_ins.best_ins_count = page_ins.ins_count;
                }
            }
            if cost == AWFUL_BAD || penalty <= EJECT_PENALTY {
                return true;
            }
        }
        false
    }

    /// See 1006.
    fn display_page_break_cost(&self, pi: i32, b: i32, c: i32, eqtb: &Eqtb, logger: &mut Logger) {
        logger.begin_diagnostic(eqtb.tracing_online());
        logger.print_nl_str("%");
        logger.print_str(" t=");
        eqtb.page_dims.print_totals(logger);
        logger.print_str(" g=");
        logger.print_scaled(eqtb.page_dims.page_goal);
        logger.print_str(" b=");
        if b == AWFUL_BAD {
            logger.print_char(b'*');
        } else {
            logger.print_int(b);
        }
        logger.print_str(" p=");
        logger.print_int(pi);
        logger.print_str(" c=");
        if c == AWFUL_BAD {
            logger.print_char(b'*');
        } else {
            logger.print_int(c);
        }
        if c <= self.least_cost {
            logger.print_char(b'#');
        }
        logger.end_diagnostic(false);
    }

    /// See 1007.
    fn compute_badness_of_current_page(eqtb: &Eqtb) -> i32 {
        if eqtb.page_dims.height < eqtb.page_dims.page_goal {
            let stretch = eqtb.page_dims.stretch.evaluate();
            if stretch.order == DimensionOrder::Normal {
                calculate_badness(
                    eqtb.page_dims.page_goal - eqtb.page_dims.height,
                    stretch.value,
                )
            } else {
                0
            }
        } else if eqtb.page_dims.height - eqtb.page_dims.page_goal > eqtb.page_dims.shrink {
            AWFUL_BAD
        } else {
            calculate_badness(
                eqtb.page_dims.height - eqtb.page_dims.page_goal,
                eqtb.page_dims.shrink,
            )
        }
    }

    /// See 1008.
    fn append_insertion_to_current_page(
        &mut self,
        ins_node: &mut InsNode,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        if eqtb.page_contents == PageContents::Empty {
            self.freeze_page_specs(eqtb, logger);
            eqtb.page_contents = PageContents::InsertsOnly;
        }
        let n = ins_node.number;
        let page_ins = if let Some(page_ins) = self.page_ins_list.get_mut(&n) {
            page_ins
        } else {
            self.create_page_insertion_node(n, scanner, eqtb, logger);
            let Some(page_ins) = self.page_ins_list.get_mut(&n) else {
                panic!("We just added it");
            };
            page_ins
        };

        if let InsType::SplitUp { .. } = page_ins.typ {
            eqtb.insert_penalties += ins_node.float_cost;
        } else {
            page_ins.ins_count += 1;
            let delta = eqtb.page_dims.page_goal - eqtb.page_dims.height - eqtb.page_dims.depth
                + eqtb.page_dims.shrink;
            let h = if eqtb.integer(IntegerVariable::Count(n)) == 1000 {
                ins_node.size
            } else {
                ins_node.size / 1000 * eqtb.integer(IntegerVariable::Count(n))
            };
            if (h <= 0 || h <= delta)
                && ins_node.size + page_ins.height <= eqtb.dimen(DimensionVariable::Dimen(n))
            {
                eqtb.page_dims.page_goal -= h;
                page_ins.height += ins_node.size;
            } else {
                self.find_best_way_to_split_insertion(ins_node, n, scanner, eqtb, logger);
            }
        }
    }

    /// See 1009.
    fn create_page_insertion_node(
        &mut self,
        n: RegisterIndex,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        let mut page_ins = PageIns {
            typ: InsType::Inserting,
            height: 0,
            ins_count: 0,
            best_ins_count: 0,
        };
        Self::ensure_vbox(n, scanner, eqtb, logger);
        let boks = eqtb.boks(n);
        match boks {
            None => {
                page_ins.height = 0;
            }
            Some(list_node) => {
                page_ins.height = list_node.height + list_node.depth;
            }
        }
        let ins_skip = eqtb.skips.get(SkipVariable::Skip(n));
        let h = if eqtb.integer(IntegerVariable::Count(n)) == 1000 {
            page_ins.height
        } else {
            page_ins.height / 1000 * eqtb.integer(IntegerVariable::Count(n))
        };
        self.page_ins_list.insert(n, page_ins);

        eqtb.page_dims.page_goal -= h + ins_skip.width;
        eqtb.page_dims.stretch.add_dimen(ins_skip.stretch);
        eqtb.page_dims.shrink += ins_skip.shrink.value;
        if ins_skip.shrink.is_infinite() {
            logger.print_err("Infinite glue shrinkage inserted from ");
            logger.print_esc_str(b"skip");
            logger.print_int(n as i32);
            let help = &[
                "The correction glue for page breaking with insertions",
                "must have finite shrinkability. But you may proceed,",
                "since the offensive shrinkability has been made finite.",
            ];
            logger.error(help, scanner, eqtb);
        }
    }

    /// See 1010.
    fn find_best_way_to_split_insertion(
        &mut self,
        ins_node: &mut InsNode,
        n: RegisterIndex,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        let Some(page_ins) = self.page_ins_list.get_mut(&n) else {
            panic!("We ensured it's there.");
        };
        let mut w;
        let count = eqtb.integer(IntegerVariable::Count(n));
        if count <= 0 {
            w = MAX_DIMEN;
        } else {
            w = eqtb.page_dims.page_goal - eqtb.page_dims.height - eqtb.page_dims.depth;
            if count != 1000 {
                w = w / count * 1000;
            }
        }
        let dimen = eqtb.dimen(DimensionVariable::Dimen(n));
        if w > dimen - page_ins.height {
            w = dimen - page_ins.height;
        }
        let (break_point, mut best_height_plus_depth) = vert_break(
            &mut ins_node.insertion,
            w,
            ins_node.split_max_depth,
            scanner,
            eqtb,
            logger,
        );

        page_ins.height += best_height_plus_depth;
        if cfg!(feature = "stats") {
            if eqtb.integer(IntegerVariable::TracingPages) > 0 {
                PageBuilder::display_insertion_split_cost(
                    ins_node,
                    break_point,
                    n,
                    w,
                    best_height_plus_depth,
                    eqtb,
                    logger,
                );
            }
        }
        if eqtb.integer(IntegerVariable::Count(n)) != 1000 {
            best_height_plus_depth =
                best_height_plus_depth / 1000 * eqtb.integer(IntegerVariable::Count(n));
        }
        eqtb.page_dims.page_goal -= best_height_plus_depth;
        page_ins.typ = InsType::SplitUp {
            inner_break_pos: break_point,
            broken_count: page_ins.ins_count,
        };
        if break_point == ins_node.insertion.len() {
            eqtb.insert_penalties += EJECT_PENALTY;
        } else if let Node::Penalty(penalty_node) = &ins_node.insertion[break_point] {
            eqtb.insert_penalties += penalty_node.penalty;
        }
    }

    /// See 1011.
    fn display_insertion_split_cost(
        ins_node: &InsNode,
        break_point: usize,
        n: RegisterIndex,
        w: Scaled,
        best_height_plus_depth: Dimension,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        logger.begin_diagnostic(eqtb.tracing_online());
        logger.print_nl_str("% split");
        logger.print_int(n as i32);
        logger.print_str(" to ");
        logger.print_scaled(w);
        logger.print_char(b',');
        logger.print_scaled(best_height_plus_depth);
        logger.print_str(" p=");
        if break_point == ins_node.insertion.len() {
            logger.print_int(EJECT_PENALTY);
        } else if let Node::Penalty(penalty_node) = &ins_node.insertion[break_point] {
            logger.print_int(penalty_node.penalty);
        } else {
            logger.print_char(b'0');
        }
        logger.end_diagnostic(false);
    }

    /// See 1012.
    fn fire_up(
        &mut self,
        contributions: &mut Vec<Node>,
        output: &mut Output,
        nest: &mut SemanticState,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        if let Some(rc_token_list) = &eqtb.marks.bot {
            eqtb.marks.top = Some(rc_token_list.clone());
            eqtb.marks.first = None;
        }

        self.put_optimal_currentpage_into_box(contributions, scanner, eqtb, logger);

        if eqtb.marks.first.is_none() {
            if let Some(rc_token_list) = &eqtb.marks.top {
                eqtb.marks.first = Some(rc_token_list.clone());
            }
        }
        if let Some(output_routine) = eqtb.token_lists.get(TokenListVariable::OutputRoutine) {
            if eqtb.dead_cycles >= eqtb.integer(IntegerVariable::MaxDeadCycles) {
                PageBuilder::explain_that_too_many_dead_cycles_have_occurred_in_a_row(
                    scanner, eqtb, logger,
                );
            } else {
                PageBuilder::fire_up_users_output_routine(
                    output_routine.clone(),
                    nest,
                    scanner,
                    eqtb,
                    logger,
                );
                return;
            }
        }

        self.perform_default_output_routine(contributions, output, scanner, eqtb, logger);
    }

    /// See 1013.
    fn set_value_of_output_penalty(
        &mut self,
        node: &mut Node,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        // Deal with the case where the break is at the end of the current page.
        let break_node = if self.best_break == self.page.len() {
            node
        } else {
            &mut self.page[self.best_break]
        };

        let output_penalty = if let Node::Penalty(penalty_node) = break_node {
            std::mem::replace(&mut penalty_node.penalty, INF_PENALTY)
        } else {
            INF_PENALTY
        };
        eqtb.int_define(IntegerVariable::OutputPenalty, output_penalty, true, logger);
    }

    /// See 1014. and 1017.
    fn put_optimal_currentpage_into_box(
        &mut self,
        contributions: &mut Vec<Node>,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        PageBuilder::ensure_that_box255_is_empty(scanner, eqtb, logger);

        // \insertpenalties holds the number of used insertions while output is active.
        eqtb.insert_penalties = 0;

        let save_split_top_skip = eqtb.skips.get(SkipVariable::SplitTopSkip).clone();
        if eqtb.integer(IntegerVariable::HoldingInserts) <= 0 {
            self.prepare_all_boxes_involved_in_insertions_to_act_as_queues(scanner, eqtb, logger);
        }
        let mut hold_list = Vec::new();

        let mut page = std::mem::take(&mut self.page);
        let remainder = page.split_off(self.best_break);

        let mut new_page = Vec::new();
        for node in page {
            if let Node::Ins(ins_node) = node {
                if eqtb.integer(IntegerVariable::HoldingInserts) <= 0 {
                    self.either_insert_material_or_hold_for_next_page(
                        ins_node,
                        &mut hold_list,
                        eqtb,
                        logger,
                    );
                } else {
                    new_page.push(Node::Ins(ins_node));
                }
                continue;
            }
            if let Node::Mark(mark_node) = &node {
                PageBuilder::update_values_of_first_mark_and_bot_mark(mark_node, eqtb);
            }
            new_page.push(node);
        }

        eqtb.skips
            .set(SkipVariable::SplitTopSkip, save_split_top_skip);

        PageBuilder::move_remainder_back_to_contributions(remainder, contributions);

        self.break_current_page_at_node(new_page, hold_list, eqtb, logger);
        self.page_ins_list.clear();
    }

    /// See 1015.
    fn ensure_that_box255_is_empty(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) {
        if let Some(list_node) = eqtb.boxes.set(BoxVariable(255), None) {
            logger.print_err("");
            logger.print_esc_str(b"box");
            logger.print_str("255 is not void");
            let help = &[
                "You shouldn't use \\box255 except in \\output routines.",
                "Proceed, and I'll discard its present contents.",
            ];
            PageBuilder::box_error(list_node, help, scanner, eqtb, logger);
        }
    }

    /// See 1016.
    fn update_values_of_first_mark_and_bot_mark(mark_node: &MarkNode, eqtb: &mut Eqtb) {
        if eqtb.marks.first.is_none() {
            eqtb.marks.first = Some(mark_node.mark.clone());
        }
        eqtb.marks.bot = Some(mark_node.mark.clone());
    }

    /// Move everything after the page break to the front of the contribution list.
    /// See 1017.
    fn move_remainder_back_to_contributions(
        mut remainder: Vec<Node>,
        contributions: &mut Vec<Node>,
    ) {
        if !remainder.is_empty() {
            remainder.reverse();
            contributions.append(&mut remainder);
        }
    }

    /// See 1017.
    fn break_current_page_at_node(
        &mut self,
        new_page: Vec<Node>,
        hold_list: Vec<Node>,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        // Disable errors in vpackage by temporarily changing vbadness and vfuzz.
        let save_vbadness = eqtb.integer(IntegerVariable::Vbadness);
        eqtb.integers.set(IntegerVariable::Vbadness, INF_BAD);
        let save_vfuzz = eqtb.dimen(DimensionVariable::Vfuzz);
        eqtb.dimensions.set(DimensionVariable::Vfuzz, MAX_DIMEN);

        // Package page.
        let new_box_value = vpackage(
            new_page,
            TargetSpec::Exactly(self.best_size),
            self.max_depth,
            eqtb,
            logger,
        );

        eqtb.boxes.set(BoxVariable(255), Some(new_box_value));

        // Restore vbadness and vfuzz.
        eqtb.integers.set(IntegerVariable::Vbadness, save_vbadness);
        eqtb.dimensions.set(DimensionVariable::Vfuzz, save_vfuzz);

        self.start_new_current_page(eqtb);
        // Move the insertion that did not make it to the page the front of the next one.
        if !hold_list.is_empty() {
            self.page = hold_list;
        }
    }

    /// See 1018.
    fn prepare_all_boxes_involved_in_insertions_to_act_as_queues(
        &mut self,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        for (&n, page_ins) in &mut self.page_ins_list {
            if page_ins.best_ins_count > 0 {
                // We will use ins_count to count the number of accepted insertions.
                page_ins.ins_count = 0;

                PageBuilder::ensure_vbox(n, scanner, eqtb, logger);
                if eqtb.boks(n).is_none() {
                    eqtb.boxes
                        .set(BoxVariable(n), Some(ListNode::new_empty_vbox()));
                }
            }
        }
    }

    /// See 1020.
    fn either_insert_material_or_hold_for_next_page(
        &mut self,
        ins_node: InsNode,
        hold_list: &mut Vec<Node>,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        let Some(page_ins) = self.page_ins_list.get_mut(&ins_node.number) else {
            panic!("We ensured it's there");
        };
        page_ins.ins_count += 1;

        let mut boks = eqtb.boxes.set(BoxVariable(ins_node.number), None).unwrap();
        // We already encountered the last InsNode that still makes it onto the page.
        if page_ins.best_ins_count == 0 {
            hold_list.push(Node::Ins(ins_node));
            eqtb.insert_penalties += 1;
        } else {
            // This is the last InsNode that still makes it onto the page.
            if page_ins.best_ins_count == page_ins.ins_count {
                self.wrap_up_box(ins_node, boks, hold_list, eqtb, logger);
            } else {
                let mut insertion = ins_node.insertion;
                let list = match &mut boks.list {
                    HlistOrVlist::Vlist(list) => list,
                    HlistOrVlist::Hlist(_) => panic!("It should always be a vlist"),
                };

                list.append(&mut insertion);
                eqtb.boxes.set(BoxVariable(ins_node.number), Some(boks));
            }
        }
    }

    /// See 1021.
    fn wrap_up_box(
        &mut self,
        mut ins_node: InsNode,
        boks: ListNode,
        hold_list: &mut Vec<Node>,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        let n = ins_node.number;
        let Some(page_ins) = self.page_ins_list.get_mut(&n) else {
            panic!("We ensured it's there");
        };
        let HlistOrVlist::Vlist(mut vlist) = boks.list else {
            panic!("It should always be a vlist");
        };
        if let InsType::SplitUp {
            broken_count,
            inner_break_pos,
        } = page_ins.typ
        {
            if broken_count == page_ins.ins_count && inner_break_pos != ins_node.insertion.len() {
                let mut insertion = std::mem::take(&mut ins_node.insertion);
                let second_part = insertion.split_off(inner_break_pos);
                let mut first_part = insertion;

                // Attach first part
                vlist.append(&mut first_part);

                // Prune the remainder.
                eqtb.skips
                    .set(SkipVariable::SplitTopSkip, ins_node.split_top_skip.clone());
                let pruned_remainder = prune_page_top(second_part, eqtb);

                ins_node.insertion = pruned_remainder;

                // If the remainder is non-empty, we keep it around.
                if !ins_node.insertion.is_empty() {
                    let measurement = measure_vlist(&ins_node.insertion);
                    ins_node.size = measurement.height + measurement.depth;

                    hold_list.push(Node::Ins(ins_node));
                    eqtb.insert_penalties += 1;
                }
            } else {
                vlist.append(&mut ins_node.insertion);
            }
        } else {
            vlist.append(&mut ins_node.insertion);
        }

        page_ins.best_ins_count = 0;

        let vbox = vpack(vlist, TargetSpec::Natural, eqtb, logger);
        eqtb.boxes.set(BoxVariable(n), Some(vbox));
    }

    /// See 1023.
    fn perform_default_output_routine(
        &mut self,
        contributions: &mut Vec<Node>,
        output: &mut Output,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        // Move the insertions back from the page to the contributions list.
        if !self.page.is_empty() {
            let mut page = std::mem::take(&mut self.page);
            page.reverse();
            contributions.append(&mut page);
        }
        let Some(list_node) = eqtb.boxes.set(BoxVariable(255), None) else {
            panic!("Should not happen");
        };
        output.ship_out(list_node, scanner, eqtb, logger)
    }

    /// See 1024.
    fn explain_that_too_many_dead_cycles_have_occurred_in_a_row(
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        logger.print_err("Output loop---");
        logger.print_int(eqtb.dead_cycles);
        logger.print_str(" consecutive dead cycles");
        let help = &[
            "I've concluded that your \\output is awry; it never does a",
            "\\shipout, so I'm shipping \\box255 out myself. Next time",
            "increase \\maxdeadcycles if you want me to be more patient!",
        ];
        logger.error(help, scanner, eqtb);
    }

    /// See 1025.
    fn fire_up_users_output_routine(
        output_routine: RcTokenList,
        nest: &mut SemanticState,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        eqtb.output_active = true;
        eqtb.dead_cycles += 1;
        nest.push_nest(
            RichMode::Vertical(VerticalMode::new_internal()),
            &scanner.input_stack,
            eqtb,
            logger,
        );
        *nest.mode_line_mut() = -(eqtb.line_number() as i32);
        scanner.input_stack.begin_token_list(
            output_routine,
            TokenSourceType::OutputText,
            eqtb,
            logger,
        );
        eqtb.new_save_level(GroupType::Output, &scanner.input_stack, logger);
        normal_paragraph(eqtb, logger);
        scanner.scan_left_brace(eqtb, logger);
    }

    /// See 1026.
    pub fn resume_page_builder_after_output_routine_had_come_to_end(
        &mut self,
        hyphenator: &mut Hyphenator,
        output: &mut Output,
        nest: &mut SemanticState,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        if !scanner.input_stack.is_output_text_finished() {
            PageBuilder::recover_from_unbalanced_output_routine(scanner, eqtb, logger);
        }
        scanner.end_token_list(eqtb, logger);

        end_graf(hyphenator, nest, scanner, eqtb, logger);
        eqtb.unsave(scanner, logger);
        let old_level = nest.pop_nest(eqtb);
        let RichMode::Vertical(vmode) = old_level.mode else {
            panic!("The mode must have been vertical");
        };
        eqtb.output_active = false;
        eqtb.insert_penalties = 0;
        PageBuilder::ensure_box255_is_empty(scanner, eqtb, logger);

        let mut contributions = get_contributions_stack(nest);
        let mut node_list = vmode.list;
        // We push the current list before remaining contributions.
        if !node_list.is_empty() {
            node_list.reverse();
            contributions.append(&mut node_list);
        }
        // We push held-over insertions before these and empty the page.
        if !self.page.is_empty() {
            let mut insertions = std::mem::take(&mut self.page);
            insertions.reverse();
            contributions.append(&mut insertions);
        }
        move_contributions_back(contributions, nest);

        self.build_page(output, nest, scanner, eqtb, logger);
    }

    /// If we have an unbalanced output routine, we are in trouble. This routine tries to salvage
    /// the situation nonetheless.
    /// See 1027.
    fn recover_from_unbalanced_output_routine(
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        logger.print_err("Unbalanced output routine");
        let help = &[
            "Your sneaky output routine has problematic {'s and/or }'s.",
            "I can't handle that very well; good luck.",
        ];
        logger.error(help, scanner, eqtb);

        scanner.input_stack.deplete_current_token_list();
    }

    /// See 1028.
    fn ensure_box255_is_empty(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) {
        if let Some(list_node) = eqtb.boxes.set(BoxVariable(255), None) {
            logger.print_err("Output routine didn't use all of ");
            logger.print_esc_str(b"box");
            logger.print_int(255);
            let help = &[
                "Your \\output commands should empty \\box255,",
                "e.g., by saying `\\shipout\\box255'.",
                "Proceed; I'll discard its present contents.",
            ];
            PageBuilder::box_error(list_node, help, scanner, eqtb, logger);
        }
    }
}

/// Note: reverses the order of Nodes.
fn get_contributions_stack(nest: &mut SemanticState) -> Vec<Node> {
    let mut contributions = std::mem::take(nest.base_list_mut());
    contributions.reverse();
    contributions
}

fn move_contributions_back(mut contributions: Vec<Node>, nest: &mut SemanticState) {
    contributions.reverse();
    *nest.base_list_mut() = contributions;
}

#[derive(Debug)]
pub struct Marks {
    pub top: Option<RcTokenList>,
    pub first: Option<RcTokenList>,
    pub bot: Option<RcTokenList>,
    pub split_first: Option<RcTokenList>,
    pub split_bot: Option<RcTokenList>,
}

impl Marks {
    pub fn get(&self, mark_command: MarkCommand) -> &Option<RcTokenList> {
        match mark_command {
            MarkCommand::Top => &self.top,
            MarkCommand::First => &self.first,
            MarkCommand::Bot => &self.bot,
            MarkCommand::SplitFirst => &self.split_first,
            MarkCommand::SplitBot => &self.split_bot,
        }
    }
}
