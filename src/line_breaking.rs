use crate::dimension::{Dimension, MAX_DIMEN};
use crate::eqtb::Eqtb;
use crate::eqtb::{DimensionVariable, IntegerVariable, ParShapeVariable, SkipVariable, NULL_FONT};
use crate::horizontal_mode::LanguageData;
use crate::hyphenation::Hyphenator;
use crate::input::Scanner;
use crate::logger::Logger;
use crate::nodes::{
    short_display, CharNode, DimensionOrder, DiscNode, GlueNode, GlueSign, GlueSpec, GlueType,
    HigherOrderDimension, HlistOrVlist, KernNode, KernSubtype, LigatureNode, ListNode, MathNode,
    MathNodeKind, Node, PenaltyNode, RuleNode,
};
use crate::packaging::{hpack, split_adjust_material_off, GlueCollector, TargetSpec};
use crate::print::Printer;
use crate::scaled::{calculate_badness, Scaled, INF_BAD};
use crate::semantic_nest::{RichMode, SemanticState};

use std::ops::{Add, AddAssign, Neg, Sub, SubAssign};

pub const INF_PENALTY: i32 = INF_BAD;
pub const EJECT_PENALTY: i32 = -INF_PENALTY;

/// See 817.
const TIGHT_FIT: usize = 3;
const DECENT_FIT: usize = 2;
const LOOSE_FIT: usize = 1;
const VERY_LOOSE_FIT: usize = 0;

/// See 833.
pub const AWFUL_BAD: i32 = 0o7777777777;

/// If `give_pre_display_size` is true, returns the `pre_display_size`, else MAX_DIMEN.
/// See 815.
pub fn line_break(
    mut hlist: Vec<Node>,
    lang_data: &LanguageData,
    begin_line: i32,
    final_widow_penalty: i32,
    give_pre_display_size: bool,
    hyphenator: &mut Hyphenator,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Dimension {
    logger.pack_begin_line = begin_line;
    ensure_finite_shrinkage(&mut hlist, scanner, eqtb, logger);

    let mut line_breaker = LineBreaker::new();
    line_breaker.get_ready_to_start_line_breaking(lang_data, &mut hlist, hyphenator, eqtb);

    let RichMode::Vertical(vmode) = nest.mode() else {
        panic!("We expected to be in vertical mode here");
    };

    let (best_bet, hlist) = line_breaker.find_optimal_break_points(
        hlist,
        hyphenator,
        vmode.prev_graf,
        scanner,
        eqtb,
        logger,
    );
    let pre_display_size = line_breaker.post_line_break(
        hlist,
        best_bet,
        final_widow_penalty,
        give_pre_display_size,
        nest,
        eqtb,
        logger,
    );
    line_breaker.clean_up_memory_by_removing_break_nodes();

    logger.pack_begin_line = 0;
    pre_display_size
}

/// Ensure that all glue used in the line has finite shrinkage.
/// See 825., 826., 868. and 872.
fn ensure_finite_shrinkage(
    hlist: &mut Vec<Node>,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let mut found_shrink_error = false;

    // Check the left and right skips for infinite shrinkage.
    if eqtb.left_skip().shrink.is_infinite() {
        found_shrink_error = true;
        eqtb.skips.set(
            SkipVariable::LeftSkip,
            eqtb.left_skip().finite_shrink_copy(),
        );
    }
    if eqtb.right_skip().shrink.is_infinite() {
        found_shrink_error = true;
        eqtb.skips.set(
            SkipVariable::RightSkip,
            eqtb.right_skip().finite_shrink_copy(),
        );
    }

    // Check the hlist glue nodes for infinite shrinkage.
    for node in hlist {
        if let Node::Glue(glue_node) = node {
            if glue_node.glue_spec.shrink.is_infinite() {
                found_shrink_error = true;
                glue_node.glue_spec = glue_node.glue_spec.finite_shrink_copy();
            }
        }
    }

    if found_shrink_error {
        if cfg!(feature = "stats") {
            if eqtb.tracing_paragraphs() > 0 {
                logger.end_diagnostic(true);
            }
        }
        logger.print_err("Infinite glue shrinkage found in a paragraph");
        let help = &[
            "The paragraph just ended includes some glue that has",
            "infinite shrinkability, e.g., `\\hskip 0pt minus 1fil'.",
            "Such glue doesn't belong there---it allows a paragraph",
            "of any length to fit on one line. But it's safe to proceed,",
            "since the offensive shrinkability has been made finite.",
        ];
        logger.error(help, scanner, eqtb);
        if cfg!(feature = "stats") {
            if eqtb.tracing_paragraphs() > 0 {
                logger.begin_diagnostic(eqtb.tracing_online());
            }
        }
    }
}

struct LineBreaker {
    cur_pos: usize,

    /// See 847.
    easy_line: usize,
    last_special_line: usize,
    first_width: Dimension,
    first_indent: Dimension,
    second_width: Dimension,
    second_indent: Dimension,

    /// See 828.
    second_pass: bool,
    final_pass: bool,
    threshold: i32,
    /// See 833.
    minimum_demerits: i32,
    minimal_demerits: [i32; 4],
    best_place: [Option<usize>; 4],
    best_pl_line: [usize; 4],

    actives: Vec<ActiveNode>,
    // We keep this around to avoid reallocation.
    new_actives: Vec<ActiveNode>,
    passive: Vec<PassiveNode>,
    /// See 823.
    break_dimens: HorizontalDimensions,
    /// The dimensions of an empty line.
    background: HorizontalDimensions,
    /// Stores the change in line width when the moving through the hlist.
    delta: HorizontalDimensions,

    /// Indicates the index of the first unprinted node.
    unprinted_node: usize,
}

/// See 819.
#[derive(Debug, Clone)]
struct ActiveNode {
    fitness: u8,
    break_node: Option<usize>,
    line_number: usize,
    total_demerits: i32,
    typ: BreakType,
    dimens: HorizontalDimensions,
}

/// See 821.
#[derive(Debug)]
struct PassiveNode {
    break_pos: usize,
    prev_break: Option<usize>,
    serial: usize,
}

impl LineBreaker {
    fn new() -> Self {
        Self {
            cur_pos: 0,
            easy_line: 0,
            last_special_line: 0,
            first_width: 0,
            first_indent: 0,
            second_width: 0,
            second_indent: 0,
            second_pass: false,
            final_pass: false,
            threshold: 0,
            minimum_demerits: 0,
            minimal_demerits: [0; 4],
            best_place: [None; 4],
            best_pl_line: [0; 4],
            actives: Vec::new(),
            new_actives: Vec::new(),
            passive: Vec::new(),
            break_dimens: HorizontalDimensions::new(),
            background: HorizontalDimensions::new(),
            delta: HorizontalDimensions::new(),
            unprinted_node: 0,
        }
    }

    /// See 816. and 834.
    fn get_ready_to_start_line_breaking(
        &mut self,
        lang_data: &LanguageData,
        hlist: &mut Vec<Node>,
        hyphenator: &mut Hyphenator,
        eqtb: &Eqtb,
    ) {
        // Potentially remove a trailing glue nodekip is never a breakpoint.
        if let Some(Node::Glue(_)) = hlist.last() {
            hlist.pop();
        }

        // Append an infinite penalty followed by a ParFillSkip such that the second is never a
        // breakpoint.
        let inf_penalty = Node::Penalty(PenaltyNode {
            penalty: INF_PENALTY,
        });
        let par_fill_skip = Node::Glue(GlueNode::new_param(SkipVariable::ParFillSkip, eqtb));
        hlist.extend([inf_penalty, par_fill_skip]);

        hyphenator.init_cur_lang = lang_data.language;
        hyphenator.init_l_hyf = lang_data.left_hyphen_min;
        hyphenator.init_r_hyf = lang_data.right_hyphen_min;
        self.background = self.get_size_of_empty_line(eqtb);
        self.minimum_demerits = AWFUL_BAD;
        self.minimal_demerits = [AWFUL_BAD; 4];
        self.determine_line_classes(eqtb);
    }

    /// See 827.
    fn get_size_of_empty_line(&mut self, eqtb: &Eqtb) -> HorizontalDimensions {
        let left_skip = eqtb.left_skip();
        let right_skip = eqtb.right_skip();

        let mut background = HorizontalDimensions::new();
        background.width = left_skip.width + right_skip.width;
        background.stretch = GlueCollector::new();
        background.stretch.add_dimen(left_skip.stretch);
        background.stretch.add_dimen(right_skip.stretch);
        background.shrink = left_skip.shrink.value + right_skip.shrink.value;
        background
    }

    /// See 829., 831., 832., 835., 851., and 854.
    fn try_break(
        &mut self,
        hlist: &Vec<Node>,
        mut pi: i32,
        break_type: BreakType,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        // From 831.
        if pi.abs() >= INF_PENALTY {
            if pi > 0 {
                return;
            } else {
                pi = EJECT_PENALTY;
            }
        }

        let mut no_break_yet = true;
        let mut old_l = 0;
        let mut l;
        let mut line_width = 0;
        let mut actives = std::mem::take(&mut self.actives);
        let mut actives_iter = actives.drain(..).peekable();
        while let Some(mut active) = actives_iter.next() {
            let is_last = actives_iter.peek().is_none();

            // From 832.
            active.dimens += self.delta;

            // From 835.
            l = active.line_number;
            if l > old_l {
                if self.minimum_demerits < AWFUL_BAD && old_l != self.easy_line {
                    self.create_new_active_nodes(
                        hlist,
                        &mut no_break_yet,
                        break_type,
                        eqtb,
                        logger,
                    );
                }
                line_width = self.compute_new_line_width(l, &mut old_l, eqtb);
            }

            // From 851.
            let shortfall = line_width - active.dimens.width;
            let (b, fit_class) = if shortfall > 0 {
                Self::compute_badness_and_fit_class_for_stretching(&active, shortfall)
            } else {
                Self::compute_badness_and_fit_class_for_shrinking(&active, shortfall)
            };

            // Note: b > INF_BAD only if the line cannot be shrunk to the required size.
            if b > INF_BAD {
                // From 854.
                if self.is_desperate(is_last) {
                    self.record_artificial_break(hlist, &active, fit_class, l, b, pi, eqtb, logger);
                }
            } else if pi == EJECT_PENALTY {
                // From 854.
                if self.is_desperate(is_last) {
                    self.record_artificial_break(hlist, &active, fit_class, l, b, pi, eqtb, logger);
                } else if b <= self.threshold {
                    self.record_feasible_break(
                        hlist, &active, break_type, fit_class, l, b, pi, eqtb, logger,
                    );
                }
                // We always break here, so we don't want to keep any old active nodes.
            } else {
                if b <= self.threshold {
                    self.record_feasible_break(
                        hlist, &active, break_type, fit_class, l, b, pi, eqtb, logger,
                    );
                }
                // We want to keep this active alive
                self.new_actives.push(active);
            }
        }
        if self.minimum_demerits < AWFUL_BAD {
            self.create_new_active_nodes(hlist, &mut no_break_yet, break_type, eqtb, logger);
        }

        self.delta = HorizontalDimensions::new();
        drop(actives_iter);
        self.actives = actives;
        std::mem::swap(&mut self.new_actives, &mut self.actives);
    }

    fn is_desperate(&self, is_last: bool) -> bool {
        // If we really need at least one active node to remain.
        self.final_pass
            && self.minimum_demerits == AWFUL_BAD
            && self.new_actives.is_empty()
            && is_last
    }

    /// 836.
    fn create_new_active_nodes(
        &mut self,
        hlist: &Vec<Node>,
        no_break_yet: &mut bool,
        break_type: BreakType,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        if *no_break_yet {
            self.break_dimens = self.compute_value_of_break_dimens(hlist, break_type);
            *no_break_yet = false;
        }

        let adj_demerits = eqtb.integer(IntegerVariable::AdjDemerits);
        let demerits_bound = if adj_demerits.abs() >= AWFUL_BAD - self.minimum_demerits {
            AWFUL_BAD - 1
        } else {
            self.minimum_demerits + adj_demerits.abs()
        };

        for fit_class in VERY_LOOSE_FIT..=TIGHT_FIT {
            if self.minimal_demerits[fit_class] <= demerits_bound {
                let active = self.insert_new_active_node(fit_class, break_type, eqtb, logger);
                self.new_actives.push(active);
            }
            self.minimal_demerits[fit_class] = AWFUL_BAD;
        }
        self.minimum_demerits = AWFUL_BAD;
    }

    /// See 837.
    fn compute_value_of_break_dimens(
        &mut self,
        hlist: &Vec<Node>,
        break_type: BreakType,
    ) -> HorizontalDimensions {
        let mut break_width = self.background;
        match break_type {
            BreakType::Hyphenated { disc_width } => {
                // We must be at a DiscNode.
                let Node::Disc(disc_node) = &hlist[self.cur_pos] else {
                    panic!("Should not happen")
                };

                break_width.width +=
                    self.compute_discretionary_break_width_values(disc_width, disc_node);

                // If the post_break is empty, we want to remove any following discardables
                // following the skipped material.
                if disc_node.post_break.is_empty() {
                    break_width -= self.discardable_width_after_break(hlist, self.cur_pos + 1);
                }
            }
            BreakType::Unhyphenated => {
                break_width -= self.discardable_width_after_break(hlist, self.cur_pos);
            }
            BreakType::Final => {}
        }
        break_width
    }

    /// Collect and return the width of all discardable item from the given index to the first
    /// non-discardable item (or the end of the hlist).
    /// See 837.
    fn discardable_width_after_break(
        &mut self,
        hlist: &Vec<Node>,
        mut pos: usize,
    ) -> HorizontalDimensions {
        let mut discardable_width = HorizontalDimensions::new();
        while pos < hlist.len() {
            match &hlist[pos] {
                Node::Glue(glue_node) => {
                    let glue_spec = &glue_node.glue_spec;
                    discardable_width.width += glue_spec.width;
                    discardable_width.stretch.add_dimen(glue_spec.stretch);
                    discardable_width.shrink += glue_spec.shrink.value;
                }
                Node::Penalty(_) => {}
                Node::Math(math_node) => {
                    discardable_width.width += math_node.width;
                }
                Node::Kern(kern_node) => {
                    if kern_node.subtype != KernSubtype::Explicit {
                        break;
                    } else {
                        discardable_width.width += kern_node.width;
                    }
                }
                _ => break,
            }
            pos += 1
        }
        discardable_width
    }

    /// See 840.
    fn compute_discretionary_break_width_values(
        &mut self,
        disc_width: Dimension,
        disc_node: &DiscNode,
    ) -> Dimension {
        // We add the pre break width.
        let mut disc_break_width = disc_width;

        // We add the post break width.
        for node in &disc_node.post_break {
            disc_break_width += Self::get_node_width(node);
        }

        // We subtract the width of the skipped material.
        for node in &disc_node.no_break {
            disc_break_width -= Self::get_node_width(node);
        }
        disc_break_width
    }

    /// See 841. and 842.
    fn get_node_width(node: &Node) -> Dimension {
        match node {
            &Node::Char(CharNode { width, .. })
            | &Node::Ligature(LigatureNode { width, .. })
            | &Node::List(ListNode { width, .. })
            | &Node::Rule(RuleNode { width, .. })
            | &Node::Kern(KernNode { width, .. }) => width,
            _ => panic!("disc1"),
        }
    }

    /// See 845.
    fn insert_new_active_node(
        &mut self,
        fit_class: usize,
        break_type: BreakType,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) -> ActiveNode {
        let passive = PassiveNode {
            break_pos: self.cur_pos,
            serial: self.passive.len() + 1,
            prev_break: self.best_place[fit_class],
        };
        self.passive.push(passive);

        let active = ActiveNode {
            break_node: Some(self.passive.len() - 1),
            line_number: self.best_pl_line[fit_class] + 1,
            fitness: fit_class as u8,
            typ: break_type,
            total_demerits: self.minimal_demerits[fit_class],
            dimens: self.break_dimens,
        };
        if cfg!(feature = "stats") {
            if eqtb.tracing_paragraphs() > 0 {
                self.print_symbolic_description_of_new_break_node(
                    &active, fit_class, break_type, logger,
                );
            }
        }
        active
    }

    /// See 846.
    fn print_symbolic_description_of_new_break_node(
        &self,
        active: &ActiveNode,
        fit_class: usize,
        break_type: BreakType,
        logger: &mut Logger,
    ) {
        logger.print_nl_str("@@");
        logger.print_int(self.passive.last().unwrap().serial as i32);
        logger.print_str(": line ");
        logger.print_int(active.line_number as i32 - 1);
        logger.print_char(b'.');
        logger.print_int(fit_class as i32);
        if let BreakType::Hyphenated { .. } | BreakType::Final = break_type {
            logger.print_char(b'-');
        }
        logger.print_str(" t=");
        logger.print_int(active.total_demerits);
        logger.print_str(" -> @@");
        match self.passive.last().unwrap().prev_break {
            None => logger.print_char(b'0'),
            Some(prev_break) => logger.print_int(self.passive[prev_break].serial as i32),
        };
    }

    /// Determine how many lines get special treatment and determine the corresponding
    /// withs and indentations.
    /// See 848.
    fn determine_line_classes(&mut self, eqtb: &Eqtb) {
        let par_shape = eqtb.par_shape.get(ParShapeVariable);
        // If there is no paragraph shape specified, we use the \hangindent specification.
        if par_shape.is_empty() {
            let hang_indent = eqtb.dimen(DimensionVariable::HangIndent);
            // If \hangindent is 0, all lines have the same indentation and width.
            if hang_indent == 0 {
                self.last_special_line = 0;
                self.second_width = eqtb.dimen(DimensionVariable::Hsize);
                self.second_indent = 0;
            } else {
                let hang_after = eqtb.integer(IntegerVariable::HangAfter);
                let hsize = eqtb.dimen(DimensionVariable::Hsize);
                self.set_line_length_parameters_in_preparation_for_hanging_indentation(
                    hang_indent,
                    hang_after,
                    hsize,
                );
            }
        } else {
            // Note that the last line in ParShape is the first normal line as it is the
            // default format for all other lines.
            self.last_special_line = par_shape.len() - 1;
            self.second_width = par_shape[self.last_special_line].1;
            self.second_indent = par_shape[self.last_special_line].0;
        }
        if eqtb.integer(IntegerVariable::Looseness) == 0 {
            self.easy_line = self.last_special_line;
        } else {
            self.easy_line = usize::MAX;
        }
    }

    /// If \hangafter is non-negative, all lines after and including line \hangafter are
    /// indented. Otherwise all lines up to and including line |\hangafter| are indented.
    /// The indentation is on the left if \hangindent is non-negative, otherwise the
    /// indentation is on the right. The indentation length is always |\hangindent|.
    /// See 849.
    fn set_line_length_parameters_in_preparation_for_hanging_indentation(
        &mut self,
        hang_indent: Dimension,
        hang_after: i32,
        hsize: Dimension,
    ) {
        self.last_special_line = hang_after.unsigned_abs() as usize;
        if hang_after < 0 {
            self.first_width = hsize - hang_indent.abs();
            if hang_indent >= 0 {
                // We indent on the left.
                self.first_indent = hang_indent;
            } else {
                // We indent on the right.
                self.first_indent = 0;
            }
            self.second_width = hsize;
            self.second_indent = 0;
        } else {
            self.first_width = hsize;
            self.first_indent = 0;
            self.second_width = hsize - hang_indent.abs();
            if hang_indent >= 0 {
                // We indent on the left.
                self.second_indent = hang_indent;
            } else {
                // We indent on the right.
                self.second_indent = 0;
            }
        }
    }

    /// See 850.
    fn compute_new_line_width(&self, l: usize, old_l: &mut usize, eqtb: &Eqtb) -> Dimension {
        if l > self.easy_line {
            *old_l = usize::MAX - 1;
            self.second_width
        } else {
            *old_l = l;
            let par_shape = eqtb.par_shape.get(ParShapeVariable);
            if l > self.last_special_line {
                self.second_width
            } else if par_shape.is_empty() {
                self.first_width
            } else {
                par_shape[l - 1].1
            }
        }
    }

    /// See 852.
    fn compute_badness_and_fit_class_for_stretching(
        active: &ActiveNode,
        shortfall: Scaled,
    ) -> (i32, usize) {
        let stretch = active.dimens.stretch.evaluate();
        if stretch.order == DimensionOrder::Normal {
            if shortfall > 7_230_584 && stretch.value < 1_663_497 {
                return (INF_BAD, VERY_LOOSE_FIT);
            }
            let b = calculate_badness(shortfall, stretch.value);
            let fit_class = if b > 12 {
                if b > 99 {
                    VERY_LOOSE_FIT
                } else {
                    LOOSE_FIT
                }
            } else {
                DECENT_FIT
            };
            (b, fit_class)
        } else {
            (0, DECENT_FIT)
        }
    }

    /// See 853.
    fn compute_badness_and_fit_class_for_shrinking(
        active: &ActiveNode,
        shortfall: Scaled,
    ) -> (i32, usize) {
        let b = if -shortfall > active.dimens.shrink {
            INF_BAD + 1
        } else {
            calculate_badness(-shortfall, active.dimens.shrink)
        };
        let fit_class = if b > 12 { TIGHT_FIT } else { DECENT_FIT };
        (b, fit_class)
    }

    /// See 855.
    fn record_feasible_break(
        &mut self,
        hlist: &Vec<Node>,
        active: &ActiveNode,
        break_type: BreakType,
        fit_class: usize,
        l: usize,
        b: i32,
        pi: i32,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        let mut demerits = self.compute_demerits(b, pi, active, break_type, fit_class, eqtb);
        if cfg!(feature = "stats") {
            if eqtb.tracing_paragraphs() > 0 {
                self.print_symbolic_description_of_feasible_break(
                    hlist, active, b, pi, false, demerits, eqtb, logger,
                );
            }
        }
        demerits += active.total_demerits;
        if demerits <= self.minimal_demerits[fit_class] {
            self.minimal_demerits[fit_class] = demerits;
            self.best_place[fit_class] = active.break_node;
            self.best_pl_line[fit_class] = l;
            if demerits < self.minimum_demerits {
                self.minimum_demerits = demerits;
            }
        }
    }

    /// See 855.
    fn record_artificial_break(
        &mut self,
        hlist: &Vec<Node>,
        active: &ActiveNode,
        fit_class: usize,
        l: usize,
        b: i32,
        pi: i32,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        let mut demerits = 0;
        if cfg!(feature = "stats") {
            if eqtb.tracing_paragraphs() > 0 {
                self.print_symbolic_description_of_feasible_break(
                    hlist, active, b, pi, true, demerits, eqtb, logger,
                );
            }
        }
        demerits += active.total_demerits;
        if demerits <= self.minimal_demerits[fit_class] {
            self.minimal_demerits[fit_class] = demerits;
            self.best_place[fit_class] = active.break_node;
            self.best_pl_line[fit_class] = l;
            if demerits < self.minimum_demerits {
                self.minimum_demerits = demerits;
            }
        }
    }

    /// See 856.
    fn print_symbolic_description_of_feasible_break(
        &mut self,
        hlist: &Vec<Node>,
        active: &ActiveNode,
        b: i32,
        pi: i32,
        artificial_demerits: bool,
        d: i32,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        if self.unprinted_node <= self.cur_pos {
            self.print_list_between_printed_node_and_cur_p(hlist, eqtb, logger);
        }
        logger.print_nl_str("@");
        if self.cur_pos >= hlist.len() {
            logger.print_esc_str(b"par");
        } else {
            match &hlist[self.cur_pos] {
                Node::Glue(_) => {}
                Node::Penalty(_) => {
                    logger.print_esc_str(b"penalty");
                }
                Node::Disc(_) => {
                    logger.print_esc_str(b"discretionary");
                }
                Node::Kern(_) => {
                    logger.print_esc_str(b"kern");
                }
                Node::Math(_) => {
                    logger.print_esc_str(b"math");
                }
                _ => panic!("Should not happen"),
            }
        }
        logger.print_str(" via @@");
        match active.break_node {
            None => logger.print_char(b'0'),
            Some(break_node) => logger.print_int(self.passive[break_node].serial as i32),
        };
        logger.print_str(" b=");
        if b > INF_BAD {
            logger.print_char(b'*');
        } else {
            logger.print_int(b);
        }
        logger.print_str(" p=");
        logger.print_int(pi);
        logger.print_str(" d=");
        if artificial_demerits {
            logger.print_char(b'*');
        } else {
            logger.print_int(d);
        }
    }

    /// See 857.
    fn print_list_between_printed_node_and_cur_p(
        &mut self,
        hlist: &Vec<Node>,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        logger.print_nl_str("");
        if self.cur_pos == hlist.len() {
            short_display(&hlist[self.unprinted_node..], eqtb, logger);
        } else {
            logger.print_nl_str("");
            short_display(&hlist[self.unprinted_node..=self.cur_pos], eqtb, logger);
        }
        self.unprinted_node = self.cur_pos + 1;
    }

    /// See 859.
    fn compute_demerits(
        &self,
        b: i32,
        pi: i32,
        active: &ActiveNode,
        break_type: BreakType,
        fit_class: usize,
        eqtb: &Eqtb,
    ) -> i32 {
        let mut demerits = eqtb.integer(IntegerVariable::LinePenalty) + b;
        if demerits.abs() >= 10_000 {
            demerits = 100_000_000;
        } else {
            demerits *= demerits;
        }
        if pi != 0 {
            if pi > 0 {
                demerits += pi * pi;
            } else if pi > EJECT_PENALTY {
                demerits -= pi * pi;
            }
        }
        if let BreakType::Hyphenated { .. } = active.typ {
            match break_type {
                BreakType::Hyphenated { .. } => {
                    demerits += eqtb.integer(IntegerVariable::DoubleHyphenDemerits);
                }
                BreakType::Final => {
                    demerits += eqtb.integer(IntegerVariable::FinalHyphenDemerits);
                }
                BreakType::Unhyphenated => {}
            }
        }
        if (fit_class as i32 - active.fitness as i32).abs() > 1 {
            demerits += eqtb.integer(IntegerVariable::AdjDemerits);
        }
        demerits
    }

    /// Return the ActiveNode that corresponds to the best break sequence.
    /// See 863.
    fn find_optimal_break_points(
        &mut self,
        mut hlist: Vec<Node>,
        hyphenator: &mut Hyphenator,
        prev_graf: i32,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> (ActiveNode, Vec<Node>) {
        let pretolerance = eqtb.integer(IntegerVariable::Pretolerance);
        let tolerance = eqtb.integer(IntegerVariable::Tolerance);
        let emergency_stretch = eqtb.dimen(DimensionVariable::EmergencyStretch);

        self.threshold = pretolerance;
        if self.threshold >= 0 {
            if cfg!(feature = "stats") {
                if eqtb.tracing_paragraphs() > 0 {
                    logger.begin_diagnostic(eqtb.tracing_online());
                    logger.print_nl_str("@firstpass");
                }
            }
            self.second_pass = false;
            self.final_pass = false;
        } else {
            self.threshold = tolerance;
            self.second_pass = true;
            self.final_pass = emergency_stretch <= 0;
            if cfg!(feature = "stats") {
                if eqtb.tracing_paragraphs() > 0 {
                    logger.begin_diagnostic(eqtb.tracing_online());
                }
            }
        }

        let best_bet = loop {
            if self.threshold > INF_BAD {
                self.threshold = INF_BAD;
            }
            if self.second_pass {
                hlist = hyphenator.hyphenate_hlist(hlist, scanner, eqtb, logger);
            }

            self.create_active_breakpoint_representing_beginning_of_paragraph(prev_graf, logger);
            self.cur_pos = 0;
            let mut auto_breaking = true;
            // NOTE This avoids breaking on initial glue.
            let mut prev_pos = 0;
            while self.cur_pos < hlist.len() && !self.actives.is_empty() {
                self.call_try_break_if_cur_p_is_legal_breakpoint(
                    &hlist,
                    &mut auto_breaking,
                    &mut prev_pos,
                    eqtb,
                    logger,
                );
            }
            if self.cur_pos == hlist.len() {
                if let Some(best_bet) =
                    self.try_final_line_break_at_end_of_paragraph(&hlist, eqtb, logger)
                {
                    break best_bet;
                }
            }

            self.clean_up_memory_by_removing_break_nodes();
            if !self.second_pass {
                if cfg!(feature = "stats") {
                    if eqtb.tracing_paragraphs() > 0 {
                        logger.print_nl_str("@secondpass");
                    }
                }
                self.threshold = tolerance;
                self.second_pass = true;
                self.final_pass = emergency_stretch <= 0;
            } else {
                if cfg!(feature = "stats") {
                    if eqtb.tracing_paragraphs() > 0 {
                        logger.print_nl_str("@emergencypass");
                    }
                }
                self.background.stretch.add_dimen(HigherOrderDimension {
                    order: DimensionOrder::Normal,
                    value: emergency_stretch,
                });
                self.final_pass = true;
            }
        };
        if cfg!(feature = "stats") {
            if eqtb.tracing_paragraphs() > 0 {
                logger.end_diagnostic(true);
            }
        }
        (best_bet, hlist)
    }

    /// See 864.
    fn create_active_breakpoint_representing_beginning_of_paragraph(
        &mut self,
        prev_graf: i32,
        logger: &mut Logger,
    ) {
        self.delta = HorizontalDimensions::new();
        self.unprinted_node = 0;
        logger.font_in_short_display = NULL_FONT;

        let active = ActiveNode {
            typ: BreakType::Unhyphenated,
            fitness: DECENT_FIT as u8,
            break_node: None,
            line_number: prev_graf as usize + 1,
            total_demerits: 0,
            dimens: self.background,
        };

        self.actives.push(active);
    }

    /// See 865.
    fn clean_up_memory_by_removing_break_nodes(&mut self) {
        self.actives.clear();
        self.passive.clear();
    }

    /// See 866.
    fn kern_break(
        &mut self,
        hlist: &Vec<Node>,
        auto_breaking: bool,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        if auto_breaking {
            if let Node::Glue(_) = hlist[self.cur_pos + 1] {
                self.try_break(hlist, 0, BreakType::Unhyphenated, eqtb, logger);
            }
        }
    }

    /// See 866.
    fn call_try_break_if_cur_p_is_legal_breakpoint(
        &mut self,
        hlist: &Vec<Node>,
        auto_breaking: &mut bool,
        prev_pos: &mut usize,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        match &hlist[self.cur_pos] {
            Node::Char(_) => {
                self.advance_cur_p_to_node_following_present_string_of_characters(hlist, prev_pos);
                return;
            }
            Node::List(list_node) => {
                self.delta.width += list_node.width;
            }
            Node::Rule(rule_node) => {
                self.delta.width += rule_node.width;
            }
            Node::Whatsit(_) => {}
            Node::Glue(glue_node) => {
                self.if_node_cur_p_is_legal_breakpoint(
                    hlist,
                    glue_node,
                    *auto_breaking,
                    *prev_pos,
                    eqtb,
                    logger,
                );
            }
            Node::Kern(kern_node) => {
                if kern_node.subtype == KernSubtype::Explicit {
                    self.kern_break(hlist, *auto_breaking, eqtb, logger);
                }
                self.delta.width += kern_node.width;
            }
            Node::Ligature(ligature_node) => {
                self.delta.width += ligature_node.width;
            }
            Node::Disc(disc_node) => {
                self.try_to_break_after_discretionary(hlist, disc_node, eqtb, logger);
            }
            Node::Math(math_node) => {
                *auto_breaking = match math_node.kind {
                    MathNodeKind::Before => false,
                    MathNodeKind::After => true,
                };
                self.kern_break(hlist, *auto_breaking, eqtb, logger);
                self.delta.width += math_node.width;
            }
            Node::Penalty(penalty_node) => {
                self.try_break(
                    hlist,
                    penalty_node.penalty,
                    BreakType::Unhyphenated,
                    eqtb,
                    logger,
                );
            }
            Node::Mark(_) | Node::Ins(_) | Node::Adjust(_) => {}
            _ => panic!("paragraph"),
        }
        *prev_pos = self.cur_pos;
        self.cur_pos += 1;
    }

    /// See 867.
    fn advance_cur_p_to_node_following_present_string_of_characters(
        &mut self,
        hlist: &Vec<Node>,
        prev_pos: &mut usize,
    ) {
        *prev_pos = self.cur_pos;
        while let Node::Char(char_node) = &hlist[self.cur_pos] {
            self.delta.width += char_node.width;
            self.cur_pos += 1;
        }
    }

    /// See 868.
    fn if_node_cur_p_is_legal_breakpoint(
        &mut self,
        hlist: &Vec<Node>,
        glue_node: &GlueNode,
        auto_breaking: bool,
        prev_pos: usize,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        if auto_breaking && hlist[prev_pos].precedes_break() {
            self.try_break(hlist, 0, BreakType::Unhyphenated, eqtb, logger);
        }
        let glue_spec = &glue_node.glue_spec;
        self.delta.width += glue_spec.width;
        self.delta.stretch.add_dimen(glue_spec.stretch);
        self.delta.shrink += glue_spec.shrink.value;
    }

    /// See 869.
    fn try_to_break_after_discretionary(
        &mut self,
        hlist: &Vec<Node>,
        disc_node: &DiscNode,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        let pre_break = &disc_node.pre_break;
        if pre_break.is_empty() {
            self.try_break(
                hlist,
                eqtb.integer(IntegerVariable::ExHyphenPenalty),
                BreakType::Hyphenated { disc_width: 0 },
                eqtb,
                logger,
            );
        } else {
            let mut disc_width = 0;
            for node in pre_break {
                disc_width += Self::width_of_node(node);
            }
            self.delta.width += disc_width;
            self.try_break(
                hlist,
                eqtb.integer(IntegerVariable::HyphenPenalty),
                BreakType::Hyphenated { disc_width },
                eqtb,
                logger,
            );
            self.delta.width -= disc_width;
        }
        for node in &disc_node.no_break {
            self.add_width_of_node_to_act_width(node);
        }
    }

    /// See 870.
    fn width_of_node(node: &Node) -> Dimension {
        match node {
            &Node::Char(CharNode { width, .. })
            | &Node::Ligature(LigatureNode { width, .. })
            | &Node::List(ListNode { width, .. })
            | &Node::Rule(RuleNode { width, .. })
            | &Node::Kern(KernNode { width, .. }) => width,
            _ => panic!("disc3"),
        }
    }

    /// See 871.
    fn add_width_of_node_to_act_width(&mut self, node: &Node) {
        match node {
            &Node::Char(CharNode { width, .. })
            | &Node::Ligature(LigatureNode { width, .. })
            | &Node::List(ListNode { width, .. })
            | &Node::Rule(RuleNode { width, .. })
            | &Node::Kern(KernNode { width, .. }) => {
                self.delta.width += width;
            }
            _ => panic!("disc4"),
        }
    }

    /// Return the acceptable ActiveNode with the fewest demerits if it exists, else None.
    /// See 873.
    fn try_final_line_break_at_end_of_paragraph(
        &mut self,
        hlist: &Vec<Node>,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) -> Option<ActiveNode> {
        self.try_break(hlist, EJECT_PENALTY, BreakType::Final, eqtb, logger);
        if !self.actives.is_empty() {
            let best_bet = self.find_active_node_with_fewest_demerits();
            if eqtb.integer(IntegerVariable::Looseness) == 0 {
                return Some(best_bet);
            }
            let best_loose_bet = self.find_best_active_node_for_desired_looseness(&best_bet, eqtb);
            if (best_loose_bet.line_number as i32) - best_bet.line_number as i32
                == eqtb.integer(IntegerVariable::Looseness)
                || self.final_pass
            {
                return Some(best_loose_bet);
            }
        }
        None
    }

    /// Return the ActiveNode with the fewest demerits.
    /// See 874.
    fn find_active_node_with_fewest_demerits(&self) -> ActiveNode {
        let mut fewest_demerits = self.actives[0].total_demerits;
        let mut best_bet = self.actives[0].clone();
        for active_break in &self.actives[1..] {
            if active_break.total_demerits < fewest_demerits {
                fewest_demerits = active_break.total_demerits;
                best_bet = active_break.clone();
            }
        }
        best_bet
    }

    /// Return the ActiveNode that best corresponds to the desired looseness.
    /// See 875.
    fn find_best_active_node_for_desired_looseness(
        &self,
        best_bet: &ActiveNode,
        eqtb: &Eqtb,
    ) -> ActiveNode {
        let mut best_loose_bet = best_bet.clone();
        let mut actual_looseness = 0;
        let mut fewest_demerits = best_bet.total_demerits;
        for active_break in &self.actives {
            let line_diff = active_break.line_number as i32 - best_bet.line_number as i32;
            if (line_diff < actual_looseness
                && eqtb.integer(IntegerVariable::Looseness) <= line_diff)
                || (line_diff > actual_looseness
                    && eqtb.integer(IntegerVariable::Looseness) >= line_diff)
            {
                fewest_demerits = active_break.total_demerits;
                best_loose_bet = active_break.clone();
                actual_looseness = line_diff;
            } else if line_diff == actual_looseness && active_break.total_demerits < fewest_demerits
            {
                fewest_demerits = active_break.total_demerits;
                best_loose_bet = active_break.clone();
            }
        }
        best_loose_bet
    }

    /// Returns a pointer to just_box.
    /// See 876., 877., and 880. to 887.
    fn post_line_break(
        &self,
        hlist: Vec<Node>,
        best_bet: ActiveNode,
        final_widow_penalty: i32,
        give_pre_display_size: bool,
        nest: &mut SemanticState,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> Dimension {
        let mut break_points = self.determine_break_points(&best_bet);

        let RichMode::Vertical(vmode) = nest.mode_mut() else {
            panic!("We expected to be in vertical mode here");
        };
        let mut cur_line_number = vmode.prev_graf as usize + 1;

        let mut nodes = hlist.into_iter().enumerate();
        let mut line = Vec::new();

        let right_skip_node = Node::Glue(GlueNode::new_param(SkipVariable::RightSkip, eqtb));
        let left_skip_node = Node::Glue(GlueNode::new_param(SkipVariable::LeftSkip, eqtb));
        let left_skip_not_zero = **eqtb.left_skip() != GlueSpec::ZERO_GLUE;

        if left_skip_not_zero {
            line.push(left_skip_node.clone());
        }

        while let Some((pos, node)) = nodes.next() {
            if break_points.last() == Some(&pos) {
                break_points.pop();
                if let Node::Disc(disc_node) = node {
                    // We push an empty DiscNode onto the current line.
                    // This is done solely to preserve compatibility with TeX.
                    line.push(Node::Disc(DiscNode::new()));

                    // Transplant pre-break nodes
                    for node in disc_node.pre_break {
                        line.push(node);
                    }

                    // Append right skip
                    line.push(right_skip_node.clone());

                    let (packed_line, adjustment_material) =
                        self.call_packaging_subroutine(line, cur_line_number, eqtb, logger);
                    Self::append_new_box_to_current_vertical_list(
                        packed_line,
                        adjustment_material,
                        nest,
                        eqtb,
                    );
                    Self::append_penalty_node_if_appropriate(
                        cur_line_number,
                        final_widow_penalty,
                        true,
                        &best_bet,
                        nest,
                        eqtb,
                    );
                    cur_line_number += 1;

                    line = Vec::new();
                    if left_skip_not_zero {
                        line.push(left_skip_node.clone());
                    }

                    if disc_node.post_break.is_empty() {
                        // We don't want to get the next node yet if it is a break point.
                        if break_points.last() != Some(&(pos + 1)) {
                            match Self::prune_unwanted_nodes_at_beginning_of_line(
                                &mut nodes,
                                &break_points,
                            ) {
                                None => {}
                                Some(node) => line.push(node),
                            }
                        }
                    } else {
                        for node in disc_node.post_break {
                            line.push(node);
                        }
                    }
                    continue;
                // The following code is mostly for complete compatibility with TeX82. See 881.
                } else if let Node::Math(math_node) = &node {
                    let mut empty_math_node = math_node.clone();
                    empty_math_node.width = 0;
                    line.push(Node::Math(empty_math_node));
                } else if let Node::Kern(kern_node) = &node {
                    let mut empty_kern_node = kern_node.clone();
                    empty_kern_node.width = 0;
                    line.push(Node::Kern(empty_kern_node));
                } else if let Node::Glue(_) = &node {
                    // Drop it.
                } else {
                    line.push(node.clone());
                }

                // Append right skip
                line.push(Node::Glue(GlueNode::new_param(
                    SkipVariable::RightSkip,
                    eqtb,
                )));

                let (packed_line, adjustment_material) =
                    self.call_packaging_subroutine(line, cur_line_number, eqtb, logger);
                Self::append_new_box_to_current_vertical_list(
                    packed_line,
                    adjustment_material,
                    nest,
                    eqtb,
                );
                Self::append_penalty_node_if_appropriate(
                    cur_line_number,
                    final_widow_penalty,
                    false,
                    &best_bet,
                    nest,
                    eqtb,
                );
                cur_line_number += 1;

                line = Vec::new();

                if left_skip_not_zero {
                    line.push(left_skip_node.clone());
                }

                if node.is_discardable() {
                    // We don't want to get the next node yet if it is a break point.
                    if break_points.last() != Some(&(pos + 1)) {
                        match Self::prune_unwanted_nodes_at_beginning_of_line(
                            &mut nodes,
                            &break_points,
                        ) {
                            None => {}
                            Some(node) => line.push(node),
                        }
                    }
                } else {
                    line.push(node);
                }
            } else {
                line.push(node);
            }
        }

        // Append right skip
        line.push(Node::Glue(GlueNode::new_param(
            SkipVariable::RightSkip,
            eqtb,
        )));

        let (packed_line, adjustment_material) =
            self.call_packaging_subroutine(line, cur_line_number, eqtb, logger);

        // Calculate the pre_display_size if requested.
        let pre_display_size = if give_pre_display_size {
            calculate_natural_width(&packed_line, eqtb)
        } else {
            MAX_DIMEN
        };

        Self::append_new_box_to_current_vertical_list(packed_line, adjustment_material, nest, eqtb);
        cur_line_number += 1;

        if cur_line_number != best_bet.line_number {
            panic!("line breaking");
        }
        let RichMode::Vertical(vmode) = nest.mode_mut() else {
            panic!("We expected to be in vertical mode here");
        };

        vmode.set_prev_graf(best_bet.line_number as i32 - 1, eqtb);

        pre_display_size
    }

    /// Returns the break points in reverse order.
    /// See 878.
    fn determine_break_points(&self, best_bet: &ActiveNode) -> Vec<usize> {
        let mut break_points = Vec::new();
        let mut q = best_bet.break_node;
        while let Some(i) = q {
            break_points.push(self.passive[i].break_pos);
            q = self.passive[i].prev_break;
        }
        break_points
    }

    /// See 879.
    fn prune_unwanted_nodes_at_beginning_of_line(
        nodes: &mut impl Iterator<Item = (usize, Node)>,
        break_points: &Vec<usize>,
    ) -> Option<Node> {
        while let Some((pos, node)) = nodes.next() {
            if !node.is_discardable() {
                return Some(node);
            }
            if break_points.last() == Some(&(pos + 1)) {
                return None;
            }
        }
        None
    }

    /// See 888.
    fn append_new_box_to_current_vertical_list(
        packed_line: ListNode,
        adjustment_material: Vec<Node>,
        nest: &mut SemanticState,
        eqtb: &mut Eqtb,
    ) {
        let RichMode::Vertical(vmode) = nest.mode_mut() else {
            panic!("We expected to be in vertical mode here");
        };
        vmode.append_to_vlist(packed_line, eqtb);
        nest.tail_append(adjustment_material, eqtb);
    }

    /// See 889.
    fn call_packaging_subroutine(
        &self,
        mut line: Vec<Node>,
        line_number: usize,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> (ListNode, Vec<Node>) {
        let cur_width;
        let cur_indent;
        let par_shape = eqtb.par_shape.get(ParShapeVariable);
        if line_number > self.last_special_line {
            cur_width = self.second_width;
            cur_indent = self.second_indent;
        } else if par_shape.is_empty() {
            cur_width = self.first_width;
            cur_indent = self.first_indent;
        } else {
            cur_width = par_shape[line_number - 1].1;
            cur_indent = par_shape[line_number - 1].0;
        }
        let adjustment_material = split_adjust_material_off(&mut line);
        let mut packed_line = hpack(line, TargetSpec::Exactly(cur_width), eqtb, logger);
        packed_line.shift_amount = cur_indent;
        (packed_line, adjustment_material)
    }

    /// See 890.
    fn append_penalty_node_if_appropriate(
        cur_line_number: usize,
        final_widow_penalty: i32,
        disc_break: bool,
        best_bet: &ActiveNode,
        nest: &mut SemanticState,
        eqtb: &mut Eqtb,
    ) {
        let mut pen = eqtb.integer(IntegerVariable::InterLinePenalty);
        let RichMode::Vertical(vmode) = nest.mode_mut() else {
            panic!("We expected to be in vertical mode here");
        };
        if cur_line_number == vmode.prev_graf as usize + 1 {
            pen += eqtb.integer(IntegerVariable::ClubPenalty);
        }
        if cur_line_number + 2 == best_bet.line_number {
            pen += final_widow_penalty;
        }
        if disc_break {
            pen += eqtb.integer(IntegerVariable::BrokenPenalty);
        }
        if pen != 0 {
            nest.tail_push(Node::Penalty(PenaltyNode::new(pen)), eqtb);
        }
    }
}

/// See 1146. and 1148.
fn calculate_natural_width(just_box: &ListNode, eqtb: &Eqtb) -> Dimension {
    let mut total_width = just_box.shift_amount + 2 * eqtb.fonts[eqtb.cur_font() as usize].quad();
    let mut visible_width = -MAX_DIMEN;
    let list = match &just_box.list {
        HlistOrVlist::Hlist(list) => list,
        HlistOrVlist::Vlist(list) => list,
    };
    let mut flexible_glue_found = false;
    for node in list {
        if let Node::Glue(glue_node) = node {
            let glue_spec = &glue_node.glue_spec;
            if just_box.glue_sign == GlueSign::Stretching {
                if just_box.glue_order == glue_spec.stretch.order && glue_spec.stretch.value != 0 {
                    flexible_glue_found = true;
                }
            } else if just_box.glue_sign == GlueSign::Shrinking {
                if just_box.glue_order == glue_spec.shrink.order && glue_spec.shrink.value != 0 {
                    flexible_glue_found = true;
                }
            }
        }
        let (is_visible, width) = determine_visibility_and_width(node);
        total_width += width;

        if is_visible {
            if flexible_glue_found {
                return MAX_DIMEN;
            } else {
                visible_width = total_width;
            }
        }
    }
    visible_width
}

/// See 1147. and 1148.
fn determine_visibility_and_width(node: &Node) -> (bool, Dimension) {
    match node {
        &Node::Char(CharNode { width, .. })
        | &Node::Ligature(LigatureNode { width, .. })
        | &Node::List(ListNode { width, .. })
        | &Node::Rule(RuleNode { width, .. }) => (true, width),
        &Node::Kern(KernNode { width, .. }) | &Node::Math(MathNode { width, .. }) => (false, width),
        Node::Glue(glue_node) => {
            if let GlueType::Leaders { .. } = glue_node.subtype {
                (true, glue_node.glue_spec.width)
            } else {
                (false, glue_node.glue_spec.width)
            }
        }
        _ => (false, 0),
    }
}

#[derive(Debug, Clone, Copy)]
enum BreakType {
    Unhyphenated,
    Hyphenated { disc_width: Dimension },
    Final,
}

/// See 823.
#[derive(Clone, Copy, Debug)]
struct HorizontalDimensions {
    width: Dimension,
    // Shrinkage must be finite.
    shrink: Dimension,
    stretch: GlueCollector,
}

impl HorizontalDimensions {
    const fn new() -> Self {
        Self {
            width: 0,
            // Shrinkage must be finite.
            shrink: 0,
            stretch: GlueCollector::new(),
        }
    }
}

impl Add for HorizontalDimensions {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        Self {
            width: self.width + rhs.width,
            shrink: self.shrink + rhs.shrink,
            stretch: self.stretch + rhs.stretch,
        }
    }
}

impl AddAssign for HorizontalDimensions {
    fn add_assign(&mut self, rhs: Self) {
        self.width += rhs.width;
        self.shrink += rhs.shrink;
        self.stretch += rhs.stretch;
    }
}

impl Neg for HorizontalDimensions {
    type Output = Self;
    fn neg(self) -> Self {
        Self {
            width: -self.width,
            shrink: -self.shrink,
            stretch: -self.stretch,
        }
    }
}

impl Sub for HorizontalDimensions {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        Self {
            width: self.width - rhs.width,
            shrink: self.shrink - rhs.shrink,
            stretch: self.stretch - rhs.stretch,
        }
    }
}

impl SubAssign for HorizontalDimensions {
    fn sub_assign(&mut self, rhs: Self) {
        self.width -= rhs.width;
        self.shrink -= rhs.shrink;
        self.stretch -= rhs.stretch;
    }
}
