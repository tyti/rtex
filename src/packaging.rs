use crate::dimension::{scan_normal_dimen, Dimension, MAX_DIMEN};
use crate::eqtb::{DimensionVariable, Eqtb, IntegerVariable, NULL_FONT};
use crate::input::Scanner;
use crate::logger::Logger;
use crate::nodes::{
    short_display, show_box, AdjustNode, CharNode, DimensionOrder, GlueNode, GlueSign, GlueType,
    HigherOrderDimension, HlistOrVlist, KernNode, LigatureNode, ListNode, MathNode, Node, RuleNode,
    UnsetNode,
};
use crate::print::Printer;
use crate::scaled::{calculate_badness, Scaled};

use std::ops::{Add, AddAssign, Neg, Sub, SubAssign};

/// See 644.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TargetSpec {
    /// The natural size.
    Natural,
    /// The natural size plus the specified dimension.
    Additional(Dimension),
    /// The specified dimension.
    Exactly(Dimension),
}

/// Returns spec_code and Scaled value.
/// See 645.
pub fn scan_spec(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) -> TargetSpec {
    if scanner.scan_keyword(b"to", eqtb, logger) {
        let dim = scan_normal_dimen(scanner, eqtb, logger);
        TargetSpec::Exactly(dim)
    } else if scanner.scan_keyword(b"spread", eqtb, logger) {
        let dim = scan_normal_dimen(scanner, eqtb, logger);
        TargetSpec::Additional(dim)
    } else {
        TargetSpec::Natural
    }
}

pub struct Measurement {
    pub width: Dimension,
    pub height: Dimension,
    pub depth: Dimension,
    stretch: HigherOrderDimension,
    pub shrink: HigherOrderDimension,
}

/// Uses the given hlist and creates an hbox with the given target size.
/// See 649.
pub fn hpack(hlist: Vec<Node>, spec: TargetSpec, eqtb: &mut Eqtb, logger: &mut Logger) -> ListNode {
    let measurement = measure_hlist(&hlist);
    pack_hbox(hlist, spec, measurement, eqtb, logger)
}

/// Splits off adjustment material from the given hlist. Returns a pair
/// of the reduced hlist and the adjust material.
/// See 651. and 655.
pub fn split_adjust_material_off(hlist: &mut Vec<Node>) -> Vec<Node> {
    let mut adjust_material = Vec::new();
    // NOTE Consider using Vec::drain_filter/extract_if once it's stable.
    let mut i = 0;
    while i < hlist.len() {
        match &mut hlist[i] {
            Node::Ins(_) | Node::Mark(_) => {
                let node = hlist.remove(i);
                adjust_material.push(node);
            }
            Node::Adjust(AdjustNode { adjustment }) => {
                adjust_material.append(adjustment);
                hlist.remove(i);
            }
            Node::Char(_)
            | Node::Ligature(_)
            | Node::List(_)
            | Node::Rule(_)
            | Node::Whatsit(_)
            | Node::Glue(_)
            | Node::Kern(_)
            | Node::Math(_)
            | Node::Disc(_)
            | Node::Penalty(_) => {
                i += 1;
            }
            Node::Unset(_) | Node::Noad(_) | Node::Style(_) | Node::Choice(_) => {
                panic!("Should not appear here");
            }
        }
    }
    adjust_material
}

/// See 651., 653., 654., 655., and 656.
pub fn measure_hlist(hlist: &Vec<Node>) -> Measurement {
    let mut max_height = 0;
    let mut max_depth = 0;
    let mut total_width = 0;
    let mut stretch_collector = GlueCollector::new();
    let mut shrink_collector = GlueCollector::new();
    for node in hlist {
        match node {
            &Node::Char(CharNode {
                width,
                height,
                depth,
                ..
            })
            | &Node::Ligature(LigatureNode {
                width,
                height,
                depth,
                ..
            })
            | &Node::Rule(RuleNode {
                width,
                height,
                depth,
            })
            | &Node::Unset(UnsetNode {
                width,
                height,
                depth,
                ..
            }) => {
                total_width += width;
                if height > max_height {
                    max_height = height;
                }
                if depth > max_depth {
                    max_depth = depth;
                }
            }
            Node::List(ListNode {
                width,
                height,
                depth,
                shift_amount,
                ..
            }) => {
                total_width += width;
                if height - shift_amount > max_height {
                    max_height = height - shift_amount;
                }
                if depth + shift_amount > max_depth {
                    max_depth = depth + shift_amount;
                }
            }
            Node::Ins(_) | Node::Mark(_) | Node::Adjust(_) => {
                // The adjustment material has been moved earlier if necessary.
            }
            Node::Whatsit(_) => {
                // Whatsits have no size.
            }
            Node::Glue(GlueNode { glue_spec, subtype }) => {
                // From 656.
                total_width += glue_spec.width;
                stretch_collector.add_dimen(glue_spec.stretch);
                shrink_collector.add_dimen(glue_spec.shrink);
                if let GlueType::Leaders { leader_node, .. } = subtype {
                    match **leader_node {
                        // NOTE ListNodes in leaders always have zero shift amount.
                        Node::List(ListNode { height, depth, .. })
                        | Node::Rule(RuleNode { height, depth, .. }) => {
                            if height > max_height {
                                max_height = height;
                            }
                            if depth > max_depth {
                                max_depth = depth;
                            }
                        }
                        _ => panic!("Should not happen"),
                    }
                }
            }
            Node::Kern(KernNode { width, .. }) | Node::Math(MathNode { width, .. }) => {
                total_width += width;
            }
            Node::Disc(disc_node) => {
                for node in &disc_node.no_break {
                    measure_disc_internal_node(
                        node,
                        &mut total_width,
                        &mut max_height,
                        &mut max_depth,
                    )
                }
            }
            Node::Penalty(_) => {}
            Node::Style(_) | Node::Choice(_) | Node::Noad(_) => {
                panic!("Should not appear here");
            }
        }
    }
    Measurement {
        width: total_width,
        height: max_height,
        depth: max_depth,
        stretch: stretch_collector.evaluate(),
        shrink: shrink_collector.evaluate(),
    }
}

fn measure_disc_internal_node(
    node: &Node,
    total_width: &mut Dimension,
    max_height: &mut Dimension,
    max_depth: &mut Dimension,
) {
    match node {
        &Node::Char(CharNode {
            width,
            height,
            depth,
            ..
        })
        | &Node::Ligature(LigatureNode {
            width,
            height,
            depth,
            ..
        })
        | &Node::Rule(RuleNode {
            width,
            height,
            depth,
        }) => {
            *total_width += width;
            if height > *max_height {
                *max_height = height;
            }
            if depth > *max_depth {
                *max_depth = depth;
            }
        }
        Node::List(ListNode {
            width,
            height,
            depth,
            shift_amount,
            ..
        }) => {
            // From 653.
            *total_width += width;
            if height - shift_amount > *max_height {
                *max_height = height - shift_amount;
            }
            if depth + shift_amount > *max_depth {
                *max_depth = depth + shift_amount;
            }
        }
        Node::Kern(KernNode { width, .. }) => {
            *total_width += width;
        }
        _ => panic!("This node type should not appear in a discretionary"),
    }
}

/// Create an hbox corresponding to the target size and the measured internal size.
/// Attempt to stretch or shrink the internal list as needed.
/// See 657.
fn pack_hbox(
    hlist: Vec<Node>,
    spec: TargetSpec,
    measurement: Measurement,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> ListNode {
    let box_width = match spec {
        TargetSpec::Natural => measurement.width,
        TargetSpec::Additional(w) => w + measurement.width,
        TargetSpec::Exactly(w) => w,
    };
    let mut hbox = ListNode {
        width: box_width,
        height: measurement.height,
        depth: measurement.depth,
        shift_amount: 0,
        list: HlistOrVlist::Hlist(hlist),
        glue_set: 0.0,
        glue_sign: GlueSign::Normal,
        glue_order: DimensionOrder::Normal,
    };
    let x = box_width - measurement.width;
    eqtb.last_badness = if x > 0 {
        determine_horizontal_glue_stretch_setting(&mut hbox, measurement.stretch, x, eqtb, logger)
    } else if x < 0 {
        determine_horizontal_glue_shrink_setting(&mut hbox, measurement.shrink, -x, eqtb, logger)
    } else {
        0
    };
    hbox
}

/// Sets glue settings for the HlistNode corresponding to the measured stretchability.
/// Reports an underfull (or loose) hbox in case of insufficient stretchability.
/// Returns the corresponding badness.
/// See 658.
fn determine_horizontal_glue_stretch_setting(
    hbox: &mut ListNode,
    stretch: HigherOrderDimension,
    x: Dimension,
    eqtb: &Eqtb,
    logger: &mut Logger,
) -> i32 {
    if stretch.value != 0 {
        hbox.glue_set = x as f64 / stretch.value as f64;
        hbox.glue_order = stretch.order;
        hbox.glue_sign = GlueSign::Stretching;
    }
    let badness;
    match &hbox.list {
        HlistOrVlist::Hlist(hlist) => {
            // Empty hboxes are never considered underfull.
            if !hlist.is_empty() && stretch.order == DimensionOrder::Normal {
                badness = calculate_badness(x, stretch.value);
                if badness > eqtb.integer(IntegerVariable::Hbadness) {
                    report_underful_hbox(hbox, badness, eqtb, logger);
                }
            } else {
                badness = 0;
            }
        }
        _ => panic!("Impossible"),
    }
    badness
}

/// See 660.
fn report_underful_hbox(hbox: &ListNode, badness: i32, eqtb: &Eqtb, logger: &mut Logger) {
    logger.print_ln();
    if badness > 100 {
        logger.print_nl_str("Underfull");
    } else {
        logger.print_nl_str("Loose");
    }
    logger.print_str(" \\hbox (badness ");
    logger.print_int(badness);
    finish_issuing_diagnostic_message_for_hbox(hbox, eqtb, logger);
}

/// See 663.
fn finish_issuing_diagnostic_message_for_hbox(hbox: &ListNode, eqtb: &Eqtb, logger: &mut Logger) {
    if eqtb.output_active {
        logger.print_str(") has occurred while \\output is active");
    } else {
        if logger.pack_begin_line != 0 {
            if logger.pack_begin_line > 0 {
                logger.print_str(") in paragraph at lines ");
            } else {
                logger.print_str(") in alignment at lines ");
            }
            logger.print_int(logger.pack_begin_line.abs());
            logger.print_str("--");
        } else {
            logger.print_str(") detected at line ");
        }
        logger.print_int(eqtb.line_number() as i32);
    }
    logger.print_ln();
    logger.font_in_short_display = NULL_FONT;
    match &hbox.list {
        HlistOrVlist::Hlist(hlist) => {
            short_display(hlist, eqtb, logger);
        }
        _ => panic!("Impossible"),
    }
    logger.print_ln();
    logger.begin_diagnostic(eqtb.tracing_online());
    show_box(hbox, eqtb, logger);
    logger.end_diagnostic(true);
}

/// Sets glue settings for the HlistNode corresponding to the measured shrinkability.
/// Reports an overfull or tight hbox in case of insufficient shrinkability.
/// Returns the corresponding badness.
/// See 664.
fn determine_horizontal_glue_shrink_setting(
    hbox: &mut ListNode,
    shrink: HigherOrderDimension,
    x: Dimension,
    eqtb: &Eqtb,
    logger: &mut Logger,
) -> i32 {
    if shrink.value != 0 {
        hbox.glue_set = x as f64 / shrink.value as f64;
        hbox.glue_order = shrink.order;
        hbox.glue_sign = GlueSign::Shrinking;
    }
    let badness;
    match &hbox.list {
        HlistOrVlist::Hlist(hlist) => {
            if shrink.order == DimensionOrder::Normal && !hlist.is_empty() {
                if shrink.value < x {
                    // We never allow normal glue to shrink by more than a factor of one.
                    let excess_width = x - shrink.value;
                    badness = 1_000_000;
                    hbox.glue_set = 1.0;
                    if excess_width > eqtb.dimen(DimensionVariable::Hfuzz)
                        || eqtb.integer(IntegerVariable::Hbadness) < 100
                    {
                        report_overfull_hbox(excess_width, hbox, eqtb, logger);
                    }
                } else {
                    badness = calculate_badness(x, shrink.value);
                    if badness > eqtb.integer(IntegerVariable::Hbadness) {
                        report_tight_hbox(hbox, badness, eqtb, logger);
                    }
                }
            } else {
                badness = 0;
            }
        }
        _ => panic!("Impossbile"),
    }
    badness
}

/// See 666.
fn report_overfull_hbox(
    excess_width: Dimension,
    hbox: &mut ListNode,
    eqtb: &Eqtb,
    logger: &mut Logger,
) {
    // If the excess width excess \hfuzz and if \overfullrule is positive,
    // a vertical bar is append to the internal hlist to indicate the overfull box.
    if eqtb.dimen(DimensionVariable::OverfullRule) > 0
        && excess_width > eqtb.dimen(DimensionVariable::Hfuzz)
    {
        match &mut hbox.list {
            HlistOrVlist::Hlist(hlist) => {
                let mut rule_node = RuleNode::new();
                rule_node.width = eqtb.dimen(DimensionVariable::OverfullRule);
                hlist.push(Node::Rule(rule_node));
            }
            _ => panic!("Impossible"),
        }
    }
    logger.print_ln();
    logger.print_nl_str("Overfull \\hbox (");
    logger.print_scaled(excess_width);
    logger.print_str("pt too wide");
    finish_issuing_diagnostic_message_for_hbox(hbox, eqtb, logger);
}

/// See 667.
fn report_tight_hbox(hbox: &ListNode, badness: i32, eqtb: &Eqtb, logger: &mut Logger) {
    logger.print_ln();
    logger.print_nl_str("Tight \\hbox (badness ");
    logger.print_int(badness);
    finish_issuing_diagnostic_message_for_hbox(hbox, eqtb, logger);
}

/// Uses the given vlist and creates an vbox with the given target size.
/// See 668.
pub fn vpack(vlist: Vec<Node>, spec: TargetSpec, eqtb: &mut Eqtb, logger: &mut Logger) -> ListNode {
    vpackage(vlist, spec, MAX_DIMEN, eqtb, logger)
}

/// Uses the given vlist and creates an vbox with the given target size and given maximal depth.
/// See 668.
pub fn vpackage(
    vlist: Vec<Node>,
    spec: TargetSpec,
    l: Dimension,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> ListNode {
    let measurement = measure_vlist(&vlist);
    pack_vbox(vlist, spec, l, measurement, eqtb, logger)
}

/// See 669., 670., and 671.
pub fn measure_vlist(vlist: &Vec<Node>) -> Measurement {
    let mut max_width = 0;
    let mut last_depth = 0;
    let mut total_height = 0;
    let mut stretch_collector = GlueCollector::new();
    let mut shrink_collector = GlueCollector::new();
    for node in vlist {
        match node {
            &Node::List(ListNode {
                width,
                height,
                depth,
                shift_amount,
                ..
            }) => {
                total_height += last_depth + height;
                last_depth = depth;
                if width + shift_amount > max_width {
                    max_width = width + shift_amount;
                }
            }
            &Node::Rule(RuleNode {
                width,
                height,
                depth,
            })
            | &Node::Unset(UnsetNode {
                width,
                height,
                depth,
                ..
            }) => {
                total_height += last_depth + height;
                last_depth = depth;
                if width > max_width {
                    max_width = width;
                }
            }
            Node::Whatsit(_) => {
                // Whatsits have no size.
            }
            Node::Glue(GlueNode {
                glue_spec: ref spec,
                subtype,
            }) => {
                // From 671.
                total_height += last_depth;
                last_depth = 0;
                total_height += spec.width;
                stretch_collector.add_dimen(spec.stretch);
                shrink_collector.add_dimen(spec.shrink);
                if let GlueType::Leaders { leader_node, .. } = subtype {
                    match **leader_node {
                        // NOTE ListNodes in leaders always have zero shift amount.
                        Node::List(ListNode { width, .. }) | Node::Rule(RuleNode { width, .. }) => {
                            if width > max_width {
                                max_width = width;
                            }
                        }
                        _ => panic!("Should not happen"),
                    }
                }
            }
            Node::Kern(KernNode { width, .. }) => {
                total_height += last_depth + width;
                last_depth = 0;
            }
            Node::Ins(_) | Node::Mark(_) | Node::Penalty(_) => {}
            Node::Char(_)
            | Node::Ligature(_)
            | Node::Disc(_)
            | Node::Math(_)
            | Node::Adjust(_)
            | Node::Noad(_)
            | Node::Style(_)
            | Node::Choice(_) => {
                panic!("Should not appear here");
            }
        }
    }
    Measurement {
        width: max_width,
        height: total_height,
        depth: last_depth,
        stretch: stretch_collector.evaluate(),
        shrink: shrink_collector.evaluate(),
    }
}

/// Create a vbox corresponding to the target size and the measured internal size.
/// Attempt to stretch or shrink the internal list as needed.
/// See 672.
fn pack_vbox(
    vlist: Vec<Node>,
    spec: TargetSpec,
    l: Dimension,
    measurement: Measurement,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> ListNode {
    let (adjusted_height, adjusted_depth);
    if measurement.depth > l {
        adjusted_height = measurement.height + measurement.depth - l;
        adjusted_depth = l;
    } else {
        adjusted_height = measurement.height;
        adjusted_depth = measurement.depth;
    }
    let box_height = match spec {
        TargetSpec::Natural => adjusted_height,
        TargetSpec::Additional(h) => adjusted_height + h,
        TargetSpec::Exactly(h) => h,
    };
    let mut vbox = ListNode {
        width: measurement.width,
        height: box_height,
        depth: adjusted_depth,
        shift_amount: 0,
        list: HlistOrVlist::Vlist(vlist),
        glue_set: 0.0,
        glue_sign: GlueSign::Normal,
        glue_order: DimensionOrder::Normal,
    };
    let x = box_height - adjusted_height;
    eqtb.last_badness = if x > 0 {
        determine_vertical_glue_stretch_setting(&mut vbox, measurement.stretch, x, eqtb, logger)
    } else if x < 0 {
        determine_vertical_glue_shrink_setting(&mut vbox, measurement.shrink, -x, eqtb, logger)
    } else {
        0
    };
    vbox
}

/// See 673.
fn determine_vertical_glue_stretch_setting(
    vbox: &mut ListNode,
    stretch: HigherOrderDimension,
    x: Dimension,
    eqtb: &Eqtb,
    logger: &mut Logger,
) -> i32 {
    if stretch.value != 0 {
        vbox.glue_set = x as f64 / stretch.value as f64;
        vbox.glue_sign = GlueSign::Stretching;
        vbox.glue_order = stretch.order
    }
    let badness;
    match &vbox.list {
        HlistOrVlist::Vlist(vlist) => {
            if stretch.order == DimensionOrder::Normal && !vlist.is_empty() {
                badness = calculate_badness(x, stretch.value);
                if badness > eqtb.integer(IntegerVariable::Vbadness) {
                    report_underful_vbox(vbox, badness, eqtb, logger);
                }
            } else {
                badness = 0;
            }
        }
        _ => panic!("Impossible"),
    }
    badness
}

/// See 674.
fn report_underful_vbox(vbox: &ListNode, badness: i32, eqtb: &Eqtb, logger: &mut Logger) {
    logger.print_ln();
    if badness > 100 {
        logger.print_nl_str("Underfull");
    } else {
        logger.print_nl_str("Loose");
    }
    logger.print_str(" \\vbox (badness ");
    logger.print_int(badness);
    finish_issuing_diagnostic_message_for_vbox(vbox, eqtb, logger);
}

/// See 675.
fn finish_issuing_diagnostic_message_for_vbox(vbox: &ListNode, eqtb: &Eqtb, logger: &mut Logger) {
    if eqtb.output_active {
        logger.print_str(") has occurred while \\output is active");
    } else {
        if logger.pack_begin_line != 0 {
            logger.print_str(") in alignment at lines ");
            logger.print_int(logger.pack_begin_line.abs());
            logger.print_str("--");
        } else {
            logger.print_str(") detected at line ");
        }
        logger.print_int(eqtb.line_number() as i32);
        logger.print_ln();
    }
    logger.begin_diagnostic(eqtb.tracing_online());
    show_box(vbox, eqtb, logger);
    logger.end_diagnostic(true);
}

/// See 676.
fn determine_vertical_glue_shrink_setting(
    vbox: &mut ListNode,
    shrink: HigherOrderDimension,
    x: Dimension,
    eqtb: &Eqtb,
    logger: &mut Logger,
) -> i32 {
    if shrink.value != 0 {
        vbox.glue_set = x as f64 / shrink.value as f64;
        vbox.glue_sign = GlueSign::Shrinking;
        vbox.glue_order = shrink.order;
    }
    let badness;
    match &vbox.list {
        HlistOrVlist::Vlist(vlist) => {
            if shrink.order == DimensionOrder::Normal && !vlist.is_empty() {
                if shrink.value < x {
                    badness = 1_000_000;
                    vbox.glue_set = 1.0;
                    let excess_height = x - shrink.value;
                    if excess_height > eqtb.dimen(DimensionVariable::Vfuzz)
                        || eqtb.integer(IntegerVariable::Vbadness) < 100
                    {
                        report_overfull_vbox(vbox, excess_height, eqtb, logger);
                    }
                } else {
                    badness = calculate_badness(x, shrink.value);
                    if badness > eqtb.integer(IntegerVariable::Vbadness) {
                        report_tight_vbox(vbox, badness, eqtb, logger);
                    }
                }
            } else {
                badness = 0;
            }
        }
        _ => panic!("Impossible"),
    }
    badness
}

/// See 677.
fn report_overfull_vbox(
    vbox: &ListNode,
    excess_height: Dimension,
    eqtb: &Eqtb,
    logger: &mut Logger,
) {
    logger.print_ln();
    logger.print_nl_str("Overfull \\vbox (");
    logger.print_scaled(excess_height);
    logger.print_str("pt too high");
    finish_issuing_diagnostic_message_for_vbox(vbox, eqtb, logger);
}

/// See 678.
fn report_tight_vbox(vbox: &ListNode, badness: i32, eqtb: &Eqtb, logger: &mut Logger) {
    logger.print_ln();
    logger.print_nl_str("Tight \\vbox (badness ");
    logger.print_int(badness);
    finish_issuing_diagnostic_message_for_vbox(vbox, eqtb, logger);
}

/// Collects glue dimensions.
/// Determines the dominating order and value.
#[derive(Clone, Copy, Debug)]
pub struct GlueCollector {
    pub normal: Dimension,
    pub fil: Scaled,
    pub fill: Scaled,
    pub filll: Scaled,
}

impl GlueCollector {
    pub const fn new() -> Self {
        Self {
            normal: 0,
            fil: 0,
            fill: 0,
            filll: 0,
        }
    }

    pub fn add_dimen(&mut self, val: HigherOrderDimension) {
        match val.order {
            DimensionOrder::Normal => {
                self.normal += val.value;
            }
            DimensionOrder::Fil => {
                self.fil += val.value;
            }
            DimensionOrder::Fill => {
                self.fill += val.value;
            }
            DimensionOrder::Filll => {
                self.filll += val.value;
            }
        }
    }

    pub fn evaluate(&self) -> HigherOrderDimension {
        if self.filll != 0 {
            HigherOrderDimension {
                order: DimensionOrder::Filll,
                value: self.filll,
            }
        } else if self.fill != 0 {
            HigherOrderDimension {
                order: DimensionOrder::Fill,
                value: self.fill,
            }
        } else if self.fil != 0 {
            HigherOrderDimension {
                order: DimensionOrder::Fil,
                value: self.fil,
            }
        } else {
            HigherOrderDimension {
                order: DimensionOrder::Normal,
                value: self.normal,
            }
        }
    }
}

impl Add for GlueCollector {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        Self {
            normal: self.normal + rhs.normal,
            fil: self.fil + rhs.fil,
            fill: self.fill + rhs.fill,
            filll: self.filll + rhs.filll,
        }
    }
}

impl AddAssign for GlueCollector {
    fn add_assign(&mut self, rhs: Self) {
        self.normal += rhs.normal;
        self.fil += rhs.fil;
        self.fill += rhs.fill;
        self.filll += rhs.filll;
    }
}

impl Neg for GlueCollector {
    type Output = Self;
    fn neg(self) -> Self {
        Self {
            normal: -self.normal,
            fil: -self.fil,
            fill: -self.fill,
            filll: -self.filll,
        }
    }
}

impl Sub for GlueCollector {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        Self {
            normal: self.normal - rhs.normal,
            fil: self.fil - rhs.fil,
            fill: self.fill - rhs.fill,
            filll: self.filll - rhs.filll,
        }
    }
}

impl SubAssign for GlueCollector {
    fn sub_assign(&mut self, rhs: Self) {
        self.normal -= rhs.normal;
        self.fil -= rhs.fil;
        self.fill -= rhs.fill;
        self.filll -= rhs.filll;
    }
}

/// Uses the given hlist and creates an unset box from it.
/// This is a variant of `hpack`.
/// See 649. and 657.
pub fn pack_halign_cell(hlist: Vec<Node>) -> UnsetNode {
    let measurement = measure_hlist(&hlist);
    UnsetNode {
        width: measurement.width,
        height: measurement.height,
        depth: measurement.depth,
        list: hlist,
        stretch: measurement.stretch,
        shrink: measurement.shrink,
        span_count: 0,
    }
}

/// Uses the given vlist and creates an unset box from it.
/// This is a variant of `vpackage`.
/// See 668. and 672.
pub fn pack_valign_cell(vlist: Vec<Node>) -> UnsetNode {
    let measurement = measure_vlist(&vlist);
    let (adjusted_height, adjusted_depth);
    if measurement.depth > 0 {
        adjusted_height = measurement.height + measurement.depth;
        adjusted_depth = 0;
    } else {
        adjusted_height = measurement.height;
        adjusted_depth = measurement.depth;
    }
    UnsetNode {
        width: measurement.width,
        height: adjusted_height,
        depth: adjusted_depth,
        list: vlist,
        stretch: measurement.stretch,
        shrink: measurement.shrink,
        span_count: 0,
    }
}
