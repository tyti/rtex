use crate::dimension::Dimension;
use crate::eqtb::{BoxVariable, DimensionVariable, Eqtb, RegisterIndex, SkipVariable};
use crate::input::Scanner;
use crate::line_breaking::{AWFUL_BAD, EJECT_PENALTY, INF_PENALTY};
use crate::logger::Logger;
use crate::nodes::{DimensionOrder, GlueNode, HlistOrVlist, ListNode, Node, RuleNode};
use crate::packaging::{vpack, vpackage, GlueCollector, TargetSpec};
use crate::print::Printer;
use crate::scaled::{calculate_badness, Scaled, INF_BAD};

/// See 974.
pub const DEPLORABLE: i32 = 100_000;

/// See 977. and 978.
pub fn vsplit(
    n: RegisterIndex,
    h: Scaled,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Option<ListNode> {
    let v = eqtb.boxes.set(BoxVariable(n), None);
    if eqtb.marks.split_first.is_some() {
        eqtb.marks.split_first = None;
        eqtb.marks.split_bot = None;
    }
    // From 978.
    let Some(list_node) = v else {
        return None;
    };
    let mut vlist = match list_node.list {
        HlistOrVlist::Hlist(_) => {
            eqtb.boxes.set(BoxVariable(n), Some(list_node));
            logger.print_err("");
            logger.print_esc_str(b"vsplit");
            logger.print_str(" needs a ");
            logger.print_esc_str(b"vbox");
            let help = &[
                "The box you are trying to split is an \\hbox.",
                "I can't split such a box, so I'll leave it alone.",
            ];
            logger.error(help, scanner, eqtb);
            return None;
        }
        HlistOrVlist::Vlist(vlist) => vlist,
    };
    let (break_pos, _) = vert_break(
        &mut vlist,
        h,
        eqtb.dimen(DimensionVariable::SplitMaxDepth),
        scanner,
        eqtb,
        logger,
    );
    let remainder_list = vlist.split_off(break_pos);
    if remainder_list.is_empty() {
        eqtb.boxes.set(BoxVariable(n), None);
    } else {
        let pruned_list = prune_page_top(remainder_list, eqtb);
        let repacked_box = vpack(pruned_list, TargetSpec::Natural, eqtb, logger);
        eqtb.boxes.set(BoxVariable(n), Some(repacked_box));
    }

    let split_off_list = vlist;

    look_at_all_marks_in_split_off_list(&split_off_list, eqtb);

    Some(vpackage(
        split_off_list,
        TargetSpec::Exactly(h),
        eqtb.dimen(DimensionVariable::SplitMaxDepth),
        eqtb,
        logger,
    ))
}

/// See 979.
fn look_at_all_marks_in_split_off_list(split_off_list: &Vec<Node>, eqtb: &mut Eqtb) {
    for node in split_off_list {
        if let Node::Mark(mark_node) = node {
            if eqtb.marks.split_first.is_none() {
                eqtb.marks.split_first = Some(mark_node.mark.clone());
                eqtb.marks.split_bot = Some(mark_node.mark.clone());
            } else {
                eqtb.marks.split_bot = Some(mark_node.mark.clone());
            }
        }
    }
}

/// Removes all glue, kern or penalty nodes before the first box or rule node of the given vlist.
/// Also inserts glue before the first box or rule such that its baseline is at the distance
/// \splittopskip from the top if possible.
/// See 968. and 969.
pub fn prune_page_top(vlist: Vec<Node>, eqtb: &Eqtb) -> Vec<Node> {
    let mut pruned_vlist = Vec::new();
    let mut found_box_or_rule = false;
    for node in vlist {
        match node {
            Node::List(ListNode { height, .. }) | Node::Rule(RuleNode { height, .. }) => {
                if !found_box_or_rule {
                    let split_top_skip = eqtb.skips.get(SkipVariable::SplitTopSkip);
                    let d = if split_top_skip.width > height {
                        split_top_skip.width - height
                    } else {
                        0
                    };
                    let glue_node =
                        GlueNode::new_param_with_width(SkipVariable::SplitTopSkip, d, eqtb);
                    pruned_vlist.push(Node::Glue(glue_node));

                    found_box_or_rule = true;
                }
                pruned_vlist.push(node);
            }
            Node::Whatsit(_) | Node::Mark(_) | Node::Ins(_) => {
                pruned_vlist.push(node);
            }
            Node::Glue(_) | Node::Kern(_) | Node::Penalty(_) => {
                if found_box_or_rule {
                    pruned_vlist.push(node);
                }
            }
            _ => panic!("pruning"),
        }
    }
    pruned_vlist
}

/// See 823.
#[derive(Clone, Copy, Debug)]
struct VerticalDimensions {
    height: Dimension,
    // Shrinkage must be finite.
    shrink: Dimension,
    stretch: GlueCollector,
    depth: Dimension,
}

impl VerticalDimensions {
    const fn new() -> Self {
        Self {
            height: 0,
            // Shrinkage must be finite.
            shrink: 0,
            stretch: GlueCollector::new(),
            depth: 0,
        }
    }
}

/// Finds the best place to split off the top of a vertical list to achieve a given height and
/// maximal depth. Returns the break point index and the corresponing height plus depth of top of
/// the list.
/// See 970. and 972.
pub fn vert_break(
    vlist: &mut Vec<Node>,
    target_height: Dimension,
    max_depth: Dimension,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> (usize, Dimension) {
    let mut dimens = VerticalDimensions::new();

    let mut least_cost = AWFUL_BAD;
    let mut best_pos = 0;
    let mut best_height_plus_depth = 0;
    for pos in 0..vlist.len() {
        let pi = is_this_break_point(vlist, pos);
        if let Some(pi) = pi {
            let b = compute_badness_using_awful_bad_if_box_too_full(target_height, &dimens);
            update_best_pos(
                pos,
                &mut best_pos,
                &mut best_height_plus_depth,
                &dimens,
                pi,
                &mut least_cost,
                b,
            );
            // Break if the list got too long or if we have a forced break.
            if (pi < INF_PENALTY && b == AWFUL_BAD) || pi <= EJECT_PENALTY {
                return (best_pos, best_height_plus_depth);
            }
        }
        update_height_and_depth(&mut vlist[pos], &mut dimens, scanner, eqtb, logger);

        // If the current depth exceeds `max_depth`, move the excessive depth to the height.
        if dimens.depth > max_depth {
            dimens.height += dimens.depth - max_depth;
            dimens.depth = max_depth;
        }
    }
    // Check if the end of the list is the best break point.
    let b = compute_badness_using_awful_bad_if_box_too_full(target_height, &dimens);
    update_best_pos(
        vlist.len(),
        &mut best_pos,
        &mut best_height_plus_depth,
        &dimens,
        EJECT_PENALTY,
        &mut least_cost,
        b,
    );
    (best_pos, best_height_plus_depth)
}

/// See 973. and 976.
fn update_height_and_depth(
    node: &mut Node,
    dimens: &mut VerticalDimensions,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    match node {
        Node::List(ListNode { height, depth, .. }) | Node::Rule(RuleNode { height, depth, .. }) => {
            dimens.height += dimens.depth + *height;
            dimens.depth = *depth;
        }
        Node::Glue(glue_node) => {
            if glue_node.glue_spec.shrink.is_infinite() {
                logger.print_err("Infinite glue shrinkage found in box being split");
                let help = &[
                    "The box you are \\vsplitting contains some infinitely",
                    "shrinkable glue, e.g., `\\vss' or `\\vskip 0pt minus 1fil'.",
                    "Such glue doesn't belong there; but you can safely proceed,",
                    "since the offensive shrinkability has been made finite.",
                ];
                logger.error(help, scanner, eqtb);

                glue_node.glue_spec = glue_node.glue_spec.finite_shrink_copy();
            }
            dimens.height += dimens.depth + glue_node.glue_spec.width;
            dimens.stretch.add_dimen(glue_node.glue_spec.stretch);
            dimens.shrink += glue_node.glue_spec.shrink.value;
            dimens.depth = 0
        }
        Node::Kern(kern_node) => {
            dimens.height += dimens.depth + kern_node.width;
            dimens.depth = 0
        }
        Node::Whatsit(_) | Node::Penalty(_) | Node::Mark(_) | Node::Ins(_) => {}
        _ => panic!("vertbreak"),
    }
}

/// Returns Some(penalty) if it is a break point, None otherwise.
/// See 973.
fn is_this_break_point(vlist: &Vec<Node>, pos: usize) -> Option<i32> {
    match &vlist[pos] {
        Node::Glue(_) => {
            if pos > 0 && vlist[pos - 1].precedes_vertical_break() {
                Some(0)
            } else {
                None
            }
        }
        Node::Kern(_) => {
            if let Some(Node::Glue(_)) = vlist.get(pos + 1) {
                Some(0)
            } else {
                None
            }
        }
        Node::Penalty(penalty_node) => Some(penalty_node.penalty),
        Node::List(_) | Node::Rule(_) | Node::Whatsit(_) | Node::Mark(_) | Node::Ins(_) => None,
        _ => panic!("vertbreak"),
    }
}

/// See 974.
fn update_best_pos(
    pos: usize,
    best_pos: &mut usize,
    best_height_plus_depth: &mut Dimension,
    dimens: &VerticalDimensions,
    pi: i32,
    least_cost: &mut i32,
    mut b: i32,
) {
    if pi < INF_PENALTY {
        if b < AWFUL_BAD {
            if pi <= EJECT_PENALTY {
                b = pi;
            } else if b < INF_BAD {
                b += pi
            } else {
                b = DEPLORABLE;
            }
        }
        if b <= *least_cost {
            *best_pos = pos;
            *least_cost = b;
            *best_height_plus_depth = dimens.height + dimens.depth;
        }
    }
}

/// See 975.
fn compute_badness_using_awful_bad_if_box_too_full(
    target_height: Dimension,
    dimens: &VerticalDimensions,
) -> i32 {
    if dimens.height < target_height {
        // We need to stretch.
        let stretch = dimens.stretch.evaluate();
        if stretch.order == DimensionOrder::Normal {
            calculate_badness(target_height - dimens.height, stretch.value)
        } else {
            0
        }
    } else {
        // We need to shrink.
        if dimens.height - target_height > dimens.shrink {
            AWFUL_BAD
        } else {
            calculate_badness(dimens.height - target_height, dimens.shrink)
        }
    }
}
