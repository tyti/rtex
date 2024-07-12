use crate::box_building::{get_hskip, get_vskip, normal_paragraph, BoxContext};
use crate::command::{MakeBox, UnexpandableCommand};
use crate::dimension::scan_normal_dimen;
use crate::eqtb::save_stack::GroupType;
use crate::eqtb::{BoxVariable, Eqtb, RegisterIndex, TokenListVariable};
use crate::horizontal_mode::HorizontalMode;
use crate::input::token_source::TokenSourceType;
use crate::input::Scanner;
use crate::logger::Logger;
use crate::main_control::you_cant;
use crate::nodes::noads::{Noad, NoadField, NormalNoad};
use crate::nodes::{GlueType, LeaderKind, ListNode, Node, RuleNode};
use crate::output::Output;
use crate::packaging::scan_spec;
use crate::page_breaking::PageBuilder;
use crate::semantic_nest::{Mode, RichMode, SemanticState};
use crate::vertical_mode::VerticalMode;
use crate::vsplit::vsplit;

/// See 1084.
pub fn scan_box(
    box_context: BoxContext,
    page_builder: &mut PageBuilder,
    output: &mut Output,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let (unexpandable_command, token) =
        scanner.get_next_non_blank_non_relax_non_call_token(eqtb, logger);
    let UnexpandableCommand::MakeBox(make_box) = unexpandable_command else {
        logger.print_err("A <box> was supposed to be here");
        let help = &[
            "I was expecting to see \\hbox or \\vbox or \\copy or \\box or",
            "something like that. So you might find something missing in",
            "your output. But keep trying; you can fix this later.",
        ];
        scanner.back_error(token, help, eqtb, logger);
        return;
    };
    begin_box(
        make_box,
        box_context,
        page_builder,
        output,
        nest,
        scanner,
        eqtb,
        logger,
    );
}

/// See 1084.
pub fn scan_leader_box(
    leader_kind: LeaderKind,
    page_builder: &mut PageBuilder,
    output: &mut Output,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let (unexpandable_command, token) =
        scanner.get_next_non_blank_non_relax_non_call_token(eqtb, logger);
    match unexpandable_command {
        UnexpandableCommand::MakeBox(make_box) => {
            begin_box(
                make_box,
                BoxContext::Leaders(leader_kind),
                page_builder,
                output,
                nest,
                scanner,
                eqtb,
                logger,
            );
        }
        UnexpandableCommand::Vrule => {
            let rule_node = RuleNode::scan_vrule_spec(scanner, eqtb, logger);
            append_new_leader_node(
                Node::Rule(rule_node),
                leader_kind,
                nest,
                scanner,
                eqtb,
                logger,
            );
        }
        UnexpandableCommand::Hrule => {
            let rule_node = RuleNode::scan_hrule_spec(scanner, eqtb, logger);
            append_new_leader_node(
                Node::Rule(rule_node),
                leader_kind,
                nest,
                scanner,
                eqtb,
                logger,
            );
        }
        _ => {
            logger.print_err("A <box> was supposed to be here");
            let help = &[
                "I was expecting to see \\hbox or \\vbox or \\copy or \\box or",
                "something like that. So you might find something missing in",
                "your output. But keep trying; you can fix this later.",
            ];
            scanner.back_error(token, help, eqtb, logger);
        }
    }
}

/// See 1079.
pub fn begin_box(
    make_box: MakeBox,
    box_context: BoxContext,
    page_builder: &mut PageBuilder,
    output: &mut Output,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let cur_box = match make_box {
        MakeBox::Box => {
            let register = scanner.scan_register_index(eqtb, logger);
            eqtb.boxes.set(BoxVariable(register), None)
        }
        MakeBox::Copy => {
            let register = scanner.scan_register_index(eqtb, logger);
            eqtb.boks(register).clone()
        }
        MakeBox::LastBox => get_last_box(nest, scanner, eqtb, logger),
        MakeBox::VSplit => split_off_part_of_vertical_box(scanner, eqtb, logger),
        MakeBox::HBox => {
            initiate_construction_of_hbox(box_context, nest, scanner, eqtb, logger);
            return;
        }
        MakeBox::VTop => {
            initiate_construction_of_vbox(true, box_context, nest, scanner, eqtb, logger);
            return;
        }
        MakeBox::VBox => {
            initiate_construction_of_vbox(false, box_context, nest, scanner, eqtb, logger);
            return;
        }
    };
    box_end(
        cur_box,
        box_context,
        page_builder,
        output,
        nest,
        scanner,
        eqtb,
        logger,
    );
}

/// See 1080. and 1081.
fn get_last_box(
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Option<ListNode> {
    if eqtb.mode().base() == Mode::DisplayMath {
        you_cant(
            UnexpandableCommand::MakeBox(MakeBox::LastBox),
            scanner,
            eqtb,
            logger,
        );
        let help = &["Sorry; this \\lastbox will be void."];
        logger.error(help, scanner, eqtb);
        None
    } else if eqtb.mode() == Mode::Vertical && nest.cur_list_is_empty() {
        you_cant(
            UnexpandableCommand::MakeBox(MakeBox::LastBox),
            scanner,
            eqtb,
            logger,
        );
        let help = &[
            "Sorry...I usually can't take things from the current page.",
            "This \\lastbox will therefore be void.",
        ];
        logger.error(help, scanner, eqtb);
        None
    } else {
        let last_node = nest.pop_last(eqtb);
        if let Some(last_node) = last_node {
            if let Node::List(mut list_node) = last_node {
                list_node.shift_amount = 0;
                Some(list_node)
            } else {
                nest.tail_push(last_node, eqtb);
                None
            }
        } else {
            None
        }
    }
}

/// See 1082.
fn split_off_part_of_vertical_box(
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Option<ListNode> {
    let n = scanner.scan_register_index(eqtb, logger);
    if !scanner.scan_keyword(b"to", eqtb, logger) {
        logger.print_err("Missing `to' inserted");
        let help = &[
            "I'm working on `\\vsplit<box number> to <dimen>';",
            "will look for the <dimen> next.",
        ];
        logger.error(help, scanner, eqtb);
    }
    let dimen = scan_normal_dimen(scanner, eqtb, logger);
    vsplit(n, dimen, scanner, eqtb, logger)
}

/// Starts constructing an \hbox.
/// See 1083.
fn initiate_construction_of_hbox(
    context: BoxContext,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let spec = scan_spec(scanner, eqtb, logger);
    if let (BoxContext::Shift(shift_amount), Mode::Vertical) = (context, eqtb.mode().base()) {
        eqtb.new_save_level(
            GroupType::AdjustedHbox { shift_amount, spec },
            &scanner.input_stack,
            logger,
        );
        scanner.scan_left_brace(eqtb, logger);
    } else {
        eqtb.new_save_level(
            GroupType::Hbox { context, spec },
            &scanner.input_stack,
            logger,
        );
        scanner.scan_left_brace(eqtb, logger);
    }
    nest.push_nest(
        RichMode::Horizontal(HorizontalMode::new_restricted()),
        &scanner.input_stack,
        eqtb,
        logger,
    );
    if let Some(every_hbox) = eqtb.token_lists.get(TokenListVariable::EveryHbox) {
        scanner.input_stack.begin_token_list(
            every_hbox.clone(),
            TokenSourceType::EveryHboxText,
            eqtb,
            logger,
        );
    }
}

/// Starts constructing a \vbox or \vtop.
/// See 1083.
fn initiate_construction_of_vbox(
    is_vtop: bool,
    context: BoxContext,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let spec = scan_spec(scanner, eqtb, logger);
    // \vtop
    if is_vtop {
        eqtb.new_save_level(
            GroupType::Vtop { context, spec },
            &scanner.input_stack,
            logger,
        );
        scanner.scan_left_brace(eqtb, logger);
    // \vbox
    } else {
        eqtb.new_save_level(
            GroupType::Vbox { context, spec },
            &scanner.input_stack,
            logger,
        );
        scanner.scan_left_brace(eqtb, logger);
    }
    normal_paragraph(eqtb, logger);
    nest.push_nest(
        RichMode::Vertical(VerticalMode::new_internal()),
        &scanner.input_stack,
        eqtb,
        logger,
    );
    if let Some(every_vbox) = eqtb.token_lists.get(TokenListVariable::EveryVbox) {
        scanner.input_stack.begin_token_list(
            every_vbox.clone(),
            TokenSourceType::EveryVboxText,
            eqtb,
            logger,
        );
    }
}

/// See 1075.
pub fn box_end(
    cur_box: Option<ListNode>,
    box_context: BoxContext,
    page_builder: &mut PageBuilder,
    output: &mut Output,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    match box_context {
        BoxContext::Shift(shift_amount) => append_box_cur_box_to_current_list_shifted(
            cur_box,
            shift_amount,
            page_builder,
            output,
            nest,
            scanner,
            eqtb,
            logger,
        ),
        BoxContext::SetBox { box_number, global } => {
            store_cur_box_in_box_register(cur_box, box_number, global, eqtb)
        }
        BoxContext::Leaders(leader_kind) => {
            if let Some(list_node) = cur_box {
                append_new_leader_node(
                    Node::List(list_node),
                    leader_kind,
                    nest,
                    scanner,
                    eqtb,
                    logger,
                );
            }
        }
        BoxContext::Shipout => {
            if let Some(list_node) = cur_box {
                output.ship_out(list_node, scanner, eqtb, logger);
            }
        }
    }
}

/// See 1076.
fn append_box_cur_box_to_current_list_shifted(
    cur_box: Option<ListNode>,
    shift_amount: i32,
    page_builder: &mut PageBuilder,
    output: &mut Output,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    if let Some(mut list_node) = cur_box {
        list_node.shift_amount = shift_amount;
        match nest.mode_mut() {
            RichMode::Vertical(vmode) => {
                vmode.append_to_vlist(list_node, eqtb);
                if let Mode::Vertical = eqtb.mode() {
                    page_builder.build_page(output, nest, scanner, eqtb, logger);
                }
            }
            RichMode::Horizontal(hmode) => {
                hmode.set_space_factor(1000, eqtb);
                nest.tail_push(Node::List(list_node), eqtb);
            }
            RichMode::Math(_) => {
                let mut noad = NormalNoad::new();
                noad.nucleus = Some(Box::new(NoadField::SubBox { list_node }));
                nest.tail_push(Node::Noad(Noad::Normal(noad)), eqtb);
            }
        }
    }
}

/// 1077.
fn store_cur_box_in_box_register(
    cur_box: Option<ListNode>,
    box_number: RegisterIndex,
    global: bool,
    eqtb: &mut Eqtb,
) {
    eqtb.box_define(box_number, cur_box, global);
}

/// See 1078.
fn append_new_leader_node(
    leader_node: Node,
    leader_kind: LeaderKind,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let (unexpandable_command, token) =
        scanner.get_next_non_blank_non_relax_non_call_token(eqtb, logger);
    match unexpandable_command {
        UnexpandableCommand::Hskip(hskip) if eqtb.mode().base() != Mode::Vertical => {
            let mut glue_node = get_hskip(hskip, scanner, eqtb, logger);
            glue_node.subtype = GlueType::Leaders {
                leader_kind,
                leader_node: Box::new(leader_node),
            };
            nest.tail_push(Node::Glue(glue_node), eqtb);
        }
        UnexpandableCommand::Vskip(vskip) if eqtb.mode().base() == Mode::Vertical => {
            let mut glue_node = get_vskip(vskip, scanner, eqtb, logger);
            glue_node.subtype = GlueType::Leaders {
                leader_kind,
                leader_node: Box::new(leader_node),
            };
            nest.tail_push(Node::Glue(glue_node), eqtb);
        }
        _ => {
            logger.print_err("Leaders not followed by proper glue");
            let help = &[
                "You should say `\\leaders <box or rule><hskip or vskip>'.",
                "I found the <box or rule>, but there's no suitable",
                "<hskip or vskip>, so I'm ignoring these leaders.",
            ];
            scanner.back_error(token, help, eqtb, logger);
        }
    }
}
