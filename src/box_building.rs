use crate::alignment::AlignState;
use crate::command::{Hskip, RemoveItem, UnexpandableCommand, Vskip};
use crate::dimension::{scan_mu_dimen, scan_normal_dimen, Dimension};
use crate::eqtb::save_stack::{DiscGroupType, GroupType, SubformulaType};
use crate::eqtb::{
    BoxVariable, ControlSequence, DimensionVariable, Eqtb, FontIndex, IntegerVariable,
    LastNodeInfo, ParShapeVariable, ParagraphShape, RegisterIndex, SkipVariable, TokenListVariable,
};
use crate::fonts::new_character;
use crate::glue::scan_glue;
use crate::horizontal_mode::{HorizontalMode, HorizontalModeType, LanguageData};
use crate::hyphenation::Hyphenator;
use crate::input::token_source::TokenSourceType;
use crate::input::Scanner;
use crate::integer::{Integer, IntegerExt};
use crate::line_breaking::line_break;
use crate::logger::Logger;
use crate::main_control::you_cant;
use crate::math::build_choices;
use crate::math_mode::MathMode;
use crate::mode_independent::do_assignments;
use crate::nodes::noads::{
    AccentNoad, Noad, NoadField, NoadType, NormalNoad, OverNoad, RadicalNoad, UnderNoad,
    VcenterNoad,
};
use crate::nodes::{
    show_box_content, AdjustNode, CharNode, DiscNode, GlueNode, GlueSpec, GlueType, HlistOrVlist,
    InsNode, KernNode, KernSubtype, LeaderKind, LigatureNode, ListNode, MarkNode, Node,
    PenaltyNode, RuleNode,
};
use crate::output::Output;
use crate::packaging::{
    hpack, measure_vlist, split_adjust_material_off, vpack, vpackage, TargetSpec,
};
use crate::page_breaking::PageBuilder;
use crate::print::Printer;
use crate::scan_boxes::box_end;
use crate::semantic_nest::{Mode, RichMode, SemanticState};
use crate::token::Token;
use crate::vertical_mode::VerticalMode;
use crate::{norm_min, round};

use std::rc::Rc;

/// See 1071.
#[derive(Debug, Clone, Copy)]
pub enum BoxContext {
    Shift(Dimension),
    SetBox {
        box_number: RegisterIndex,
        global: bool,
    },
    Shipout,
    Leaders(LeaderKind),
}

/// See 1068.
pub fn handle_right_brace(
    token: Token,
    align_state: &mut AlignState,
    hyphenator: &mut Hyphenator,
    page_builder: &mut PageBuilder,
    output: &mut Output,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    match eqtb.cur_group.typ {
        GroupType::Simple => eqtb.unsave(scanner, logger),
        GroupType::BottomLevel => {
            logger.print_err("Too many }'s");
            let help = &[
                "You've closed more groups than you opened.",
                "Such booboos are generally harmless, so keep going.",
            ];
            logger.error(help, scanner, eqtb);
        }
        GroupType::SemiSimple | GroupType::MathShift { .. } | GroupType::MathLeft => {
            extra_right_brace(scanner, eqtb, logger)
        }
        // Now the cases that cause a delayed action
        // From 1085.
        GroupType::Hbox { context, spec } => package(
            false,
            context,
            spec,
            page_builder,
            output,
            nest,
            scanner,
            eqtb,
            logger,
        ),
        GroupType::AdjustedHbox { shift_amount, spec } => {
            eqtb.unsave(scanner, logger);
            let old_level = nest.pop_nest(eqtb);
            let RichMode::Horizontal(hmode) = old_level.mode else {
                panic!("The mode must have been horizontal");
            };
            let mut hlist = hmode.list;
            let adjustments = split_adjust_material_off(&mut hlist);
            let mut list_node = hpack(hlist, spec, eqtb, logger);
            list_node.shift_amount = shift_amount;
            let RichMode::Vertical(vmode) = nest.mode_mut() else {
                panic!("We expected to be in vertical mode here");
            };
            vmode.append_to_vlist(list_node, eqtb);
            nest.tail_append(adjustments, eqtb);
            if let Mode::Horizontal | Mode::Vertical | Mode::DisplayMath = eqtb.mode() {
                page_builder.build_page(output, nest, scanner, eqtb, logger);
            }
        }
        GroupType::Vbox { context, spec } => {
            end_graf(hyphenator, nest, scanner, eqtb, logger);
            package(
                false,
                context,
                spec,
                page_builder,
                output,
                nest,
                scanner,
                eqtb,
                logger,
            );
        }
        GroupType::Vtop { context, spec } => {
            end_graf(hyphenator, nest, scanner, eqtb, logger);
            package(
                true,
                context,
                spec,
                page_builder,
                output,
                nest,
                scanner,
                eqtb,
                logger,
            );
        }
        // From 1100.
        GroupType::Insert { number } => {
            end_graf(hyphenator, nest, scanner, eqtb, logger);
            let split_top_skip = eqtb.skips.get(SkipVariable::SplitTopSkip).clone();
            let split_max_depth = eqtb.dimen(DimensionVariable::SplitMaxDepth);
            let floating_penalty = eqtb.integer(IntegerVariable::FloatingPenalty);
            eqtb.unsave(scanner, logger);
            let old_level = nest.pop_nest(eqtb);
            let RichMode::Vertical(vmode) = old_level.mode else {
                panic!("The mode must have been vertical");
            };
            let vlist = vmode.list;
            let measurement = measure_vlist(&vlist);
            // If this is a proper insertion
            if number != 255 {
                let ins_node = InsNode {
                    number,
                    insertion: vlist,
                    size: measurement.height + measurement.depth,
                    split_max_depth,
                    split_top_skip,
                    float_cost: floating_penalty,
                };
                nest.tail_push(Node::Ins(ins_node), eqtb);
            // If this is a \vadjust
            } else {
                let adjust_node = AdjustNode { adjustment: vlist };
                nest.tail_push(Node::Adjust(adjust_node), eqtb);
            }
            if nest.depth() == 0 {
                page_builder.build_page(output, nest, scanner, eqtb, logger);
            }
        }
        GroupType::Output => page_builder.resume_page_builder_after_output_routine_had_come_to_end(
            hyphenator, output, nest, scanner, eqtb, logger,
        ),
        // From 1118.
        GroupType::Disc { typ } => build_discretionary(typ, nest, scanner, eqtb, logger),
        // From 1132.
        GroupType::AlignEntry => {
            scanner.back_input(token, eqtb, logger);
            logger.print_err("Missing ");
            logger.print_esc_str(b"cr");
            logger.print_str(" inserted");
            let help = &["I'm guessing that you meant to end an alignment here."];
            let frozen_cr = Token::CSToken {
                cs: ControlSequence::FrozenCr,
            };
            scanner.ins_error(frozen_cr, help, eqtb, logger);
        }
        GroupType::Align { .. } => panic!("This should not be possible"),
        // From 1133.
        GroupType::NoAlign => {
            end_graf(hyphenator, nest, scanner, eqtb, logger);
            eqtb.unsave(scanner, logger);
            align_state.align_peek(
                hyphenator,
                page_builder,
                output,
                nest,
                scanner,
                eqtb,
                logger,
            );
        }
        // From 1168.
        GroupType::Vcenter { spec } => {
            end_graf(hyphenator, nest, scanner, eqtb, logger);
            eqtb.unsave(scanner, logger);
            let old_level = nest.pop_nest(eqtb);
            let RichMode::Vertical(vmode) = old_level.mode else {
                panic!("The mode must have been vertical");
            };
            let vlist = vmode.list;
            let list_node = vpack(vlist, spec, eqtb, logger);
            let vcenter_noad = VcenterNoad {
                nucleus: list_node,
                subscript: None,
                superscript: None,
            };
            nest.tail_push(Node::Noad(Noad::Vcenter(vcenter_noad)), eqtb);
        }
        // From 1173.
        GroupType::MathChoice { t } => build_choices(t, nest, scanner, eqtb, logger),
        // From 1186.
        GroupType::Math { subformula_type } => {
            eqtb.unsave(scanner, logger);
            let old_level = nest.pop_nest(eqtb);
            let RichMode::Math(mmode) = old_level.mode else {
                panic!("We must be in math mode here");
            };
            let mut mlist = mmode.fin_mlist(None);
            let noad_field = if mlist.len() == 1 {
                let node = mlist.pop().unwrap();
                match node {
                    Node::Noad(Noad::Normal(NormalNoad {
                        noad_type: NoadType::Ord,
                        subscript: None,
                        superscript: None,
                        nucleus,
                    })) => {
                        // This Ord noad only contains a nucleus. So we effectively replace the
                        // entire noad with just the nucleus.
                        nucleus
                    }
                    Node::Noad(Noad::Accent(accent_noad))
                        if subformula_type == SubformulaType::Nucleus =>
                    {
                        if let Some(
                            last_noad @ Noad::Normal(NormalNoad {
                                noad_type: NoadType::Ord,
                                ..
                            }),
                        ) = nest.last_noad_mut()
                        {
                            // This Accent is the nucleus of a fresh Ord noad. So we effectively
                            // replace the Ord noad with the Accent noad.
                            *last_noad = Noad::Accent(accent_noad);
                            return;
                        } else {
                            Some(Box::new(NoadField::SubMlist {
                                list: vec![Node::Noad(Noad::Accent(accent_noad))],
                            }))
                        }
                    }
                    _ => Some(Box::new(NoadField::SubMlist { list: vec![node] })),
                }
            } else {
                Some(Box::new(NoadField::SubMlist { list: mlist }))
            };
            let Some(last_noad) = nest.last_noad_mut() else {
                panic!("The last node must be a Noad here");
            };
            match subformula_type {
                SubformulaType::Nucleus => match last_noad {
                    Noad::Normal(NormalNoad { nucleus, .. })
                    | Noad::Radical(RadicalNoad { nucleus, .. })
                    | Noad::Under(UnderNoad { nucleus, .. })
                    | Noad::Over(OverNoad { nucleus, .. })
                    | Noad::Accent(AccentNoad { nucleus, .. }) => {
                        *nucleus = noad_field;
                    }
                    _ => panic!("Should not happen"),
                },
                SubformulaType::Subscript => match last_noad {
                    Noad::Normal(NormalNoad { subscript, .. })
                    | Noad::Radical(RadicalNoad { subscript, .. })
                    | Noad::Under(UnderNoad { subscript, .. })
                    | Noad::Over(OverNoad { subscript, .. })
                    | Noad::Accent(AccentNoad { subscript, .. })
                    | Noad::Vcenter(VcenterNoad { subscript, .. }) => {
                        *subscript = noad_field;
                    }
                    _ => panic!("Should not happen"),
                },
                SubformulaType::Superscript => match last_noad {
                    Noad::Normal(NormalNoad { superscript, .. })
                    | Noad::Radical(RadicalNoad { superscript, .. })
                    | Noad::Under(UnderNoad { superscript, .. })
                    | Noad::Over(OverNoad { superscript, .. })
                    | Noad::Accent(AccentNoad { superscript, .. })
                    | Noad::Vcenter(VcenterNoad { superscript, .. }) => {
                        *superscript = noad_field;
                    }
                    _ => panic!("Should not happen"),
                },
            }
        }
    }
}

/// See 1069.
fn extra_right_brace(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) {
    logger.print_err("Extra }, or forgotten ");
    match eqtb.cur_group.typ {
        GroupType::SemiSimple => {
            logger.print_esc_str(b"endgroup");
        }
        GroupType::MathShift { .. } => {
            logger.print_char(b'$');
        }
        GroupType::MathLeft => {
            logger.print_esc_str(b"right");
        }
        _ => panic!("Impossible"),
    }
    let help = &[
        "I've deleted a group-closing symbol because it seems to be",
        "spurious, as in `$x}$'. But perhaps the } is legitimate and",
        "you forgot something else, as in `\\hbox{$x}'. In such cases",
        "the way to recover is to insert both the forgotten and the",
        "deleted material, e.g., by typing `I$}'.",
    ];
    logger.error(help, scanner, eqtb);
    scanner.align_state += 1;
}

/// See 1119.
fn build_discretionary(
    typ: DiscGroupType,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    eqtb.unsave(scanner, logger);
    let old_level = nest.pop_nest(eqtb);
    let RichMode::Horizontal(hmode) = old_level.mode else {
        panic!("The mode must have been horizontal");
    };
    let mut cur_list = hmode.list;
    prune_current_list(&mut cur_list, scanner, eqtb, logger);
    let next_type;
    let Some(disc_node) = nest.last_disc_mut() else {
        panic!("The last node must be a DiscNode here");
    };
    match typ {
        DiscGroupType::PreBreak => {
            disc_node.pre_break = cur_list;
            next_type = DiscGroupType::PostBreak;
        }
        DiscGroupType::PostBreak => {
            disc_node.post_break = cur_list;
            next_type = DiscGroupType::NoBreak;
        }
        DiscGroupType::NoBreak => {
            ensure_list_is_empty_in_math_mode(&mut cur_list, scanner, eqtb, logger);
            disc_node.no_break = cur_list;

            // Note: We need to update the last node copies of Eqtb here as the last node of the
            // no_break part is considered part of the enclosing list.
            nest.update_last_node_info(eqtb);

            return;
        }
    }
    eqtb.new_save_level(
        GroupType::Disc { typ: next_type },
        &scanner.input_stack,
        logger,
    );
    scanner.scan_left_brace(eqtb, logger);
    nest.push_nest(
        RichMode::Horizontal(HorizontalMode::new_restricted()),
        &scanner.input_stack,
        eqtb,
        logger,
    );
}

/// See 1120.
fn ensure_list_is_empty_in_math_mode(
    cur_list: &mut Vec<Node>,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    if eqtb.mode().base() == Mode::DisplayMath && !cur_list.is_empty() {
        logger.print_err("Illegal math ");
        logger.print_esc_str(b"discretionary");
        let help = &[
            "Sorry: The third part of a discretionary break must be",
            "empty, in math formulas. I had to delete your third part.",
        ];
        *cur_list = Vec::new();
        logger.error(help, scanner, eqtb);
    }
}

/// See 1121.
fn prune_current_list(
    list: &mut Vec<Node>,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    for pos in 0..list.len() {
        match &list[pos] {
            Node::Char(_) | Node::List(_) | Node::Rule(_) | Node::Kern(_) | Node::Ligature(_) => {}
            _ => {
                logger.print_err("Improper discretionary list");
                let help = &["Discretionary lists must contain only boxes and kerns."];
                logger.error(help, scanner, eqtb);
                logger.begin_diagnostic(eqtb.tracing_online());
                logger.print_nl_str("The following discretionary sublist has been deleted:");
                show_box_content(&list[pos..], eqtb, logger);
                logger.end_diagnostic(true);
                list.truncate(pos);
                break;
            }
        }
    }
}

/// See 1086.
fn package(
    is_vtop: bool,
    context: BoxContext,
    spec: TargetSpec,
    page_builder: &mut PageBuilder,
    output: &mut Output,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let d = eqtb.dimen(DimensionVariable::BoxMaxDepth);
    eqtb.unsave(scanner, logger);
    let old_level = nest.pop_nest(eqtb);
    let list_node = match old_level.mode {
        RichMode::Horizontal(hmode) => hpack(hmode.list, spec, eqtb, logger),
        RichMode::Vertical(vmode) => {
            let mut vbox = vpackage(vmode.list, spec, d, eqtb, logger);
            if is_vtop {
                readjust_height_and_depth(&mut vbox);
            }
            vbox
        }
        RichMode::Math(_) => panic!("We should never have math mode here"),
    };
    box_end(
        Some(list_node),
        context,
        page_builder,
        output,
        nest,
        scanner,
        eqtb,
        logger,
    )
}

/// See 1087.
fn readjust_height_and_depth(cur_box: &mut ListNode) {
    let list = match &cur_box.list {
        HlistOrVlist::Hlist(list) => list,
        HlistOrVlist::Vlist(list) => list,
    };
    let h = match list.first() {
        Some(&Node::List(ListNode { height, .. }) | &Node::Rule(RuleNode { height, .. })) => height,
        _ => 0,
    };

    cur_box.depth += cur_box.height - h;
    cur_box.height = h;
}

/// See 1060.
pub fn get_hskip(
    hskip: Hskip,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> GlueNode {
    let glue_spec = match hskip {
        Hskip::Hskip => scan_glue(false, scanner, eqtb, logger),
        Hskip::Hfil => GlueSpec::fil_glue(),
        Hskip::Hfill => GlueSpec::fill_glue(),
        Hskip::Hss => GlueSpec::ss_glue(),
        Hskip::Hfilneg => GlueSpec::fil_neg_glue(),
    };
    GlueNode::new(glue_spec)
}

/// See 1060.
pub fn get_vskip(
    vskip: Vskip,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> GlueNode {
    let glue_spec = match vskip {
        Vskip::Vskip => scan_glue(false, scanner, eqtb, logger),
        Vskip::Vfil => GlueSpec::fil_glue(),
        Vskip::Vfill => GlueSpec::fill_glue(),
        Vskip::Vss => GlueSpec::ss_glue(),
        Vskip::Vfilneg => GlueSpec::fil_neg_glue(),
    };
    GlueNode::new(glue_spec)
}

/// See 1060.
pub fn get_mu_glue(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) -> GlueNode {
    let glue_spec = scan_glue(true, scanner, eqtb, logger);
    let mut glue_node = GlueNode::new(glue_spec);
    glue_node.subtype = GlueType::MuGlue;
    glue_node
}

/// See 1061.
pub fn scan_kern(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) -> Node {
    let dimen = scan_normal_dimen(scanner, eqtb, logger);
    let kern_node = KernNode {
        subtype: KernSubtype::Explicit,
        width: dimen,
    };
    Node::Kern(kern_node)
}

/// See 1061.
pub fn scan_mu_kern(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) -> Node {
    let dimen = scan_mu_dimen(scanner, eqtb, logger);
    let kern_node = KernNode {
        subtype: KernSubtype::MuGlue,
        width: dimen,
    };
    Node::Kern(kern_node)
}

/// See 1064.
pub fn off_save(
    unexpandable_command: UnexpandableCommand,
    token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    if let GroupType::BottomLevel = eqtb.cur_group.typ {
        drop_current_token_and_complain(unexpandable_command, scanner, eqtb, logger);
    } else {
        scanner.back_input(token, eqtb, logger);
        logger.print_err("Missing ");
        let token_list = prepare_to_insert_token_that_matches_cur_group(eqtb, logger);
        logger.print_str(" inserted");
        scanner.ins_list(token_list, eqtb, logger);
        let help = &[
            "I've inserted something that you may have forgotten.",
            "(See the <inserted text> above.)",
            "With luck, this will get me unwedged. But if you",
            "really didn't forget anything, try typing `2' now; then",
            "my insertion and my current dilemma will both disappear.",
        ];
        logger.error(help, scanner, eqtb);
    }
}

/// See 1065.
fn prepare_to_insert_token_that_matches_cur_group(eqtb: &Eqtb, logger: &mut Logger) -> Vec<Token> {
    match eqtb.cur_group.typ {
        GroupType::SemiSimple => {
            logger.print_esc_str(b"endgroup");
            vec![Token::CSToken {
                cs: ControlSequence::FrozenEndGroup,
            }]
        }
        GroupType::MathShift { .. } => {
            logger.print_char(b'$');
            vec![Token::MATH_SHIFT_TOKEN]
        }
        GroupType::MathLeft => {
            logger.print_esc_str(b"right.");
            vec![
                Token::CSToken {
                    cs: ControlSequence::FrozenRight,
                },
                Token::OtherChar(b'.'),
            ]
        }
        _ => {
            logger.print_char(b'}');
            vec![Token::RIGHT_BRACE_TOKEN]
        }
    }
}

/// See 1066.
fn drop_current_token_and_complain(
    unexpandable_command: UnexpandableCommand,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    logger.print_err("Extra ");
    unexpandable_command.display(&eqtb.fonts, logger);
    let help = &["Things are pretty mixed up, but I think the worst is over."];
    logger.error(help, scanner, eqtb)
}

/// See 1070.
pub fn normal_paragraph(eqtb: &mut Eqtb, logger: &mut Logger) {
    if eqtb.integer(IntegerVariable::Looseness) != 0 {
        eqtb.int_define(IntegerVariable::Looseness, 0, false, logger);
    }
    if eqtb.dimen(DimensionVariable::HangIndent) != 0 {
        eqtb.dimen_define(DimensionVariable::HangIndent, 0, false);
    }
    if eqtb.integer(IntegerVariable::HangAfter) != 1 {
        eqtb.int_define(IntegerVariable::HangAfter, 1, false, logger);
    }
    if !eqtb.par_shape.get(ParShapeVariable).is_empty() {
        eqtb.par_shape_define(ParagraphShape::new(), false);
    }
}

/// See 1091.
pub fn new_graf(
    is_indented: bool,
    hyphenator: &mut Hyphenator,
    page_builder: &mut PageBuilder,
    output: &mut Output,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let RichMode::Vertical(vmode) = nest.mode_mut() else {
        panic!("It must be vertical mode");
    };
    vmode.set_prev_graf(0, eqtb);

    if !vmode.internal || !vmode.list.is_empty() {
        let par_skip = GlueNode::new_param(SkipVariable::ParSkip, eqtb);
        vmode.append_node(Node::Glue(par_skip), eqtb);
    }
    hyphenator.set_cur_lang(eqtb);
    let lang_data = LanguageData {
        left_hyphen_min: norm_min(eqtb.integer(IntegerVariable::LeftHyphenMin)),
        right_hyphen_min: norm_min(eqtb.integer(IntegerVariable::RightHyphenMin)),
        language: hyphenator.cur_lang,
    };
    nest.push_nest(
        RichMode::Horizontal(HorizontalMode {
            list: Vec::new(),
            subtype: HorizontalModeType::Unrestricted {
                clang: hyphenator.cur_lang as u16,
                lang_data,
            },
            space_factor: 1000,
        }),
        &scanner.input_stack,
        eqtb,
        logger,
    );
    if is_indented {
        let mut hbox = ListNode::new_empty_hbox();
        hbox.width = eqtb.dimen(DimensionVariable::ParIndent);
        nest.tail_push(Node::List(hbox), eqtb);
    }
    if let Some(every_par) = eqtb.token_lists.get(TokenListVariable::EveryPar) {
        scanner.input_stack.begin_token_list(
            every_par.clone(),
            TokenSourceType::EveryParText,
            eqtb,
            logger,
        );
    }
    if nest.depth() == 1 {
        page_builder.build_page(output, nest, scanner, eqtb, logger);
    }
}

/// See 1093.
pub fn indent_in_hmode(hmode: &mut HorizontalMode, eqtb: &mut Eqtb) {
    let mut list_node = ListNode::new_empty_hbox();
    list_node.width = eqtb.dimen(DimensionVariable::ParIndent);
    hmode.set_space_factor(1000, eqtb);
    hmode.append_node(Node::List(list_node), eqtb);
}

/// See 1093.
pub fn indent_in_mmode(mmode: &mut MathMode, eqtb: &mut Eqtb) {
    let mut list_node = ListNode::new_empty_hbox();
    list_node.width = eqtb.dimen(DimensionVariable::ParIndent);
    let mut noad = NormalNoad::new();
    noad.nucleus = Some(Box::new(NoadField::SubBox { list_node }));
    mmode.append_node(Node::Noad(Noad::Normal(noad)), eqtb);
}

/// See 1095.
pub fn head_for_vmode(
    unexpandable_command: UnexpandableCommand,
    token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    if let Mode::RestrictedHorizontal = eqtb.mode() {
        if let UnexpandableCommand::Hrule = unexpandable_command {
            logger.print_err("You can't use `");
            logger.print_esc_str(b"hrule");
            logger.print_str("' here except with leaders");
            let help = &[
                "To put a horizontal rule in an hbox or an alignment,",
                "you should use \\leaders or \\hrulefill (see The TeXbook).",
            ];
            logger.error(help, scanner, eqtb);
        } else {
            off_save(unexpandable_command, token, scanner, eqtb, logger)
        }
    } else {
        scanner.back_input(token, eqtb, logger);
        scanner.input_token(TokenSourceType::Inserted, eqtb.par_token, eqtb, logger);
    }
}

/// See 1096.
pub fn end_graf(
    hyphenator: &mut Hyphenator,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    if let &RichMode::Horizontal(HorizontalMode {
        subtype: HorizontalModeType::Unrestricted { lang_data, .. },
        ..
    }) = nest.mode()
    {
        let old_level = nest.pop_nest(eqtb);
        let RichMode::Horizontal(hmode) = old_level.mode else {
            panic!("We know it is horizontal");
        };
        if !hmode.list.is_empty() {
            let widow_penalty = eqtb.integer(IntegerVariable::WidowPenalty);
            line_break(
                hmode.list,
                &lang_data,
                old_level.mode_line,
                widow_penalty,
                false,
                hyphenator,
                nest,
                scanner,
                eqtb,
                logger,
            );
        }
        normal_paragraph(eqtb, logger);
        logger.error_count = 0;
    }
}

/// See 1099.
pub fn begin_insert(
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let mut box_number;
    box_number = scanner.scan_register_index(eqtb, logger);
    if box_number == 255 {
        logger.print_err("You can't ");
        logger.print_esc_str(b"insert");
        logger.print_int(255);
        let help = &["I'm changing to \\insert0; box 255 is special."];
        logger.error(help, scanner, eqtb);
        box_number = 0;
    }
    eqtb.new_save_level(
        GroupType::Insert { number: box_number },
        &scanner.input_stack,
        logger,
    );
    scanner.scan_left_brace(eqtb, logger);
    normal_paragraph(eqtb, logger);
    nest.push_nest(
        RichMode::Vertical(VerticalMode::new_internal()),
        &scanner.input_stack,
        eqtb,
        logger,
    )
}

/// See 1099.
pub fn begin_adjust(
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let box_number = 255;
    eqtb.new_save_level(
        GroupType::Insert { number: box_number },
        &scanner.input_stack,
        logger,
    );
    scanner.scan_left_brace(eqtb, logger);
    normal_paragraph(eqtb, logger);
    nest.push_nest(
        RichMode::Vertical(VerticalMode::new_internal()),
        &scanner.input_stack,
        eqtb,
        logger,
    )
}

/// See 1101.
pub fn make_mark(
    token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Node {
    let Token::CSToken { cs } = token else {
        panic!("Impossible")
    };
    let token_list = scanner.scan_toks(cs, true, eqtb, logger);
    let mark_node = MarkNode {
        mark: Rc::new(token_list),
    };
    Node::Mark(mark_node)
}

/// See 1103.
pub fn make_penalty(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) -> Node {
    let penalty = Integer::scan_int(scanner, eqtb, logger);
    let penalty_node = PenaltyNode::new(penalty);
    Node::Penalty(penalty_node)
}

/// See 1105.
pub fn delete_last(
    remove_item: RemoveItem,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    if eqtb.mode() == Mode::Vertical && nest.cur_list_is_empty() {
        apologize_for_inability_to_do_operation(remove_item, scanner, eqtb, logger);
    } else if let Some(last_node) = nest.cur_list().last() {
        if let (&Node::Penalty(_), RemoveItem::Penalty)
        | (&Node::Kern(_), RemoveItem::Kern)
        | (&Node::Glue(_), RemoveItem::Glue) = (last_node, remove_item)
        {
            nest.pop_last(eqtb);
        }
    }
}

/// See 1106.
fn apologize_for_inability_to_do_operation(
    remove_item: RemoveItem,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    // We don't give a warning if \unskip follows non-glue.
    if let RemoveItem::Glue = remove_item {
        match eqtb.last_node_on_page {
            LastNodeInfo::Penalty(_) | LastNodeInfo::Kern(_) | LastNodeInfo::Other => return,
            LastNodeInfo::Glue(_) | LastNodeInfo::MuGlue(_) => {}
        }
    }
    you_cant(
        UnexpandableCommand::RemoveItem(remove_item),
        scanner,
        eqtb,
        logger,
    );
    let help = &[
        "Sorry...I usually can't take things from the current page.",
        match remove_item {
            RemoveItem::Glue => "Try 'I\\vskip-\\lastskip' instead.",
            RemoveItem::Kern => "Try `I\\kern-\\lastkern' instead.",
            RemoveItem::Penalty => "Perhaps you can make the output routine do it.",
        },
    ];
    logger.error(help, scanner, eqtb);
}

/// See 1110.
pub fn unpackage(
    copy: bool,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let number = scanner.scan_register_index(eqtb, logger);
    let boks = eqtb.boxes.set(BoxVariable(number), None);
    let Some(list_node) = boks else { return };
    if eqtb.mode().base() == Mode::DisplayMath
        || (eqtb.mode().base() == Mode::Vertical && list_node.list.is_hlist())
        || (eqtb.mode().base() == Mode::Horizontal && list_node.list.is_vlist())
    {
        eqtb.boxes.set(BoxVariable(number), Some(list_node));
        logger.print_err("Incompatible list can't be unboxed");
        let help = &[
            "Sorry, Pandora. (You sneaky devil.)",
            "I refuse to unbox an \\hbox in vertical mode or vice versa.",
            "And I can't open any boxes in math mode.",
        ];
        logger.error(help, scanner, eqtb);
        return;
    }
    if copy {
        let list = match &list_node.list {
            HlistOrVlist::Hlist(list) => list,
            HlistOrVlist::Vlist(list) => list,
        };
        let copy_list = list.clone();
        nest.tail_append(copy_list, eqtb);
        eqtb.boxes.set(BoxVariable(number), Some(list_node));
    } else {
        let list = match list_node.list {
            HlistOrVlist::Hlist(list) => list,
            HlistOrVlist::Vlist(list) => list,
        };
        nest.tail_append(list, eqtb);
    }
}

/// See 1113.
pub fn append_italic_correction(hmode: &mut HorizontalMode, eqtb: &mut Eqtb) {
    if let Some(last_node) = hmode.list.last() {
        if let &Node::Char(CharNode { italic, .. }) | &Node::Ligature(LigatureNode { italic, .. }) =
            last_node
        {
            let mut kern_node = KernNode::new(italic);
            kern_node.subtype = KernSubtype::Explicit;
            hmode.append_node(Node::Kern(kern_node), eqtb);
        }
    }
}

/// See 1117.
pub fn hyphen_discretionary(eqtb: &Eqtb, logger: &mut Logger) -> Node {
    let mut disc_node = DiscNode::new();
    let c = eqtb.fonts[eqtb.cur_font() as usize].hyphen_char;
    if c >= 0 && c < 256 {
        disc_node.pre_break = match new_character(eqtb.cur_font(), c as u8, eqtb, logger) {
            Some(char_node) => vec![Node::Char(char_node)],
            None => Vec::new(),
        };
    }
    Node::Disc(disc_node)
}

/// See 1117.
pub fn append_discretionary(
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    nest.tail_push(Node::Disc(DiscNode::new()), eqtb);
    eqtb.new_save_level(
        GroupType::Disc {
            typ: DiscGroupType::PreBreak,
        },
        &scanner.input_stack,
        logger,
    );
    scanner.scan_left_brace(eqtb, logger);
    nest.push_nest(
        RichMode::Horizontal(HorizontalMode::new_restricted()),
        &scanner.input_stack,
        eqtb,
        logger,
    );
}

/// See 1123.
pub fn make_accent(
    hyphenator: &mut Hyphenator,
    page_builder: &mut PageBuilder,
    output: &mut Output,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let char_num = scanner.scan_char_num(eqtb, logger);
    let accent_font_index = eqtb.cur_font();
    let accent_char = new_character(accent_font_index, char_num, eqtb, logger);
    if let Some(accent_char) = accent_char {
        let (unexpandable_command, token) = do_assignments(
            hyphenator,
            page_builder,
            output,
            nest,
            scanner,
            eqtb,
            logger,
        );
        let RichMode::Horizontal(hmode) = nest.mode_mut() else {
            panic!("We must be in horizontal mode now.");
        };
        if let Some(next_char) =
            try_next_character(unexpandable_command, token, scanner, eqtb, logger)
        {
            append_accent_with_appropriate_kerns(
                accent_char,
                accent_font_index,
                next_char,
                hmode,
                eqtb,
                logger,
            );
        } else {
            hmode.append_node(Node::Char(accent_char), eqtb);
        }
        hmode.set_space_factor(1000, eqtb);
    }
}

/// See 1124.
fn try_next_character(
    unexpandable_command: UnexpandableCommand,
    token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Option<CharNode> {
    let f = eqtb.cur_font();
    match unexpandable_command {
        UnexpandableCommand::Letter(c)
        | UnexpandableCommand::Other(c)
        | UnexpandableCommand::CharGiven(c) => new_character(f, c, eqtb, logger),
        UnexpandableCommand::CharNum => {
            let char_num = scanner.scan_char_num(eqtb, logger);
            new_character(f, char_num, eqtb, logger)
        }
        _ => {
            scanner.back_input(token, eqtb, logger);
            None
        }
    }
}

/// See 1125.
fn append_accent_with_appropriate_kerns(
    accent_char: CharNode,
    accent_font_index: FontIndex,
    next_char: CharNode,
    hmode: &mut HorizontalMode,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let accent_font = &eqtb.fonts[accent_font_index as usize];
    let accent_x_height = accent_font.x_height();
    let accent_font_slant = accent_font.slant() as f64 / 65536.0;

    let char_font = &eqtb.fonts[next_char.font_index as usize];
    let char_font_slant = char_font.slant() as f64 / 65536.0;
    // If necessary, adjust the height of the accent to fit the character.
    let accent_node = if next_char.height != accent_font.x_height() {
        let mut shifted_accent = hpack(
            vec![Node::Char(accent_char)],
            TargetSpec::Natural,
            eqtb,
            logger,
        );
        shifted_accent.shift_amount = accent_x_height - next_char.height;
        Node::List(shifted_accent)
    } else {
        Node::Char(accent_char)
    };
    let width_diff = (next_char.width - accent_char.width) as f64;
    let slant_diff =
        next_char.height as f64 * char_font_slant - accent_x_height as f64 * accent_font_slant;
    let delta = round(width_diff / 2.0 + slant_diff);

    let mut first_accent_kern = KernNode::new(delta);
    first_accent_kern.subtype = KernSubtype::AccKern;
    hmode.append_node(Node::Kern(first_accent_kern), eqtb);
    hmode.append_node(accent_node, eqtb);
    let mut second_accent_kern = KernNode::new(-accent_char.width - delta);
    second_accent_kern.subtype = KernSubtype::AccKern;
    hmode.append_node(Node::Kern(second_accent_kern), eqtb);
    hmode.append_node(Node::Char(next_char), eqtb);
}

/// See 1127.
pub fn align_error(
    unexpandable_command: UnexpandableCommand,
    token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    if scanner.align_state.abs() > 2 {
        express_consternation_over_fact_that_no_alignment_in_progress(
            unexpandable_command,
            token,
            scanner,
            eqtb,
            logger,
        );
    } else {
        scanner.back_input(token, eqtb, logger);
        let token = if scanner.align_state < 0 {
            logger.print_err("Missing { inserted");
            Token::LEFT_BRACE_TOKEN
        } else {
            logger.print_err("Missing } inserted");
            Token::RIGHT_BRACE_TOKEN
        };
        let help = &[
            "I've put in what seems to be necessary to fix",
            "the current column of the current alignment.",
            "Try to go on, since this might almost work.",
        ];
        scanner.ins_error(token, help, eqtb, logger);
    }
}

/// See 1128.
fn express_consternation_over_fact_that_no_alignment_in_progress(
    unexpandable_command: UnexpandableCommand,
    token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    logger.print_err("Misplaced ");
    unexpandable_command.display(&eqtb.fonts, logger);
    let help: &[_] = if token == Token::TabMark(b'&') {
        &[
            "I can't figure out why you would want to use a tab mark",
            "here. If you just want an ampersand, the remedy is",
            "simple: Just type `I\\&' now. But if some right brace",
            "up above has ended a previous alignment prematurely,",
            "you're probably due for more error messages, and you",
            "might try typing `S' now just to see what is salvageable.",
        ]
    } else {
        &[
            "I can't figure out why you would want to use a tab mark",
            "or \\cr or \\span just now. If something like a right brace",
            "up above has ended a previous alignment prematurely,",
            "you're probably due for more error messages, and you",
            "might try typing `S' now just to see what is salvageable.",
        ]
    };
    logger.error(help, scanner, eqtb);
}

/// See 1129.
pub fn no_align_error(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) {
    logger.print_err("Misplaced ");
    logger.print_esc_str(b"noalign");
    let help = &[
        "I expect to see \\noalign only after the \\cr of",
        "an alignment. Proceed, and I'll ignore this case.",
    ];
    logger.error(help, scanner, eqtb)
}

/// See 1129.
pub fn omit_error(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) {
    logger.print_err("Misplaced ");
    logger.print_esc_str(b"omit");
    let help = &[
        "I expect to see \\omit only after tab marks or the \\cr of",
        "an alignment. Proceed, and I'll ignore this case.",
    ];
    logger.error(help, scanner, eqtb)
}

/// See 1135.
pub fn cs_error(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) {
    logger.print_err("Extra ");
    logger.print_esc_str(b"endcsname");
    let help = &["I'm ignoring this, since I wasn't doing a \\csname."];
    logger.error(help, scanner, eqtb)
}
