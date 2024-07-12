use crate::box_building::off_save;
use crate::command::{
    Command, FractionCommand, FractionType, LimitType, MathCommand, UnexpandableCommand,
};
use crate::dimension::{scan_normal_dimen, Dimension, MAX_DIMEN};
use crate::eqtb::save_stack::{GroupType, MathShiftKind, SubformulaType};
use crate::eqtb::{
    ControlSequence, DimensionVariable, Eqtb, FontIndex, IntegerVariable, MathFontSize,
    ParShapeVariable, SkipVariable, TokenListVariable, NULL_FONT, VAR_CODE,
};
use crate::fonts::{
    char_warning, new_character, CharTag, ExtensibleRecipe, FontInfo, LigKernCommand,
};
use crate::horizontal_mode::{HorizontalMode, HorizontalModeType, LanguageData};
use crate::hyphenation::Hyphenator;
use crate::input::expansion::{get_x_token, x_token};
use crate::input::token_source::TokenSourceType;
use crate::input::InputStack;
use crate::input::Scanner;
use crate::integer::{Integer, IntegerExt};
use crate::line_breaking::{line_break, INF_PENALTY};
use crate::logger::Logger;
use crate::math_mode::MathMode;
use crate::nodes::noads::{
    AccentNoad, ChoiceNode, DelimiterField, FractionNoad, LeftNoad, Noad, NoadField, NoadType,
    NormalNoad, OverNoad, RadicalNoad, RightNoad, StyleNode, UnderNoad, VcenterNoad, DEFAULT_CODE,
};
use crate::nodes::{
    CharNode, DimensionOrder, GlueNode, GlueSign, GlueSpec, GlueType, HlistOrVlist, KernNode,
    KernSubtype, ListNode, MathNode, MathNodeKind, Node, PenaltyNode, RuleNode,
};
use crate::norm_min;
use crate::output::Output;
use crate::packaging::{
    hpack, measure_hlist, split_adjust_material_off, vpack, Measurement, TargetSpec,
};
use crate::page_breaking::PageBuilder;
use crate::print::Printer;
use crate::scaled::{nx_plus_y, xn_over_d, Scaled, UNITY};
use crate::semantic_nest::{Mode, RichMode, SemanticState};
use crate::token::Token;

const TOTAL_MATHSY_PARAMS: usize = 22;
const TOTAL_MATHEX_PARAMS: usize = 13;

/// Divides by two and rounds towards infinity.
/// See 100.
fn half(x: i32) -> i32 {
    if x.rem_euclid(2) == 1 {
        (x + 1) / 2
    } else {
        x / 2
    }
}

/// See 688.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MathStyle {
    Display { cramped: bool },
    Text { cramped: bool },
    Script { cramped: bool },
    ScriptScript { cramped: bool },
}

impl MathStyle {
    /// See 703.
    const fn cur_size(&self) -> MathFontSize {
        match self {
            Self::Display { .. } | Self::Text { .. } => MathFontSize::Text,
            Self::Script { .. } => MathFontSize::Script,
            Self::ScriptScript { .. } => MathFontSize::ScriptScript,
        }
    }

    /// See 694.
    pub fn display(&self, printer: &mut impl Printer) {
        let s: &[u8] = match self {
            Self::Display { .. } => b"displaystyle",
            Self::Text { .. } => b"textstyle",
            Self::Script { .. } => b"scriptstyle",
            Self::ScriptScript { .. } => b"scriptscriptstyle",
        };
        printer.print_esc_str(s);
    }

    /// Returns the cramped version of the style.
    /// See 702.
    fn cramped_style(&self) -> Self {
        match self {
            Self::Display { .. } => Self::Display { cramped: true },
            Self::Text { .. } => Self::Text { cramped: true },
            Self::Script { .. } => Self::Script { cramped: true },
            Self::ScriptScript { .. } => Self::ScriptScript { cramped: true },
        }
    }

    /// Return the style for subscripts of the given style.
    /// See 702.
    fn sub_style(&self) -> Self {
        match self {
            Self::Display { .. } | Self::Text { .. } => Self::Script { cramped: true },
            Self::Script { .. } | Self::ScriptScript { .. } => Self::ScriptScript { cramped: true },
        }
    }

    /// Return the style for superscripts of the given style.
    /// See 702.
    fn sup_style(&self) -> Self {
        match *self {
            Self::Display { cramped } | Self::Text { cramped } => Self::Script { cramped },
            Self::Script { cramped } | Self::ScriptScript { cramped } => {
                Self::ScriptScript { cramped }
            }
        }
    }

    /// Return the style for numerators of the given style.
    /// See 702.
    fn num_style(&self) -> Self {
        match *self {
            Self::Display { cramped } => Self::Text { cramped },
            Self::Text { cramped } => Self::Script { cramped },
            Self::Script { cramped } | Self::ScriptScript { cramped } => {
                Self::ScriptScript { cramped }
            }
        }
    }

    /// Return the style for denominators of the given style.
    /// See 702.
    fn denom_style(&self) -> Self {
        match *self {
            Self::Display { .. } => Self::Text { cramped: true },
            Self::Text { .. } => Self::Script { cramped: true },
            Self::Script { .. } | Self::ScriptScript { .. } => Self::ScriptScript { cramped: true },
        }
    }

    /// See 703.
    fn cur_mu(&self, eqtb: &Eqtb) -> i32 {
        eqtb.math_quad(self.cur_size()) / 18
    }
}

/// See 704.
fn fraction_rule(thickness: Dimension) -> RuleNode {
    let mut rule = RuleNode::new();
    rule.height = thickness;
    rule.depth = 0;
    rule
}

/// See 705.
fn overbar(
    boks: ListNode,
    kern_height: Dimension,
    thickness: Dimension,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> ListNode {
    let top_kern = KernNode::new(thickness);
    let bar = fraction_rule(thickness);
    let inner_kern = KernNode::new(kern_height);
    let vlist = vec![
        Node::Kern(top_kern),
        Node::Rule(bar),
        Node::Kern(inner_kern),
        Node::List(boks),
    ];
    vpack(vlist, TargetSpec::Natural, eqtb, logger)
}

/// See 706.
fn var_delimiter(
    delimiter: DelimiterField,
    size: MathFontSize,
    target_size: Dimension,
    eqtb: &Eqtb,
) -> ListNode {
    let mut font_index = NULL_FONT;
    let mut w = 0;
    let mut c = 0;
    for (fam, chr) in [
        (delimiter.small_fam, delimiter.small_char),
        (delimiter.large_fam, delimiter.large_char),
    ] {
        if let Some(()) = look_at_variants(
            fam,
            chr,
            size,
            &mut font_index,
            &mut c,
            target_size,
            &mut w,
            eqtb,
        ) {
            break;
        }
    }
    let mut b;
    if font_index != NULL_FONT {
        b = make_variable_b_point_to_box_for_f_c(
            &eqtb.fonts[font_index as usize],
            font_index,
            c,
            target_size,
        );
    } else {
        b = ListNode::new_empty_hbox();
        b.width = eqtb.dimen(DimensionVariable::NullDelimiterSpace);
    }
    b.shift_amount = half(b.height - b.depth) - eqtb.axis_height(size);
    b
}

/// Returns some () for goto found else None.
/// See 707.
fn look_at_variants(
    fam: u8,
    chr: u8,
    mut size: MathFontSize,
    font_index: &mut FontIndex,
    c: &mut u8,
    target_size: Dimension,
    w: &mut Dimension,
    eqtb: &Eqtb,
) -> Option<()> {
    if fam != 0 || chr != 0 {
        loop {
            let g = eqtb.fam_fnt(size, fam as usize);
            if g != NULL_FONT {
                if let Some(()) = look_at_possible_character_variants(
                    chr,
                    g,
                    &eqtb.fonts[g as usize],
                    font_index,
                    c,
                    target_size,
                    w,
                ) {
                    return Some(());
                }
            }
            size = match size {
                MathFontSize::ScriptScript => MathFontSize::Script,
                MathFontSize::Script => MathFontSize::Text,
                MathFontSize::Text => break,
            }
        }
    }
    None
}

/// Returns some () for goto found else None.
/// See 708.
fn look_at_possible_character_variants(
    mut chr: u8,
    g: FontIndex,
    font: &FontInfo,
    f: &mut FontIndex,
    c: &mut u8,
    target_size: Dimension,
    w: &mut Dimension,
) -> Option<()> {
    if chr >= font.bc && chr <= font.ec {
        while font.char_exists(chr) {
            if let CharTag::Ext(_) = font.tag(chr) {
                *f = g;
                *c = chr;
                return Some(());
            }
            let u = font.height(chr) + font.depth(chr);
            if u > *w {
                *f = g;
                *c = chr;
                *w = u;
                if u >= target_size {
                    return Some(());
                }
            }
            if let CharTag::List(next_char) = font.tag(chr) {
                chr = next_char;
            } else {
                break;
            }
        }
    }
    None
}

/// See 709.
fn char_box(font: &FontInfo, font_index: FontIndex, c: u8) -> ListNode {
    let mut list_node = ListNode::new_empty_hbox();
    list_node.width = font.width(c) + font.italic(c);
    list_node.height = font.height(c);
    list_node.depth = font.depth(c);
    list_node.list = HlistOrVlist::Hlist(vec![Node::Char(CharNode {
        font_index,
        character: c,
        width: font.width(c),
        height: font.height(c),
        depth: font.depth(c),
        italic: font.italic(c),
    })]);
    list_node
}

/// See 710.
fn make_variable_b_point_to_box_for_f_c(
    font: &FontInfo,
    font_index: FontIndex,
    c: u8,
    target_size: Dimension,
) -> ListNode {
    if let CharTag::Ext(ext_index) = font.tag(c) {
        create_extensible_character_vbox(ext_index, font, font_index, target_size)
    } else {
        char_box(font, font_index, c)
    }
}

/// See 712.
fn height_plus_depth(font: &FontInfo, c: u8) -> Dimension {
    font.height(c) + font.depth(c)
}

/// Construct an extensible character with a size of at least the given target size.
/// The extensible character is constructed as vbox with a list of the individual characters
/// wrapped in hboxes. The height is the height of the topmost character.
/// See 713.
fn create_extensible_character_vbox(
    ext_index: usize,
    font: &FontInfo,
    font_index: FontIndex,
    target_size: Dimension,
) -> ListNode {
    let mut b = ListNode::new_empty_hbox();
    let ext = font.extens[ext_index];
    b.width = font.width(ext.rep) + font.italic(ext.rep);

    let (reps, total_height) = compute_repetitions_and_total_height(ext, font, target_size);

    let mut list = Vec::new();
    let rep = ext.rep;
    if let Some(top) = ext.top {
        list.push(Node::List(char_box(font, font_index, top)));
    }
    if let Some(mid) = ext.mid {
        for _ in 1..=reps {
            list.push(Node::List(char_box(font, font_index, rep)));
        }
        list.push(Node::List(char_box(font, font_index, mid)));
    }
    for _ in 1..=reps {
        list.push(Node::List(char_box(font, font_index, rep)));
    }
    if let Some(bot) = ext.bot {
        list.push(Node::List(char_box(font, font_index, bot)));
    }
    // Set the box height to the height of the topmost component.
    b.height = if let Some(Node::List(top_node)) = list.first() {
        top_node.height
    } else {
        0
    };
    b.list = HlistOrVlist::Vlist(list);
    b.depth = total_height - b.height;
    b
}

/// See 714.
fn compute_repetitions_and_total_height(
    r: ExtensibleRecipe,
    font: &FontInfo,
    target_size: Scaled,
) -> (usize, Scaled) {
    let rep = r.rep;
    let rep_height = height_plus_depth(font, rep);
    let mut total_height = 0;
    if let Some(bot) = r.bot {
        total_height += height_plus_depth(font, bot);
    }
    if let Some(mid) = r.mid {
        total_height += height_plus_depth(font, mid);
    }
    if let Some(top) = r.top {
        total_height += height_plus_depth(font, top);
    }
    let mut reps = 0;
    if rep_height > 0 {
        while total_height < target_size {
            total_height += rep_height;
            reps += 1;
            if r.mid.is_some() {
                total_height += rep_height
            }
        }
    }
    (reps, total_height)
}

/// Try to ensure that the content of the given box is centered within a box of the given width.
/// See 715.
fn rebox(mut b: ListNode, w: Scaled, eqtb: &mut Eqtb, logger: &mut Logger) -> ListNode {
    if b.width == w {
        return b;
    }
    if match &b.list {
        HlistOrVlist::Hlist(list) | HlistOrVlist::Vlist(list) => list.is_empty(),
    } {
        b.width = w;
        return b;
    }

    let mut hlist = match b.list {
        HlistOrVlist::Hlist(mut hlist) => {
            if let &[Node::Char(char_node)] = &hlist[..] {
                if char_node.width != b.width {
                    hlist.push(Node::Kern(KernNode::new(b.width - char_node.width)));
                }
            }
            hlist
        }
        HlistOrVlist::Vlist(_) => {
            vec![Node::List(b)]
        }
    };

    let mut new_list = Vec::new();
    new_list.push(Node::Glue(GlueNode::new(GlueSpec::ss_glue())));
    new_list.append(&mut hlist);
    new_list.push(Node::Glue(GlueNode::new(GlueSpec::ss_glue())));
    hpack(new_list, TargetSpec::Exactly(w), eqtb, logger)
}

/// Convert a size in mu to a size in pt.
/// `n` and `f` are the integral and fractional parts of 1 mu / 1 pt.
/// See 716.
fn mu_mult(mu_size: Dimension, n: i32, f: i32) -> i32 {
    // NOTE It could be possible to trigger an overflow here when a large math font is
    // used together with a huge mu dimension.
    nx_plus_y(
        n,
        mu_size,
        xn_over_d(mu_size, f, UNITY).expect("Should not overflow as f < UNITY"),
    )
    .expect("Overflow when translating dimension from mu units to scaled points")
}

/// Convert a GlueSpec in mu units to one in pt units.
/// See 716.
fn math_glue(mu_glue_spec: &GlueSpec, m: Scaled) -> GlueSpec {
    // First the integral part of m in pt.
    let n = m.div_euclid(UNITY);
    // Then the fractional part of m in pt.
    let f = m.rem_euclid(UNITY);
    let mut stretch = mu_glue_spec.stretch;
    if stretch.order == DimensionOrder::Normal {
        stretch.value = mu_mult(stretch.value, n, f);
    }
    let mut shrink = mu_glue_spec.shrink;
    if shrink.order == DimensionOrder::Normal {
        shrink.value = mu_mult(shrink.value, n, f);
    }
    GlueSpec {
        width: mu_mult(mu_glue_spec.width, n, f),
        stretch,
        shrink,
    }
}

/// See 717.
fn math_kern(kern_node: &mut KernNode, m: Scaled) {
    if kern_node.subtype == KernSubtype::MuGlue {
        let n = m.div_euclid(UNITY);
        let f = m.rem_euclid(UNITY);
        kern_node.width = mu_mult(kern_node.width, n, f);
        kern_node.subtype = KernSubtype::Explicit;
    }
}

/// Make a NoadField into an hbox.
/// See 720.
fn clean_box(
    field: NoadField,
    style: MathStyle,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> ListNode {
    let mut q = Vec::new();
    match field {
        math_char @ NoadField::MathChar { .. } => {
            let noad = Node::Noad(Noad::Normal(NormalNoad {
                noad_type: NoadType::Ord,
                nucleus: Some(Box::new(math_char)),
                subscript: None,
                superscript: None,
            }));
            let mlist = vec![noad];

            q = mlist_to_hlist(mlist, style, false, scanner, eqtb, logger);
        }
        NoadField::SubBox { list_node } => {
            q.push(Node::List(list_node));
        }
        NoadField::SubMlist { list: mlist } => {
            q = mlist_to_hlist(mlist, style, false, scanner, eqtb, logger);
        }
        _ => {
            q.push(Node::List(ListNode::new_empty_hbox()));
        }
    }
    // found:
    let mut x = if q.len() == 1 {
        let q = q.pop().unwrap();
        match q {
            // Don't pack a single hlist or vlist node (with a shift amount of 0) again.
            Node::List(list_node) if list_node.shift_amount == 0 => list_node,
            _ => hpack(vec![q], TargetSpec::Natural, eqtb, logger),
        }
    } else {
        hpack(q, TargetSpec::Natural, eqtb, logger)
    };
    simplify_trivial_box(&mut x);
    x
}

/// Remove an italic correction if the box contains only a char followed by a kern.
/// See 721.
fn simplify_trivial_box(x: &mut ListNode) {
    let list = match &mut x.list {
        HlistOrVlist::Hlist(hlist) => hlist,
        HlistOrVlist::Vlist(vlist) => vlist,
    };
    if list.len() == 2 {
        if let Some(Node::Char(_)) = list.first() {
            if let Some(Node::Kern(_)) = list.get(1) {
                list.pop();
            }
        }
    }
}

/// Returns the font number corresponding to the given font family if both the
/// family and the character therein are valid.
/// See 722.
fn fetch(
    fam: u8,
    c: u8,
    size: MathFontSize,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Option<FontIndex> {
    let font_index = eqtb.fam_fnt(size, fam as usize);
    if font_index == NULL_FONT {
        complain_about_undefined_family(fam, c, size, scanner, eqtb, logger);
        None
    } else {
        let font = &eqtb.fonts[font_index as usize];
        if c < font.bc || c > font.ec || !font.char_exists(c) {
            char_warning(font, c, eqtb, logger);
            None
        } else {
            Some(font_index)
        }
    }
}

/// See 723.
fn complain_about_undefined_family(
    fam: u8,
    c: u8,
    size: MathFontSize,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    logger.print_err("");
    logger.print_esc_str(size.as_str().as_bytes());
    logger.print_char(b' ');
    logger.print_int(fam as i32);
    logger.print_str(" is undefined (character ");
    logger.print(c);
    logger.print_char(b')');
    let help = &[
        "Somewhere in the math formula just ended, you used the",
        "stated character from an undefined font family. For example,",
        "plain TeX doesn't allow \\it or \\sl in subscripts. Proceed,",
        "and I'll try to forget that I needed that character.",
    ];
    logger.error(help, scanner, eqtb)
}

/// See 726.
fn mlist_to_hlist(
    mlist: Vec<Node>,
    style: MathStyle,
    penalties: bool,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Vec<Node> {
    let mut prev_noad: Option<(usize, NoadType)> = None;
    let mut max_h = 0;
    let mut max_d = 0;
    let mut cur_style = style;
    let mut mlist_stack = mlist;
    mlist_stack.reverse();
    let mut first_pass_list = Vec::new();
    while let Some(node) = mlist_stack.pop() {
        process_node_or_noad_as_much_as_possible(
            node,
            &mut mlist_stack,
            &mut first_pass_list,
            &mut prev_noad,
            &mut max_h,
            &mut max_d,
            &mut cur_style,
            scanner,
            eqtb,
            logger,
        );
    }
    convert_final_bin_noad_to_ord_noad(&mut first_pass_list, prev_noad);

    make_second_pass_over_mlist(first_pass_list, style, max_d, max_h, penalties, eqtb)
}

/// The possible, potentially transformed, nodes after the first pass.
#[derive(Debug)]
enum FirstPassNode {
    /// A LeftNoad. It's final size is determined in the second pass.
    Left(LeftNoad),
    /// A RightNoad. It's final size is determined in the second pass.
    Right(RightNoad),
    /// A node that is only used to change the style. It will not be passed to the hlist.
    StyleNode(StyleNode),
    /// A Node that is passed unchanged into the hlist.
    DoneNode(Node),
    /// A Noad that has been transformed to a horizontal list that will become part of the hlist.
    /// We only retain the type information to allow adding type-dependent spacing between the
    /// different Noads.
    DoneNoad(Vec<Node>, NoadType),
}

/// See 727.
fn process_node_or_noad_as_much_as_possible(
    node: Node,
    mlist_stack: &mut Vec<Node>,
    first_pass_list: &mut Vec<FirstPassNode>,
    prev_noad: &mut Option<(usize, NoadType)>,
    max_h: &mut Scaled,
    max_d: &mut Scaled,
    cur_style: &mut MathStyle,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let first_pass_node = do_first_pass_processing(
        node,
        mlist_stack,
        first_pass_list,
        *prev_noad,
        max_h,
        max_d,
        cur_style,
        scanner,
        eqtb,
        logger,
    );
    match &first_pass_node {
        FirstPassNode::DoneNoad(hlist, noad_type) => {
            // check_dimensions:
            let measurement = measure_hlist(hlist);
            if measurement.height > *max_h {
                *max_h = measurement.height;
            }
            if measurement.depth > *max_d {
                *max_d = measurement.depth;
            }
            // done_with_noad:
            *prev_noad = Some((first_pass_list.len(), *noad_type));
        }
        FirstPassNode::Left(_) => {
            // done_with_noad:
            *prev_noad = Some((first_pass_list.len(), NoadType::Open));
        }
        FirstPassNode::Right(_) => {
            // done_with_noad:
            *prev_noad = Some((first_pass_list.len(), NoadType::Close));
        }
        FirstPassNode::DoneNode(_) | FirstPassNode::StyleNode(_) => {}
    }
    first_pass_list.push(first_pass_node);
}

/// See 728., 730. and 733.
fn do_first_pass_processing(
    mut node: Node,
    mlist_stack: &mut Vec<Node>,
    first_pass_list: &mut Vec<FirstPassNode>,
    prev_noad: Option<(usize, NoadType)>,
    max_h: &mut Scaled,
    max_d: &mut Scaled,
    cur_style: &mut MathStyle,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> FirstPassNode {
    let delta = 0;
    let noad_type;
    match node {
        Node::Noad(noad) => match noad {
            Noad::Normal(mut normal_noad) => {
                match normal_noad.noad_type {
                    NoadType::Ord => {
                        let hlist =
                            make_ord(normal_noad, mlist_stack, *cur_style, scanner, eqtb, logger);
                        return FirstPassNode::DoneNoad(hlist, NoadType::Ord);
                    }
                    NoadType::Op { limit_type } => {
                        let hlist =
                            make_op(normal_noad, limit_type, *cur_style, scanner, eqtb, logger);
                        noad_type = NoadType::Op { limit_type };
                        return FirstPassNode::DoneNoad(hlist, noad_type);
                    }
                    NoadType::Bin => match prev_noad {
                        // BinNoad are only allowed after Ord, Close, or Inner.
                        Some((_, NoadType::Ord | NoadType::Close | NoadType::Inner)) => {
                            noad_type = NoadType::Bin;
                        }
                        // Otherwise we regard it as an OrdNoad.
                        _ => {
                            normal_noad.noad_type = NoadType::Ord;
                            let hlist = make_ord(
                                normal_noad,
                                mlist_stack,
                                *cur_style,
                                scanner,
                                eqtb,
                                logger,
                            );
                            return FirstPassNode::DoneNoad(hlist, NoadType::Ord);
                        }
                    },
                    NoadType::Rel => {
                        convert_final_bin_noad_to_ord_noad(first_pass_list, prev_noad);
                        noad_type = NoadType::Rel;
                    }
                    NoadType::Open => {
                        noad_type = NoadType::Open;
                    }
                    NoadType::Close => {
                        convert_final_bin_noad_to_ord_noad(first_pass_list, prev_noad);
                        noad_type = NoadType::Close;
                    }
                    NoadType::Punct => {
                        convert_final_bin_noad_to_ord_noad(first_pass_list, prev_noad);
                        noad_type = NoadType::Punct;
                    }
                    NoadType::Inner => {
                        noad_type = NoadType::Inner;
                    }
                }
                let hlist = convert_nucleus_to_hlist_and_attach_sub_superscripts(
                    normal_noad.nucleus,
                    normal_noad.superscript,
                    normal_noad.subscript,
                    delta,
                    *cur_style,
                    scanner,
                    eqtb,
                    logger,
                );
                FirstPassNode::DoneNoad(hlist, noad_type)
            }
            Noad::Left(left_noad) => FirstPassNode::Left(left_noad),
            Noad::Right(right_noad) => {
                convert_final_bin_noad_to_ord_noad(first_pass_list, prev_noad);
                FirstPassNode::Right(right_noad)
            }
            Noad::Fraction(fraction_noad) => {
                let list_node = make_fraction(fraction_noad, *cur_style, scanner, eqtb, logger);
                // We treat a FractionNoad as an Ord Noad.
                FirstPassNode::DoneNoad(vec![Node::List(list_node)], NoadType::Ord)
            }
            Noad::Accent(accent_noad) => {
                let hlist = make_math_accent(accent_noad, *cur_style, scanner, eqtb, logger);
                // Treat as OrdNoad
                noad_type = NoadType::Ord;
                FirstPassNode::DoneNoad(hlist, noad_type)
            }
            Noad::Under(under_noad) => {
                let hlist = make_under(under_noad, *cur_style, scanner, eqtb, logger);
                // Treat as OrdNoad
                FirstPassNode::DoneNoad(hlist, NoadType::Ord)
            }
            Noad::Over(over_noad) => {
                let hlist = make_over(over_noad, *cur_style, scanner, eqtb, logger);
                // Treat as OrdNoad
                FirstPassNode::DoneNoad(hlist, NoadType::Ord)
            }
            Noad::Radical(radical_noad) => {
                let hlist = make_radical(radical_noad, *cur_style, scanner, eqtb, logger);
                // Treat as OrdNoad
                FirstPassNode::DoneNoad(hlist, NoadType::Ord)
            }
            Noad::Vcenter(vcenter_noad) => {
                let hlist = make_vcenter(vcenter_noad, *cur_style, scanner, eqtb, logger);
                // Treat as OrdNoad
                FirstPassNode::DoneNoad(hlist, NoadType::Ord)
            }
        },
        // All the non-noads.
        // From 730.
        Node::Style(style_node) => {
            *cur_style = style_node.style;
            FirstPassNode::StyleNode(style_node)
        }
        Node::Choice(choice_node) => {
            let style_node = make_choice(choice_node, mlist_stack, *cur_style);
            FirstPassNode::StyleNode(style_node)
        }
        node @ (Node::Ins(_)
        | Node::Mark(_)
        | Node::Adjust(_)
        | Node::Whatsit(_)
        | Node::Penalty(_)
        | Node::Disc(_)) => FirstPassNode::DoneNode(node),
        Node::Rule(ref rule_node) => {
            if rule_node.height > *max_h {
                *max_h = rule_node.height;
            }
            if rule_node.depth > *max_d {
                *max_d = rule_node.depth;
            }
            FirstPassNode::DoneNode(node)
        }
        Node::Glue(ref mut glue_node) => {
            convert_math_glue_to_ordinary_glue(glue_node, mlist_stack, *cur_style, eqtb);
            FirstPassNode::DoneNode(node)
        }
        Node::Kern(ref mut kern_node) => {
            math_kern(kern_node, cur_style.cur_mu(eqtb));
            FirstPassNode::DoneNode(node)
        }
        // These are not allowed in a math list
        Node::Char(_) | Node::List(_) | Node::Ligature(_) | Node::Math(_) | Node::Unset(_) => {
            panic!("These Nodes should never appear in a math list")
        }
    }
}

/// See 729.
fn convert_final_bin_noad_to_ord_noad(
    first_pass_list: &mut Vec<FirstPassNode>,
    prev_noad: Option<(usize, NoadType)>,
) {
    if let Some((r, NoadType::Bin)) = prev_noad {
        if let FirstPassNode::DoneNoad(_, typ @ NoadType::Bin) = &mut first_pass_list[r] {
            *typ = NoadType::Ord;
        }
    }
}

/// See 731.
fn make_choice(
    choice_node: ChoiceNode,
    mlist_stack: &mut Vec<Node>,
    style: MathStyle,
) -> StyleNode {
    let mut chosen_mlist = match style {
        MathStyle::Display { .. } => choice_node.display_mlist,
        MathStyle::Text { .. } => choice_node.text_mlist,
        MathStyle::Script { .. } => choice_node.script_mlist,
        MathStyle::ScriptScript { .. } => choice_node.script_script_mlist,
    };

    chosen_mlist.reverse();
    mlist_stack.append(&mut chosen_mlist);

    // This style node does not serve any purpose (as it does not change the style) but to
    // avoid a special return case.
    StyleNode { style }
}

/// See 732.
fn convert_math_glue_to_ordinary_glue(
    glue_node: &mut GlueNode,
    mlist_stack: &mut Vec<Node>,
    style: MathStyle,
    eqtb: &Eqtb,
) {
    if let GlueType::MuGlue = glue_node.subtype {
        glue_node.glue_spec = std::rc::Rc::new(math_glue(&glue_node.glue_spec, style.cur_mu(eqtb)));
        glue_node.subtype = GlueType::Normal;
    } else if style.cur_size() != MathFontSize::Text {
        if let GlueType::NonScript = glue_node.subtype {
            if let Some(next_node) = mlist_stack.last() {
                if let Node::Glue(_) | Node::Kern(_) = next_node {
                    mlist_stack.pop();
                }
            }
        }
    }
}

/// See 734.
fn make_over(
    over_noad: OverNoad,
    style: MathStyle,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Vec<Node> {
    let list_node = overbar(
        if let Some(nucleus) = over_noad.nucleus {
            clean_box(*nucleus, style.cramped_style(), scanner, eqtb, logger)
        } else {
            ListNode::new_empty_hbox()
        },
        3 * eqtb.default_rule_thickness(style.cur_size()),
        eqtb.default_rule_thickness(style.cur_size()),
        eqtb,
        logger,
    );

    convert_nucleus_to_hlist_and_attach_sub_superscripts(
        Some(Box::new(NoadField::SubBox { list_node })),
        over_noad.superscript,
        over_noad.subscript,
        0,
        style,
        scanner,
        eqtb,
        logger,
    )
}

/// See 735.
fn make_under(
    under_noad: UnderNoad,
    style: MathStyle,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Vec<Node> {
    let x = if let Some(nucleus) = under_noad.nucleus {
        clean_box(*nucleus, style, scanner, eqtb, logger)
    } else {
        ListNode::new_empty_hbox()
    };
    let x_height = x.height;
    let p = KernNode::new(3 * eqtb.default_rule_thickness(style.cur_size()));
    let rule = fraction_rule(eqtb.default_rule_thickness(style.cur_size()));
    let vlist = vec![Node::List(x), Node::Kern(p), Node::Rule(rule)];
    let mut y = vpack(vlist, TargetSpec::Natural, eqtb, logger);
    let delta = y.height + y.depth + eqtb.default_rule_thickness(style.cur_size());
    y.height = x_height;
    y.depth = delta - y.height;
    convert_nucleus_to_hlist_and_attach_sub_superscripts(
        Some(Box::new(NoadField::SubBox { list_node: y })),
        under_noad.superscript,
        under_noad.subscript,
        0,
        style,
        scanner,
        eqtb,
        logger,
    )
}

/// See 736.
fn make_vcenter(
    vcenter_noad: VcenterNoad,
    style: MathStyle,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Vec<Node> {
    let mut v = vcenter_noad.nucleus;
    if let HlistOrVlist::Hlist(_) = v.list {
        panic!("A VcenterNoad should always contain a Vlist");
    }
    let delta = v.height + v.depth;
    v.height = eqtb.axis_height(style.cur_size()) + half(delta);
    v.depth = delta - v.height;

    let nucleus = Some(Box::new(NoadField::SubBox { list_node: v }));
    convert_nucleus_to_hlist_and_attach_sub_superscripts(
        nucleus,
        vcenter_noad.superscript,
        vcenter_noad.subscript,
        0,
        style,
        scanner,
        eqtb,
        logger,
    )
}

/// See 737.
fn make_radical(
    radical_noad: RadicalNoad,
    style: MathStyle,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Vec<Node> {
    let nucleus = if let Some(nucleus) = radical_noad.nucleus {
        clean_box(*nucleus, style.cramped_style(), scanner, eqtb, logger)
    } else {
        ListNode::new_empty_hbox()
    };
    let mut clearance;
    if let MathStyle::Display { .. } = style {
        clearance = eqtb.default_rule_thickness(style.cur_size())
            + eqtb.math_x_height(style.cur_size()).abs() / 4;
    } else {
        clearance = eqtb.default_rule_thickness(style.cur_size());
        clearance += clearance.abs() / 4;
    }
    let mut radical_symbol = var_delimiter(
        radical_noad.left_delimiter,
        style.cur_size(),
        nucleus.height + nucleus.depth + clearance + eqtb.default_rule_thickness(style.cur_size()),
        eqtb,
    );
    let delta = radical_symbol.depth - (nucleus.height + nucleus.depth + clearance);
    if delta > 0 {
        clearance += half(delta);
    }
    radical_symbol.shift_amount = -(nucleus.height + clearance);
    let overlined_nucleus = overbar(nucleus, clearance, radical_symbol.height, eqtb, logger);
    let hlist = vec![Node::List(radical_symbol), Node::List(overlined_nucleus)];

    let packed_nucleus = hpack(hlist, TargetSpec::Natural, eqtb, logger);

    convert_nucleus_to_hlist_and_attach_sub_superscripts(
        Some(Box::new(NoadField::SubBox {
            list_node: packed_nucleus,
        })),
        radical_noad.superscript,
        radical_noad.subscript,
        0,
        style,
        scanner,
        eqtb,
        logger,
    )
}

/// See 738.
fn make_math_accent(
    accent_noad: AccentNoad,
    style: MathStyle,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Vec<Node> {
    // Destructure AccentNoad.
    let AccentNoad {
        accent_fam,
        accent_char,
        nucleus,
        subscript,
        superscript,
    } = accent_noad;

    let Some(acc_font_index) = fetch(
        accent_fam,
        accent_char,
        style.cur_size(),
        scanner,
        eqtb,
        logger,
    ) else {
        // If the accent char is not valid, ignore its presence.
        return convert_nucleus_to_hlist_and_attach_sub_superscripts(
            nucleus,
            superscript,
            subscript,
            0,
            style,
            scanner,
            eqtb,
            logger,
        );
    };

    if let Some(noad_field) = nucleus {
        // Treat a nucleus consisting of a single char slightly differently.
        if let NoadField::MathChar { fam, character } = *noad_field {
            if let Some(font_index) = fetch(fam, character, style.cur_size(), scanner, eqtb, logger)
            {
                make_char_accent(
                    font_index,
                    character,
                    fam,
                    acc_font_index,
                    accent_char,
                    subscript,
                    superscript,
                    style,
                    scanner,
                    eqtb,
                    logger,
                )
            } else {
                // If the nucleus contained an invalid font/char, use an empty nucleus.
                make_other_accent(
                    None,
                    acc_font_index,
                    accent_char,
                    subscript,
                    superscript,
                    style,
                    scanner,
                    eqtb,
                    logger,
                )
            }
        } else {
            make_other_accent(
                Some(noad_field),
                acc_font_index,
                accent_char,
                subscript,
                superscript,
                style,
                scanner,
                eqtb,
                logger,
            )
        }
    } else {
        make_other_accent(
            None,
            acc_font_index,
            accent_char,
            subscript,
            superscript,
            style,
            scanner,
            eqtb,
            logger,
        )
    }
}

fn make_char_accent(
    font_index: FontIndex,
    character: u8,
    fam: u8,
    accent_font_index: FontIndex,
    mut accent_char: u8,
    subscript: Option<Box<NoadField>>,
    superscript: Option<Box<NoadField>>,
    style: MathStyle,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Vec<Node> {
    let font = &eqtb.fonts[font_index as usize];
    let s = compute_amount_of_skew(font, character);
    let mut x = clean_box(
        NoadField::MathChar { fam, character },
        style.cramped_style(),
        scanner,
        eqtb,
        logger,
    );
    let original_width = x.width;
    let mut h = x.height;
    let accent_font = &eqtb.fonts[accent_font_index as usize];
    switch_to_larger_accent_if_available(accent_font, &mut accent_char, original_width);
    let mut delta;
    // The baseline for the accent char should be 0 if the nucleus is smaller than x_height,
    // otherwise it should be raised by the difference between the nucleus height and x_height.
    if h < accent_font.x_height() {
        delta = h;
    } else {
        delta = accent_font.x_height();
    }
    x = swap_scripts_into_box(
        fam,
        character,
        subscript,
        superscript,
        &mut delta,
        &mut h,
        style,
        scanner,
        eqtb,
        logger,
    );
    let accent_font = &eqtb.fonts[accent_font_index as usize];
    let mut y = char_box(accent_font, accent_font_index, accent_char);
    y.shift_amount = s + half(original_width - y.width);
    y.width = 0;
    let kern = KernNode::new(-delta);
    let x_width = x.width;
    let vlist = vec![Node::List(y), Node::Kern(kern), Node::List(x)];
    y = vpack(vlist, TargetSpec::Natural, eqtb, logger);
    y.width = x_width;
    if y.height < h {
        make_height_of_box_equal(&mut y, h);
    }
    let nucleus = Some(Box::new(NoadField::SubBox { list_node: y }));
    convert_nucleus_to_hlist_and_attach_sub_superscripts(
        nucleus, None, None, 0, style, scanner, eqtb, logger,
    )
}

fn make_other_accent(
    nucleus: Option<Box<NoadField>>,
    accent_font_index: FontIndex,
    mut accent_char: u8,
    subscript: Option<Box<NoadField>>,
    superscript: Option<Box<NoadField>>,
    style: MathStyle,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Vec<Node> {
    let x = if let Some(nucleus) = nucleus {
        clean_box(*nucleus, style.cramped_style(), scanner, eqtb, logger)
    } else {
        ListNode::new_empty_hbox()
    };
    let w = x.width;
    let h = x.height;
    let accent_font = &eqtb.fonts[accent_font_index as usize];
    switch_to_larger_accent_if_available(accent_font, &mut accent_char, w);
    // The baseline for the accent char should be 0 if the nucleus is smaller than x_height,
    // otherwise it should be raised by the difference between the nucleus height and x_height.
    let delta = if h < eqtb.fonts[accent_font_index as usize].x_height() {
        h
    } else {
        eqtb.fonts[accent_font_index as usize].x_height()
    };
    let mut y = char_box(accent_font, accent_font_index, accent_char);
    y.shift_amount = half(w - y.width);
    y.width = 0;
    let kern = KernNode::new(-delta);
    let vlist = vec![Node::List(y), Node::Kern(kern), Node::List(x)];
    y = vpack(vlist, TargetSpec::Natural, eqtb, logger);
    y.width = w;
    if y.height < h {
        make_height_of_box_equal(&mut y, h);
    }
    let nucleus = Some(Box::new(NoadField::SubBox { list_node: y }));
    convert_nucleus_to_hlist_and_attach_sub_superscripts(
        nucleus,
        superscript,
        subscript,
        0,
        style,
        scanner,
        eqtb,
        logger,
    )
}

/// See 739.
fn make_height_of_box_equal(y: &mut ListNode, h: Scaled) {
    let kern = KernNode::new(h - y.height);
    match &mut y.list {
        HlistOrVlist::Hlist(_) => panic!("Impossible"),
        HlistOrVlist::Vlist(vlist) => vlist.insert(0, Node::Kern(kern)),
    }
    y.height = h;
}

/// See 740.
fn switch_to_larger_accent_if_available(font: &FontInfo, c: &mut u8, w: Scaled) {
    loop {
        if let CharTag::List(y) = font.tag(*c) {
            if !font.char_exists(y) {
                return;
            }
            if font.width(y) > w {
                return;
            }
            *c = y;
        } else {
            return;
        }
    }
}

/// See 741.
fn compute_amount_of_skew(font: &FontInfo, character: u8) -> Scaled {
    if let CharTag::Lig(lig_index) = font.tag(character) {
        let lig_kern = &font.lig_kerns[lig_index];
        let skew_char = font.skew_char;
        if skew_char >= 0 && skew_char < 256 {
            if let Some(LigKernCommand::Kern(w)) = lig_kern.get(&(skew_char as u8)) {
                return *w;
            }
        }
    }
    0
}

/// See 742.
fn swap_scripts_into_box(
    nucleus_fam: u8,
    nucleus_char: u8,
    subscript: Option<Box<NoadField>>,
    superscript: Option<Box<NoadField>>,
    delta: &mut Scaled,
    h: &mut Scaled,
    style: MathStyle,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> ListNode {
    let inner_noad = NormalNoad {
        noad_type: NoadType::Ord,
        nucleus: Some(Box::new(NoadField::MathChar {
            fam: nucleus_fam,
            character: nucleus_char,
        })),
        subscript,
        superscript,
    };
    let new_nucleus = NoadField::SubMlist {
        list: vec![Node::Noad(Noad::Normal(inner_noad))],
    };
    let x = clean_box(new_nucleus, style, scanner, eqtb, logger);
    *delta += x.height - *h;
    *h = x.height;
    x
}

/// 743.
fn make_fraction(
    fraction_noad: FractionNoad,
    style: MathStyle,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> ListNode {
    let thickness = if fraction_noad.thickness == DEFAULT_CODE {
        eqtb.default_rule_thickness(style.cur_size())
    } else {
        fraction_noad.thickness
    };
    let (x, z, mut shift_up, mut shift_down) =
        create_equal_width_boxes_for_numerator_and_denominator(
            fraction_noad.numerator,
            fraction_noad.denominator,
            thickness,
            style,
            scanner,
            eqtb,
            logger,
        );
    let delta = if thickness == 0 {
        adjust_shift_up_and_shift_down_for_no_fraction_line(
            &x,
            &z,
            &mut shift_up,
            &mut shift_down,
            style,
            eqtb,
        )
    } else {
        adjust_shift_up_and_shift_down_for_fraction_line(
            thickness,
            &x,
            &z,
            &mut shift_up,
            &mut shift_down,
            style,
            eqtb,
        )
    };
    let v =
        construct_vlist_box_for_fraction(thickness, delta, x, z, shift_up, shift_down, style, eqtb);
    put_fraction_into_box_with_delimiters(
        fraction_noad.left_delimiter,
        fraction_noad.right_delimiter,
        v,
        style,
        eqtb,
        logger,
    )
}

/// See 744.
fn create_equal_width_boxes_for_numerator_and_denominator(
    numerator: Vec<Node>,
    denominator: Vec<Node>,
    thickness: Dimension,
    style: MathStyle,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> (ListNode, ListNode, i32, i32) {
    let numerator = NoadField::SubMlist { list: numerator };
    let denominator = NoadField::SubMlist { list: denominator };
    let mut x = clean_box(numerator, style.num_style(), scanner, eqtb, logger);
    let mut z = clean_box(denominator, style.denom_style(), scanner, eqtb, logger);
    if x.width < z.width {
        x = rebox(x, z.width, eqtb, logger);
    } else {
        z = rebox(z, x.width, eqtb, logger);
    }
    let shift_up;
    let shift_down;
    if let MathStyle::Display { .. } = style {
        shift_up = eqtb.num1(style.cur_size());
        shift_down = eqtb.denom1(style.cur_size());
    } else {
        shift_down = eqtb.denom2(style.cur_size());
        if thickness != 0 {
            shift_up = eqtb.num2(style.cur_size());
        } else {
            shift_up = eqtb.num3(style.cur_size());
        }
    }
    (x, z, shift_up, shift_down)
}

/// See 745.
fn adjust_shift_up_and_shift_down_for_no_fraction_line(
    x: &ListNode,
    z: &ListNode,
    shift_up: &mut i32,
    shift_down: &mut i32,
    style: MathStyle,
    eqtb: &Eqtb,
) -> i32 {
    let clr;
    if let MathStyle::Display { .. } = style {
        clr = 7 * eqtb.default_rule_thickness(style.cur_size());
    } else {
        clr = 3 * eqtb.default_rule_thickness(style.cur_size());
    }
    let delta = half(clr - ((*shift_up - x.depth) - (z.height - *shift_down)));
    if delta > 0 {
        *shift_up += delta;
        *shift_down += delta;
    }
    delta
}

/// See 746.
fn adjust_shift_up_and_shift_down_for_fraction_line(
    thickness: Dimension,
    x: &ListNode,
    z: &ListNode,
    shift_up: &mut i32,
    shift_down: &mut i32,
    style: MathStyle,
    eqtb: &Eqtb,
) -> i32 {
    let clr;
    if let MathStyle::Display { .. } = style {
        clr = 3 * thickness;
    } else {
        clr = thickness;
    }
    let delta = half(thickness);
    let delta1 = clr - ((*shift_up - x.depth) - (eqtb.axis_height(style.cur_size()) + delta));
    let delta2 = clr - ((eqtb.axis_height(style.cur_size()) - delta) - (z.height - *shift_down));
    if delta1 > 0 {
        *shift_up += delta1;
    }
    if delta2 > 0 {
        *shift_down += delta2;
    }
    delta
}

/// See 747.
fn construct_vlist_box_for_fraction(
    thickness: Dimension,
    delta: i32,
    x: ListNode,
    z: ListNode,
    shift_up: i32,
    shift_down: i32,
    style: MathStyle,
    eqtb: &Eqtb,
) -> ListNode {
    let width = x.width;
    let height = shift_up + x.height;
    let depth = z.depth + shift_down;
    let list = if thickness == 0 {
        let kern = KernNode::new((shift_up - x.depth) - (z.height - shift_down));
        vec![Node::List(x), Node::Kern(kern), Node::List(z)]
    } else {
        let y = fraction_rule(thickness);
        let bottom_kern =
            KernNode::new((eqtb.axis_height(style.cur_size()) - delta) - (z.height - shift_down));
        let top_kern =
            KernNode::new((shift_up - x.depth) - (eqtb.axis_height(style.cur_size()) + delta));
        vec![
            Node::List(x),
            Node::Kern(top_kern),
            Node::Rule(y),
            Node::Kern(bottom_kern),
            Node::List(z),
        ]
    };
    ListNode {
        list: HlistOrVlist::Vlist(list),
        width,
        height,
        depth,
        shift_amount: 0,
        glue_sign: GlueSign::Normal,
        glue_order: DimensionOrder::Normal,
        glue_set: 0.0,
    }
}

/// See 748.
fn put_fraction_into_box_with_delimiters(
    left_delimiter: DelimiterField,
    right_delimiter: DelimiterField,
    v: ListNode,
    style: MathStyle,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> ListNode {
    let delta;
    if let MathStyle::Display { .. } = style {
        delta = eqtb.delim1(style.cur_size());
    } else {
        delta = eqtb.delim2(style.cur_size());
    }
    let x = var_delimiter(left_delimiter, style.cur_size(), delta, eqtb);
    let z = var_delimiter(right_delimiter, style.cur_size(), delta, eqtb);
    let hlist = vec![Node::List(x), Node::List(v), Node::List(z)];
    hpack(hlist, TargetSpec::Natural, eqtb, logger)
}

/// See 749.
fn make_op(
    mut op_noad: NormalNoad,
    mut limit_type: LimitType,
    style: MathStyle,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Vec<Node> {
    if let LimitType::DisplayLimits = limit_type {
        if let MathStyle::Display { .. } = style {
            limit_type = LimitType::Limits;
        }
    }
    let delta;
    if let Some(noad_field) = &mut op_noad.nucleus {
        if let NoadField::MathChar { fam, character } = noad_field.as_mut() {
            delta = match fetch(*fam, *character, style.cur_size(), scanner, eqtb, logger) {
                Some(font_index) => {
                    if let MathStyle::Display { .. } = style {
                        if let CharTag::List(next_char) =
                            eqtb.fonts[font_index as usize].tag(*character)
                        {
                            if eqtb.fonts[font_index as usize].char_exists(next_char) {
                                *character = next_char;
                            }
                        }
                    }
                    eqtb.fonts[font_index as usize].italic(*character)
                }
                None => {
                    op_noad.nucleus = None;
                    0
                }
            };
            let mut x = if let Some(nucleus) = op_noad.nucleus {
                clean_box(*nucleus, style, scanner, eqtb, logger)
            } else {
                ListNode::new_empty_hbox()
            };

            if op_noad.subscript.is_none() {
            } else if limit_type != LimitType::Limits {
                x.width -= delta;
            }
            x.shift_amount = half(x.height - x.depth) - eqtb.axis_height(style.cur_size());
            op_noad.nucleus = Some(Box::new(NoadField::SubBox { list_node: x }));
        } else {
            delta = 0;
        }
    } else {
        delta = 0;
    }
    if limit_type == LimitType::Limits {
        let list_node =
            construct_box_with_limits_above_and_below(op_noad, delta, style, scanner, eqtb, logger);
        vec![Node::List(list_node)]
    } else {
        convert_nucleus_to_hlist_and_attach_sub_superscripts(
            op_noad.nucleus,
            op_noad.superscript,
            op_noad.subscript,
            delta,
            style,
            scanner,
            eqtb,
            logger,
        )
    }
}

/// See 750.
fn construct_box_with_limits_above_and_below(
    op_noad: NormalNoad,
    delta: Scaled,
    style: MathStyle,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> ListNode {
    let superscript_exists = op_noad.superscript.is_some();
    let subscript_exists = op_noad.subscript.is_some();
    let mut x = if let Some(superscript) = op_noad.superscript {
        clean_box(*superscript, style.sup_style(), scanner, eqtb, logger)
    } else {
        ListNode::new_empty_hbox()
    };
    let mut y = if let Some(nucleus) = op_noad.nucleus {
        clean_box(*nucleus, style, scanner, eqtb, logger)
    } else {
        ListNode::new_empty_hbox()
    };
    let mut z = if let Some(subscript) = op_noad.subscript {
        clean_box(*subscript, style.sub_style(), scanner, eqtb, logger)
    } else {
        ListNode::new_empty_hbox()
    };
    let mut v = ListNode::new_empty_hbox();
    v.list = HlistOrVlist::Vlist(Vec::new());
    v.width = y.width;
    if x.width > v.width {
        v.width = x.width;
    }
    if z.width > v.width {
        v.width = z.width;
    }
    x = rebox(x, v.width, eqtb, logger);
    y = rebox(y, v.width, eqtb, logger);
    z = rebox(z, v.width, eqtb, logger);
    x.shift_amount = half(delta);
    z.shift_amount = -x.shift_amount;
    v.height = y.height;
    v.depth = y.depth;
    attach_limits_and_adjust_height_and_depth(
        superscript_exists,
        subscript_exists,
        x,
        y,
        z,
        &mut v,
        style,
        eqtb,
    );
    v
}

/// See 751.
fn attach_limits_and_adjust_height_and_depth(
    superscript_exists: bool,
    subscript_exists: bool,
    x: ListNode,
    y: ListNode,
    z: ListNode,
    v: &mut ListNode,
    style: MathStyle,
    eqtb: &Eqtb,
) {
    let mut vlist;
    if !superscript_exists {
        vlist = vec![Node::List(y)];
    } else {
        let mut shift_up = eqtb.big_op_spacing3(style.cur_size()) - x.depth;
        if shift_up < eqtb.big_op_spacing1(style.cur_size()) {
            shift_up = eqtb.big_op_spacing1(style.cur_size());
        }
        let sup_kern = KernNode::new(shift_up);
        let top_kern = KernNode::new(eqtb.big_op_spacing5(style.cur_size()));
        v.height += eqtb.big_op_spacing5(style.cur_size()) + x.height + x.depth + shift_up;
        vlist = vec![
            Node::Kern(top_kern),
            Node::List(x),
            Node::Kern(sup_kern),
            Node::List(y),
        ];
    }
    if subscript_exists {
        let mut shift_down = eqtb.big_op_spacing4(style.cur_size()) - z.height;
        if shift_down < eqtb.big_op_spacing2(style.cur_size()) {
            shift_down = eqtb.big_op_spacing2(style.cur_size());
        }
        let sub_kern = KernNode::new(shift_down);
        let bottom_kern = KernNode::new(eqtb.big_op_spacing5(style.cur_size()));
        v.depth += eqtb.big_op_spacing5(style.cur_size()) + z.height + z.depth + shift_down;
        vlist.push(Node::Kern(sub_kern));
        vlist.push(Node::List(z));
        vlist.push(Node::Kern(bottom_kern));
    }
    v.list = HlistOrVlist::Vlist(vlist);
}

/// See 752.
fn make_ord(
    mut ord_noad: NormalNoad,
    mlist_stack: &mut Vec<Node>,
    style: MathStyle,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Vec<Node> {
    loop {
        if let (None, None) = (&ord_noad.subscript, &ord_noad.superscript) {
        } else {
            break;
        }
        let (cur_fam, cur_char) = match &ord_noad.nucleus {
            Some(noad_field) => match noad_field.as_ref() {
                &NoadField::MathChar { fam, character } => (fam, character),
                _ => break,
            },
            _ => break,
        };

        let next_node = match mlist_stack.pop() {
            Some(node) => node,
            _ => break,
        };
        let next_noad = match next_node {
            Node::Noad(Noad::Normal(noad @ NormalNoad { noad_type, .. }))
                if noad_type != NoadType::Inner =>
            {
                noad
            }
            _ => {
                mlist_stack.push(next_node);
                break;
            }
        };
        let next_char = match &next_noad.nucleus {
            Some(noad_field) => match noad_field.as_ref() {
                &NoadField::MathChar { fam, character } if fam == cur_fam => character,
                _ => {
                    mlist_stack.push(Node::Noad(Noad::Normal(next_noad)));
                    break;
                }
            },
            _ => {
                mlist_stack.push(Node::Noad(Noad::Normal(next_noad)));
                break;
            }
        };
        let (cur_font_index, cur_c) =
            match fetch(cur_fam, cur_char, style.cur_size(), scanner, eqtb, logger) {
                Some(font_index) => (font_index, cur_char),
                None => {
                    ord_noad.nucleus = None;
                    mlist_stack.push(Node::Noad(Noad::Normal(next_noad)));
                    break;
                }
            };
        ord_noad.nucleus = Some(Box::new(NoadField::MathTextChar {
            fam: cur_fam,
            character: cur_char,
        }));
        let _lig_kern = match eqtb.fonts[cur_font_index as usize].tag(cur_c) {
            CharTag::Lig(lig_index) => &eqtb.fonts[cur_font_index as usize].lig_kerns[lig_index],
            _ => {
                mlist_stack.push(Node::Noad(Noad::Normal(next_noad)));
                break;
            }
        };

        // NOTE: We cannot pass lig_kern further down as long as we also need to pass eqtb.
        match if_instruction_is_kern(
            cur_font_index,
            cur_c,
            &mut ord_noad,
            next_noad,
            cur_fam,
            next_char,
            mlist_stack,
            scanner,
            eqtb,
            logger,
        ) {
            true => break,
            false => continue,
        }
    }
    convert_nucleus_to_hlist_and_attach_sub_superscripts(
        ord_noad.nucleus,
        ord_noad.superscript,
        ord_noad.subscript,
        0,
        style,
        scanner,
        eqtb,
        logger,
    )
}

/// Returns true for return, false for continue.
/// See 753.
fn if_instruction_is_kern(
    cur_font_index: FontIndex,
    cur_c: u8,
    cur_noad: &mut NormalNoad,
    mut next_noad: NormalNoad,
    fam: u8,
    next_char: u8,
    mlist_stack: &mut Vec<Node>,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> bool {
    let CharTag::Lig(lig_index) = eqtb.fonts[cur_font_index as usize].tag(cur_c) else {
        panic!("This should be the only case here");
    };
    let lig_kern = &eqtb.fonts[cur_font_index as usize].lig_kerns[lig_index];
    match lig_kern.get(&next_char) {
        Some(&LigKernCommand::Kern(w)) => {
            mlist_stack.push(Node::Noad(Noad::Normal(next_noad)));
            mlist_stack.push(Node::Kern(KernNode::new(w)));
            true
        }
        Some(&LigKernCommand::Lig {
            ligature,
            delete_left,
            delete_right,
            moves,
        }) => {
            // Allow a user to interrupt in case of an infinite ligature loop.
            logger.check_interrupt(scanner, eqtb);
            match (delete_left, delete_right) {
                // :=|, :=|>
                (true, false) => {
                    cur_noad.nucleus = Some(Box::new(NoadField::MathTextChar {
                        fam,
                        character: ligature,
                    }));
                    mlist_stack.push(Node::Noad(Noad::Normal(next_noad)));
                }
                // |:=, |:=>
                (false, true) => {
                    next_noad.nucleus = Some(Box::new(NoadField::MathChar {
                        fam,
                        character: ligature,
                    }));
                    mlist_stack.push(Node::Noad(Noad::Normal(next_noad)));
                }
                // |:=|, |:=|>, |:=|>>
                (false, false) => {
                    mlist_stack.push(Node::Noad(Noad::Normal(next_noad)));
                    let nucleus = if moves < 2 {
                        Some(Box::new(NoadField::MathChar {
                            fam,
                            character: ligature,
                        }))
                    } else {
                        Some(Box::new(NoadField::MathTextChar {
                            fam,
                            character: ligature,
                        }))
                    };
                    let inserted_noad = NormalNoad {
                        noad_type: NoadType::Ord,
                        nucleus,
                        subscript: None,
                        superscript: None,
                    };
                    mlist_stack.push(Node::Noad(Noad::Normal(inserted_noad)));
                }
                (true, true) => {
                    // Preserve scripts.
                    cur_noad.subscript = next_noad.subscript;
                    cur_noad.superscript = next_noad.superscript;

                    cur_noad.nucleus = Some(Box::new(NoadField::MathTextChar {
                        fam,
                        character: ligature,
                    }));
                }
            }
            if moves > 0 {
                return true;
            }

            let cur_char = match &cur_noad.nucleus {
                Some(noad_field) => match noad_field.as_ref() {
                    &NoadField::MathTextChar { character, .. } => character,
                    _ => panic!("Impossible"),
                },
                _ => panic!("Impossible"),
            };
            cur_noad.nucleus = Some(Box::new(NoadField::MathChar {
                fam,
                character: cur_char,
            }));
            false
        }
        None => {
            mlist_stack.push(Node::Noad(Noad::Normal(next_noad)));
            true
        }
    }
}

/// See 754.
fn convert_nucleus_to_hlist_and_attach_sub_superscripts(
    nucleus: Option<Box<NoadField>>,
    superscript: Option<Box<NoadField>>,
    subscript: Option<Box<NoadField>>,
    mut delta: Scaled,
    style: MathStyle,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Vec<Node> {
    let hlist = if let Some(noad_field) = nucleus {
        match *noad_field {
            NoadField::MathChar { fam, character } => create_character_node_for_nucleus(
                fam, character, &mut delta, false, &subscript, style, scanner, eqtb, logger,
            ),
            NoadField::MathTextChar { fam, character } => create_character_node_for_nucleus(
                fam, character, &mut delta, true, &subscript, style, scanner, eqtb, logger,
            ),
            NoadField::SubBox { list_node } => vec![Node::List(list_node)],
            NoadField::SubMlist { list: mlist } => {
                let hlist = mlist_to_hlist(mlist, style, false, scanner, eqtb, logger);
                let hbox = hpack(hlist, TargetSpec::Natural, eqtb, logger);
                vec![Node::List(hbox)]
            }
        }
    } else {
        Vec::new()
    };
    if let (None, None) = (&superscript, &subscript) {
        hlist
    } else {
        make_scripts(
            hlist,
            superscript,
            subscript,
            delta,
            style,
            scanner,
            eqtb,
            logger,
        )
    }
}

/// See 755.
fn create_character_node_for_nucleus(
    fam: u8,
    character: u8,
    delta: &mut Scaled,
    is_math_text_char: bool,
    subscript: &Option<Box<NoadField>>,
    style: MathStyle,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Vec<Node> {
    if let Some(font_index) = fetch(fam, character, style.cur_size(), scanner, eqtb, logger) {
        *delta = eqtb.fonts[font_index as usize].italic(character);
        let mut list = Vec::new();
        if let Some(char_node) = new_character(font_index, character, eqtb, logger) {
            list.push(Node::Char(char_node));
        }
        // We test to see if the font is a math font by looking at the size of the space.
        if is_math_text_char && eqtb.fonts[font_index as usize].space() != 0 {
            *delta = 0;
        }
        if *delta != 0 && subscript.is_none() {
            list.push(Node::Kern(KernNode::new(*delta)));
            *delta = 0;
        }
        list
    } else {
        Vec::new()
    }
}

/// Assumes that at least one of superscript and subscript is non-empty.
/// See 756.
fn make_scripts(
    mut hlist: Vec<Node>,
    superscript: Option<Box<NoadField>>,
    subscript: Option<Box<NoadField>>,
    delta: Scaled,
    style: MathStyle,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Vec<Node> {
    let mut shift_up;
    let shift_down;
    if let Some(Node::Char(_)) = hlist.first() {
        shift_up = 0;
        shift_down = 0;
    } else {
        let measurement = measure_hlist(&hlist);
        let t = if let MathStyle::Display { .. } | MathStyle::Text { .. } = style {
            MathFontSize::Script
        } else {
            MathFontSize::ScriptScript
        };
        shift_up = measurement.height - eqtb.sup_drop(t);
        shift_down = measurement.depth + eqtb.sub_drop(t);
    }

    let sub_sup_box = if let Some(superscript) = superscript {
        let mut supscr_box =
            construct_superscript_box(*superscript, &mut shift_up, style, scanner, eqtb, logger);
        if let Some(subscript) = subscript {
            construct_subscript_superscript_combination(
                *subscript, supscr_box, shift_up, shift_down, delta, style, scanner, eqtb, logger,
            )
        } else {
            supscr_box.shift_amount = -shift_up;
            supscr_box
        }
    } else {
        construct_only_subscript_box(
            *subscript.expect("We expect subscript to be non-empty if superscript is empty"),
            shift_down,
            style,
            scanner,
            eqtb,
            logger,
        )
    };
    hlist.push(Node::List(sub_sup_box));
    hlist
}

/// See 757.
fn construct_only_subscript_box(
    subscr: NoadField,
    mut shift_down: Scaled,
    style: MathStyle,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> ListNode {
    let mut x = clean_box(subscr, style.sub_style(), scanner, eqtb, logger);
    x.width += eqtb.dimen(DimensionVariable::ScriptSpace);
    if shift_down < eqtb.sub1(style.cur_size()) {
        shift_down = eqtb.sub1(style.cur_size());
    }
    let clr = x.height - (eqtb.math_x_height(style.cur_size()) * 4).abs() / 5;
    if shift_down < clr {
        shift_down = clr;
    }
    x.shift_amount = shift_down;
    x
}

/// See 758.
fn construct_superscript_box(
    supscr: NoadField,
    shift_up: &mut Scaled,
    style: MathStyle,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> ListNode {
    let mut x = clean_box(supscr, style.sup_style(), scanner, eqtb, logger);
    x.width += eqtb.dimen(DimensionVariable::ScriptSpace);
    let mut clr;
    match style {
        MathStyle::Display { cramped } => {
            if cramped {
                clr = eqtb.sup3(style.cur_size());
            } else {
                clr = eqtb.sup1(style.cur_size());
            }
        }
        MathStyle::Text { cramped }
        | MathStyle::Script { cramped }
        | MathStyle::ScriptScript { cramped } => {
            if cramped {
                clr = eqtb.sup3(style.cur_size());
            } else {
                clr = eqtb.sup2(style.cur_size());
            }
        }
    }
    if *shift_up < clr {
        *shift_up = clr;
    }
    clr = x.depth + eqtb.math_x_height(style.cur_size()).abs() / 4;
    if *shift_up < clr {
        *shift_up = clr;
    }
    x
}

/// See 759.
fn construct_subscript_superscript_combination(
    subscript: NoadField,
    mut supscr_box: ListNode,
    mut shift_up: Scaled,
    mut shift_down: Scaled,
    delta: Scaled,
    style: MathStyle,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> ListNode {
    let mut y = clean_box(subscript, style.sub_style(), scanner, eqtb, logger);
    y.width += eqtb.dimen(DimensionVariable::ScriptSpace);
    if shift_down < eqtb.sub2(style.cur_size()) {
        shift_down = eqtb.sub2(style.cur_size());
    }
    let clr = 4 * eqtb.default_rule_thickness(style.cur_size())
        - ((shift_up - supscr_box.depth) - (y.height - shift_down));
    if clr > 0 {
        shift_down += clr;
        let clr =
            (eqtb.math_x_height(style.cur_size()) * 4).abs() / 5 - (shift_up - supscr_box.depth);
        if clr > 0 {
            shift_up += clr;
            shift_down -= clr;
        }
    }
    supscr_box.shift_amount = delta;
    let p = KernNode::new((shift_up - supscr_box.depth) - (y.height - shift_down));
    let vlist = vec![Node::List(supscr_box), Node::Kern(p), Node::List(y)];
    let mut vbox = vpack(vlist, TargetSpec::Natural, eqtb, logger);
    vbox.shift_amount = shift_down;
    vbox
}

/// See 760., 761. and 767.
fn make_second_pass_over_mlist(
    nodes_or_hlists: Vec<FirstPassNode>,
    orig_style: MathStyle,
    max_d: Scaled,
    max_h: Scaled,
    penalties: bool,
    eqtb: &Eqtb,
) -> Vec<Node> {
    let mut hlist = Vec::new();
    let mut r_type = None;
    let mut cur_style = orig_style;
    let mut pen = INF_PENALTY;
    for elem in nodes_or_hlists {
        if penalties {
            // First push a penalty if a previous Rel or Bin noad has set one.
            if pen < INF_PENALTY {
                match elem {
                    // Let's not insert a penalty before an already existing penalty or before RelNoad.
                    FirstPassNode::DoneNode(Node::Penalty(_))
                    | FirstPassNode::DoneNoad(_, NoadType::Rel) => {}
                    _ => {
                        let z = PenaltyNode::new(pen);
                        hlist.push(Node::Penalty(z));
                    }
                }
            }
            pen = INF_PENALTY
        }
        match elem {
            FirstPassNode::StyleNode(style_node) => {
                change_current_style(style_node, &mut cur_style);
            }
            FirstPassNode::Left(left_noad) => {
                let noad_type = NoadType::Open;
                append_inter_element_spacing(r_type, noad_type, &mut hlist, cur_style, eqtb);
                let left = make_left_right(left_noad.delimiter, orig_style, max_d, max_h, eqtb);
                hlist.push(Node::List(left));
                r_type = Some(noad_type);
            }
            FirstPassNode::Right(right_noad) => {
                let noad_type = NoadType::Close;
                append_inter_element_spacing(r_type, noad_type, &mut hlist, cur_style, eqtb);
                let right = make_left_right(right_noad.delimiter, orig_style, max_d, max_h, eqtb);
                hlist.push(Node::List(right));
                r_type = Some(noad_type);
            }
            FirstPassNode::DoneNoad(new_hlist, noad_type) => {
                append_inter_element_spacing(r_type, noad_type, &mut hlist, cur_style, eqtb);
                for node in new_hlist {
                    hlist.push(node);
                }
                // Set a penalty to be potentially added before the next node.
                pen = match noad_type {
                    NoadType::Rel => eqtb.integer(IntegerVariable::RelPenalty),
                    NoadType::Bin => eqtb.integer(IntegerVariable::BinOpPenalty),
                    _ => INF_PENALTY,
                };
                r_type = Some(noad_type);
            }
            FirstPassNode::DoneNode(node) => {
                hlist.push(node);
            }
        }
    }
    hlist
}

/// See 762.
fn make_left_right(
    delimiter: DelimiterField,
    orig_style: MathStyle,
    max_d: Scaled,
    max_h: Scaled,
    eqtb: &Eqtb,
) -> ListNode {
    let mut delta2 = max_d + eqtb.axis_height(orig_style.cur_size());
    let mut delta1 = max_h + max_d - delta2;
    if delta2 > delta1 {
        delta1 = delta2;
    }
    let mut delta = (delta1 / 500) * eqtb.integer(IntegerVariable::DelimiterFactor);
    delta2 = delta1 + delta1 - eqtb.dimen(DimensionVariable::DelimiterShortfall);
    if delta < delta2 {
        delta = delta2;
    }
    var_delimiter(delimiter, orig_style.cur_size(), delta, eqtb)
}

/// See 763.
fn change_current_style(style_node: StyleNode, cur_style: &mut MathStyle) {
    *cur_style = style_node.style;
}

/// See 764.
enum MathSpacing {
    NoSpace,
    TextThinSpace,
    ThinSpace,
    TextMediumSpace,
    TextThickSpace,
    Invalid,
}

/// This function describes what spacing to use between different Noad types.
/// See 764.
const fn inter_element_space(left: NoadType, right: NoadType) -> MathSpacing {
    use MathSpacing::*;
    use NoadType::*;
    match left {
        Ord => match right {
            Ord | Open | Close | Punct => NoSpace,
            Op { .. } => ThinSpace,
            Bin => TextMediumSpace,
            Rel => TextThickSpace,
            Inner => TextThinSpace,
        },
        Op { .. } => match right {
            Ord | Op { .. } => ThinSpace,
            Bin => Invalid,
            Rel => TextThickSpace,
            Open | Close | Punct => NoSpace,
            Inner => TextThinSpace,
        },
        Bin => match right {
            Ord | Op { .. } | Open | Inner => TextMediumSpace,
            Bin | Rel | Close | Punct => Invalid,
        },
        Rel => match right {
            Ord | Op { .. } | Open | Inner => TextThickSpace,
            Bin => Invalid,
            Rel | Close | Punct => NoSpace,
        },
        Open => match right {
            Ord | Op { .. } | Rel | Open | Close | Punct | Inner => NoSpace,
            Bin => Invalid,
        },
        Close => match right {
            Ord | Open | Close | Punct => NoSpace,
            Op { .. } => ThinSpace,
            Bin => TextMediumSpace,
            Rel => TextThickSpace,
            Inner => TextThinSpace,
        },
        Punct => match right {
            Ord | Op { .. } | Rel | Open | Close | Punct | Inner => TextThinSpace,
            Bin => Invalid,
        },
        Inner => match right {
            Ord | Open | Punct | Inner => TextThinSpace,
            Op { .. } => ThinSpace,
            Bin => TextMediumSpace,
            Rel => TextThickSpace,
            Close => NoSpace,
        },
    }
}

/// See 766.
fn append_inter_element_spacing(
    r_type: Option<NoadType>,
    t: NoadType,
    hlist: &mut Vec<Node>,
    style: MathStyle,
    eqtb: &Eqtb,
) {
    if let Some(r_type) = r_type {
        let x: Option<SkipVariable>;
        // See 765. for the negative term.
        match inter_element_space(r_type, t) {
            MathSpacing::NoSpace => {
                x = None;
            }
            MathSpacing::TextThinSpace => {
                if let MathStyle::Display { .. } | MathStyle::Text { .. } = style {
                    x = Some(SkipVariable::ThinMuSkip);
                } else {
                    x = None;
                }
            }
            MathSpacing::ThinSpace => {
                x = Some(SkipVariable::ThinMuSkip);
            }
            MathSpacing::TextMediumSpace => {
                if let MathStyle::Display { .. } | MathStyle::Text { .. } = style {
                    x = Some(SkipVariable::MedMuSkip);
                } else {
                    x = None;
                }
            }
            MathSpacing::TextThickSpace => {
                if let MathStyle::Display { .. } | MathStyle::Text { .. } = style {
                    x = Some(SkipVariable::ThickMuSkip);
                } else {
                    x = None;
                }
            }
            MathSpacing::Invalid => panic!("This combination should be impossible"),
        }
        if let Some(mu_skip_var) = x {
            let mu_skip = eqtb.skips.get(mu_skip_var);
            let y = std::rc::Rc::new(math_glue(mu_skip, style.cur_mu(eqtb)));
            let mut z = GlueNode::new(y);
            z.subtype = GlueType::Skip(mu_skip_var);
            hlist.push(Node::Glue(z));
        }
    }
}

/// See 1138.
pub fn init_math(
    hmode_type: HorizontalModeType,
    hyphenator: &mut Hyphenator,
    page_builder: &mut PageBuilder,
    output: &mut Output,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let (command, token) = scanner.get_command_and_token(eqtb, logger);
    // Note: we only go into DisplayMath if we are not in RestrictedHorizontal
    match hmode_type {
        HorizontalModeType::Restricted => {
            scanner.back_input(token, eqtb, logger);
            go_into_ordinary_math_mode(MathShiftKind::Inline, nest, scanner, eqtb, logger);
        }
        HorizontalModeType::Unrestricted { lang_data, .. } => {
            if let Command::Unexpandable(UnexpandableCommand::MathShift(_)) = command {
                go_into_display_math_mode(
                    &lang_data,
                    hyphenator,
                    page_builder,
                    output,
                    nest,
                    scanner,
                    eqtb,
                    logger,
                );
            } else {
                scanner.back_input(token, eqtb, logger);
                go_into_ordinary_math_mode(MathShiftKind::Inline, nest, scanner, eqtb, logger);
            }
        }
    }
}

/// See 1139.
fn go_into_ordinary_math_mode(
    typ: MathShiftKind,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    push_math(
        GroupType::MathShift { kind: typ },
        nest,
        &scanner.input_stack,
        eqtb,
        logger,
    );
    eqtb.int_define(IntegerVariable::CurFam, -1, false, logger);
    if let Some(every_math) = eqtb.token_lists.get(TokenListVariable::EveryMath) {
        scanner.input_stack.begin_token_list(
            every_math.clone(),
            TokenSourceType::EveryMathText,
            eqtb,
            logger,
        );
    }
}

/// See 1145.
fn go_into_display_math_mode(
    lang_data: &LanguageData,
    hyphenator: &mut Hyphenator,
    page_builder: &mut PageBuilder,
    output: &mut Output,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let pre_display_size;
    if nest.cur_list_is_empty() {
        nest.pop_nest(eqtb);
        pre_display_size = -MAX_DIMEN;
    } else {
        let display_widow_penalty = *eqtb.integers.get(IntegerVariable::DisplayWidowPenalty);
        let old_level = nest.pop_nest(eqtb);
        let RichMode::Horizontal(hmode) = old_level.mode else {
            panic!("We expect the mode to be horizontal when entering display math");
        };
        pre_display_size = line_break(
            hmode.list,
            lang_data,
            old_level.mode_line,
            display_widow_penalty,
            true,
            hyphenator,
            nest,
            scanner,
            eqtb,
            logger,
        );
    }
    let (l, s) = calculate_length_and_shift_amount(nest, eqtb);
    nest.push_nest(
        RichMode::Math(MathMode {
            list: Vec::new(),
            display: true,
            incompleat_noad: None,
        }),
        &scanner.input_stack,
        eqtb,
        logger,
    );
    eqtb.new_save_level(
        GroupType::MathShift {
            kind: MathShiftKind::Display,
        },
        &scanner.input_stack,
        logger,
    );
    eqtb.int_define(IntegerVariable::CurFam, -1, false, logger);
    eqtb.dimen_define(DimensionVariable::PreDisplaySize, pre_display_size, false);
    eqtb.dimen_define(DimensionVariable::DisplayWidth, l, false);
    eqtb.dimen_define(DimensionVariable::DisplayIndent, s, false);
    if let Some(every_display) = eqtb.token_lists.get(TokenListVariable::EveryDisplay) {
        scanner.input_stack.begin_token_list(
            every_display.clone(),
            TokenSourceType::EveryDisplayText,
            eqtb,
            logger,
        );
    }
    if nest.depth() == 1 {
        page_builder.build_page(output, nest, scanner, eqtb, logger);
    }
}

/// See 1149.
fn calculate_length_and_shift_amount(nest: &SemanticState, eqtb: &Eqtb) -> (Dimension, Dimension) {
    let RichMode::Vertical(vmode) = nest.mode() else {
        panic!("We expect to be in vertical mode here")
    };
    let par_shape = eqtb.par_shape.get(ParShapeVariable);
    let l;
    let s;
    if par_shape.is_empty() {
        let hang_indent = *eqtb.dimensions.get(DimensionVariable::HangIndent);
        let hang_after = *eqtb.integers.get(IntegerVariable::HangAfter);
        let hsize = *eqtb.dimensions.get(DimensionVariable::Hsize);
        if hang_indent != 0
            && ((hang_after >= 0 && vmode.prev_graf + 2 > hang_after)
                || vmode.prev_graf + 1 < -hang_after)
        {
            l = hsize - hang_indent.abs();
            if hang_indent > 0 {
                s = hang_indent;
            } else {
                s = 0;
            }
        } else {
            l = hsize;
            s = 0;
        }
    } else {
        let n = par_shape.len();
        if vmode.prev_graf as usize + 2 >= n {
            s = par_shape[n - 1].0;
            l = par_shape[n - 1].1;
        } else {
            s = par_shape[vmode.prev_graf as usize + 2 - 1].0;
            l = par_shape[vmode.prev_graf as usize + 2 - 1].1;
        }
    }
    (l, s)
}

/// See 1142.
pub fn start_eq_no(
    is_left: bool,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    if is_left {
        go_into_ordinary_math_mode(MathShiftKind::LeftEqNo, nest, scanner, eqtb, logger)
    } else {
        go_into_ordinary_math_mode(MathShiftKind::RightEqNo, nest, scanner, eqtb, logger)
    }
}

/// See 1194.
pub fn after_math(
    kind: MathShiftKind,
    hyphenator: &mut Hyphenator,
    page_builder: &mut PageBuilder,
    output: &mut Output,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let RichMode::Math(mmode) = nest.mode_mut() else {
        panic!("We must be in math mode here");
    };
    let mut danger = false;
    check_necessary_fonts_for_math(mmode, &mut danger, scanner, eqtb, logger);
    let mut m = eqtb.mode();

    let old_level = nest.pop_nest(eqtb);
    let RichMode::Math(mmode) = old_level.mode else {
        panic!("We must be in math mode here");
    };
    let mut mlist = mmode.fin_mlist(None);

    let eqno;
    if let MathShiftKind::LeftEqNo | MathShiftKind::RightEqNo = kind {
        check_that_another_mathshift_symbol_follows(scanner, eqtb, logger);
        let hlist = mlist_to_hlist(
            mlist,
            MathStyle::Text { cramped: false },
            false,
            scanner,
            eqtb,
            logger,
        );
        eqno = Some(hpack(hlist, TargetSpec::Natural, eqtb, logger));
        eqtb.unsave(scanner, logger);
        danger = false;
        let RichMode::Math(mmode) = nest.mode_mut() else {
            panic!("We expect to be in math mode here");
        };
        check_necessary_fonts_for_math(mmode, &mut danger, scanner, eqtb, logger);
        m = eqtb.mode();

        let old_level = nest.pop_nest(eqtb);
        let RichMode::Math(mmode) = old_level.mode else {
            panic!("We must be in math mode here");
        };
        mlist = mmode.fin_mlist(None);
    } else {
        eqno = None;
    }
    if let Mode::Math = m {
        finish_math_in_text(mlist, nest, scanner, eqtb, logger);
    } else {
        if eqno.is_none() {
            check_that_another_mathshift_symbol_follows(scanner, eqtb, logger);
        }
        finish_displayed_math(
            mlist,
            eqno,
            danger,
            kind,
            hyphenator,
            page_builder,
            output,
            nest,
            scanner,
            eqtb,
            logger,
        );
    }
}

/// See 1195.
fn check_necessary_fonts_for_math(
    mmode: &mut MathMode,
    danger: &mut bool,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    if eqtb.fonts[eqtb.fam_fnt(MathFontSize::Text, 2) as usize]
        .params
        .len()
        < TOTAL_MATHSY_PARAMS
        || eqtb.fonts[eqtb.fam_fnt(MathFontSize::Script, 2) as usize]
            .params
            .len()
            < TOTAL_MATHSY_PARAMS
        || eqtb.fonts[eqtb.fam_fnt(MathFontSize::ScriptScript, 2) as usize]
            .params
            .len()
            < TOTAL_MATHSY_PARAMS
    {
        logger.print_err("Math formula deleted: Insufficient symbol fonts");
        let help = &[
            "Sorry, but I can't typeset math unless \\textfont 2",
            "and \\scriptfont 2 and \\scriptscriptfont 2 have all",
            "the \\fontdimen values needed in math symbol fonts.",
        ];
        logger.error(help, scanner, eqtb);
        mmode.flush_math(eqtb);
        *danger = true;
    } else if eqtb.fonts[eqtb.fam_fnt(MathFontSize::Text, 3) as usize]
        .params
        .len()
        < TOTAL_MATHEX_PARAMS
        || eqtb.fonts[eqtb.fam_fnt(MathFontSize::Script, 3) as usize]
            .params
            .len()
            < TOTAL_MATHEX_PARAMS
        || eqtb.fonts[eqtb.fam_fnt(MathFontSize::ScriptScript, 3) as usize]
            .params
            .len()
            < TOTAL_MATHEX_PARAMS
    {
        logger.print_err("Math formula deleted: Insufficient extension fonts");
        let help = &[
            "Sorry, but I can't typeset math unless \\textfont 3",
            "and \\scriptfont 3 and \\scriptscriptfont 3 have all",
            "the \\fontdimen values needed in math extension fonts.",
        ];
        logger.error(help, scanner, eqtb);
        mmode.flush_math(eqtb);
        *danger = true;
    }
}

/// See 1196.
fn finish_math_in_text(
    mlist: Vec<Node>,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let math_surround = *eqtb.dimensions.get(DimensionVariable::MathSurround);
    nest.tail_push(
        Node::Math(MathNode {
            kind: MathNodeKind::Before,
            width: math_surround,
        }),
        eqtb,
    );
    let penalties = if let Mode::Vertical | Mode::Horizontal | Mode::DisplayMath = eqtb.mode() {
        true
    } else {
        false
    };
    let hlist = mlist_to_hlist(
        mlist,
        MathStyle::Text { cramped: false },
        penalties,
        scanner,
        eqtb,
        logger,
    );
    nest.tail_append(hlist, eqtb);
    nest.tail_push(
        Node::Math(MathNode {
            kind: MathNodeKind::After,
            width: math_surround,
        }),
        eqtb,
    );
    let RichMode::Horizontal(hmode) = nest.mode_mut() else {
        panic!("We expect the mode to be horizontal after a Math mode finished");
    };
    hmode.set_space_factor(1000, eqtb);

    eqtb.unsave(scanner, logger);
}

/// See 1197.
pub fn check_that_another_mathshift_symbol_follows(
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    // NOTE We use expansion here to get the next MathShift as this is what TeX82 does.
    let (unexpandable_command, token) = get_x_token(scanner, eqtb, logger);
    if let UnexpandableCommand::MathShift(_) = unexpandable_command {
        // All is good.
    } else {
        logger.print_err("Display math should end with $$");
        let help = &[
            "The `$' that I just saw supposedly matches a previous `$$'.",
            "So I shall assume that you typed `$$' both times.",
        ];
        scanner.back_error(token, help, eqtb, logger);
    }
}

/// See 1199.
fn finish_displayed_math(
    mlist: Vec<Node>,
    eqno: Option<ListNode>,
    danger: bool,
    kind: MathShiftKind,
    hyphenator: &mut Hyphenator,
    page_builder: &mut PageBuilder,
    output: &mut Output,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let mut formula = mlist_to_hlist(
        mlist,
        MathStyle::Display { cramped: false },
        false,
        scanner,
        eqtb,
        logger,
    );
    let has_glue_first = if let Some(Node::Glue(_)) = formula.first() {
        true
    } else {
        false
    };
    let adjustment_material = split_adjust_material_off(&mut formula);
    let formula_measurement = measure_hlist(&formula);
    let mut w = formula_measurement.width;
    let display_width = *eqtb.dimensions.get(DimensionVariable::DisplayWidth);
    let display_indent = *eqtb.dimensions.get(DimensionVariable::DisplayIndent);
    let z = display_width;
    let s = display_indent;
    let mut e;
    let q;
    match &eqno {
        Some(eqno) if !danger => {
            e = eqno.width;
            q = e + eqtb.math_quad(MathFontSize::Text);
        }
        _ => {
            e = 0;
            q = 0;
        }
    }
    let packed_formula = if w + q > z {
        squeeze_equation_as_much_as_possible(
            &mut w,
            &mut e,
            q,
            z,
            formula,
            formula_measurement,
            eqtb,
            logger,
        )
    } else {
        hpack(formula, TargetSpec::Natural, eqtb, logger)
    };
    let mut d = determine_displacement(z, w, e, has_glue_first);
    let g2 = append_glue_or_equation_number_preceding(d, s, e, kind, &eqno, nest, eqtb);
    append_display_and_perhaps_equation_number(
        z,
        w,
        e,
        s,
        &mut d,
        kind,
        &eqno,
        packed_formula,
        nest,
        eqtb,
        logger,
    );
    append_glue_or_equation_number_following(
        s,
        z,
        eqno,
        e,
        kind,
        g2,
        adjustment_material,
        nest,
        eqtb,
    );
    resume_after_display(
        hyphenator,
        page_builder,
        output,
        nest,
        scanner,
        eqtb,
        logger,
    );
}

/// See 1200.
pub fn resume_after_display(
    hyphenator: &mut Hyphenator,
    page_builder: &mut PageBuilder,
    output: &mut Output,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    if let GroupType::MathShift { .. } = eqtb.cur_group.typ {
        // All good.
    } else {
        panic!("We expect to always be in a MathShift group here");
    }
    let RichMode::Vertical(vmode) = nest.mode_mut() else {
        panic!("We expect to be in vertical mode here")
    };
    eqtb.unsave(scanner, logger);

    vmode.increment_prev_graf(3, eqtb);

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
    scanner.scan_optional_space(eqtb, logger);
    if nest.depth() == 1 {
        page_builder.build_page(output, nest, scanner, eqtb, logger);
    }
}

/// See 1201.
fn squeeze_equation_as_much_as_possible(
    w: &mut Dimension,
    e: &mut Dimension,
    q: Dimension,
    z: Dimension,
    formula: Vec<Node>,
    formula_measurement: Measurement,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> ListNode {
    let packed_formula = if *e != 0 {
        match formula_measurement.shrink.order {
            DimensionOrder::Normal if *w - formula_measurement.shrink.value + q > z => {
                *e = 0;
                if *w > z {
                    hpack(formula, TargetSpec::Exactly(z), eqtb, logger)
                } else {
                    hpack(formula, TargetSpec::Natural, eqtb, logger)
                }
            }
            _ => hpack(formula, TargetSpec::Exactly(z - q), eqtb, logger),
        }
    } else if *w > z {
        hpack(formula, TargetSpec::Exactly(z), eqtb, logger)
    } else {
        hpack(formula, TargetSpec::Natural, eqtb, logger)
    };
    *w = packed_formula.width;
    packed_formula
}

/// See 1202.
fn determine_displacement(
    z: Dimension,
    w: Dimension,
    e: Dimension,
    has_glue_first: bool,
) -> Dimension {
    let mut d = half(z - w);
    if e > 0 && d < 2 * e {
        d = half(z - w - e);
        if has_glue_first {
            d = 0;
        }
    }
    d
}

/// See 1203.
fn append_glue_or_equation_number_preceding(
    d: Dimension,
    s: Dimension,
    e: Dimension,
    kind: MathShiftKind,
    eqno: &Option<ListNode>,
    nest: &mut SemanticState,
    eqtb: &mut Eqtb,
) -> SkipVariable {
    let pre_display_penalty = *eqtb.integers.get(IntegerVariable::PreDisplayPenalty);
    nest.tail_push(Node::Penalty(PenaltyNode::new(pre_display_penalty)), eqtb);
    let pre_display_size = *eqtb.dimensions.get(DimensionVariable::PreDisplaySize);
    let g1;
    let g2;
    if d + s <= pre_display_size || kind == MathShiftKind::LeftEqNo {
        g1 = SkipVariable::AboveDisplaySkip;
        g2 = SkipVariable::BelowDisplaySkip;
    } else {
        g1 = SkipVariable::AboveDisplayShortSkip;
        g2 = SkipVariable::BelowDisplayShortSkip;
    }
    if kind == MathShiftKind::LeftEqNo && e == 0 {
        let mut eqno = eqno.as_ref().unwrap().clone();
        eqno.shift_amount = s;
        let RichMode::Vertical(vmode) = nest.mode_mut() else {
            panic!("We expected to be in vertical mode here");
        };
        vmode.append_to_vlist(eqno, eqtb);
        nest.tail_push(Node::Penalty(PenaltyNode::new(INF_PENALTY)), eqtb);
    } else {
        nest.tail_push(Node::Glue(GlueNode::new_param(g1, eqtb)), eqtb);
    }
    g2
}

/// See 1204.
fn append_display_and_perhaps_equation_number(
    z: Dimension,
    w: Dimension,
    e: Dimension,
    s: Dimension,
    d: &mut Dimension,
    kind: MathShiftKind,
    eqno: &Option<ListNode>,
    packed_formula: ListNode,
    nest: &mut SemanticState,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let mut middle_line = if e != 0 {
        let kern_node = KernNode::new(z - w - e - *d);
        let eqno = eqno.as_ref().unwrap().clone();
        let hlist = if kind == MathShiftKind::LeftEqNo {
            *d = 0;
            vec![
                Node::List(eqno),
                Node::Kern(kern_node),
                Node::List(packed_formula),
            ]
        } else {
            vec![
                Node::List(packed_formula),
                Node::Kern(kern_node),
                Node::List(eqno),
            ]
        };
        hpack(hlist, TargetSpec::Natural, eqtb, logger)
    } else {
        packed_formula
    };
    middle_line.shift_amount = s + *d;
    let RichMode::Vertical(vmode) = nest.mode_mut() else {
        panic!("We expected to be in vertical mode here");
    };
    vmode.append_to_vlist(middle_line, eqtb);
}

/// See 1205
fn append_glue_or_equation_number_following(
    s: Dimension,
    z: Dimension,
    eqno: Option<ListNode>,
    e: Dimension,
    kind: MathShiftKind,
    g2: SkipVariable,
    adjustment_material: Vec<Node>,
    nest: &mut SemanticState,
    eqtb: &mut Eqtb,
) {
    let final_skip = if eqno.is_some() && e == 0 && kind != MathShiftKind::LeftEqNo {
        let mut eqno = eqno.unwrap();
        nest.tail_push(Node::Penalty(PenaltyNode::new(INF_PENALTY)), eqtb);
        eqno.shift_amount = s + z - eqno.width;
        let RichMode::Vertical(vmode) = nest.mode_mut() else {
            panic!("We expected to be in vertical mode here");
        };
        vmode.append_to_vlist(eqno, eqtb);
        None
    } else {
        Some(g2)
    };
    nest.tail_append(adjustment_material, eqtb);
    let post_display_penalty = *eqtb.integers.get(IntegerVariable::PostDisplayPenalty);
    nest.tail_push(Node::Penalty(PenaltyNode::new(post_display_penalty)), eqtb);
    if let Some(final_skip) = final_skip {
        nest.tail_push(Node::Glue(GlueNode::new_param(final_skip, eqtb)), eqtb);
    }
}

/// See 1181. and 1182.
pub fn math_fraction(
    mmode: &mut MathMode,
    fraction_command: FractionCommand,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    if mmode.incompleat_noad.is_some() {
        ignore_fraction_operation_and_complain(fraction_command, scanner, eqtb, logger);
    } else {
        // Extract numerator from the current list.
        let numerator = std::mem::take(&mut mmode.list);
        // NOTE We need to update the last node info here.
        mmode.update_last_node_info(eqtb);

        let left_delimiter;
        let right_delimiter;
        if fraction_command.is_delimited {
            left_delimiter = scan_delimiter(scanner, eqtb, logger);
            right_delimiter = scan_delimiter(scanner, eqtb, logger);
        } else {
            left_delimiter = DelimiterField::null_delimiter();
            right_delimiter = DelimiterField::null_delimiter();
        };

        let thickness = match fraction_command.typ {
            FractionType::Above => scan_normal_dimen(scanner, eqtb, logger),
            FractionType::Over => DEFAULT_CODE,
            FractionType::Atop => 0,
        };

        let fraction_noad = FractionNoad {
            numerator,
            denominator: Vec::new(),
            left_delimiter,
            right_delimiter,
            thickness,
        };
        mmode.incompleat_noad = Some(fraction_noad);
    }
}

/// See 1183.
fn ignore_fraction_operation_and_complain(
    fraction_command: FractionCommand,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    // Scan and drop the delimiters.
    if fraction_command.is_delimited {
        scan_delimiter(scanner, eqtb, logger);
        scan_delimiter(scanner, eqtb, logger);
    }
    // Scan and drop a thickness specification.
    if let FractionType::Above = fraction_command.typ {
        scan_normal_dimen(scanner, eqtb, logger);
    }
    logger.print_err("Ambiguous; you need another { and }");
    let help = &[
        "I'm ignoring this fraction specification, since I don't",
        "know whether a construction like `x \\over y \\over z'",
        "means `{x \\over y} \\over z' or `x \\over {y \\over z}'.",
    ];
    logger.error(help, scanner, eqtb);
}

/// See 1191.
pub fn math_left(
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let delimiter = scan_delimiter(scanner, eqtb, logger);
    let left_noad = LeftNoad { delimiter };
    push_math(
        GroupType::MathLeft,
        nest,
        &scanner.input_stack,
        eqtb,
        logger,
    );
    nest.tail_push(Node::Noad(Noad::Left(left_noad)), eqtb);
}

/// See 1191.
pub fn math_right(
    token: Token,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    if let GroupType::MathLeft = eqtb.cur_group.typ {
        let delimiter = scan_delimiter(scanner, eqtb, logger);
        let right_noad = RightNoad { delimiter };
        let old_level = nest.pop_nest(eqtb);
        let RichMode::Math(mmode) = old_level.mode else {
            panic!("We must be in math mode here");
        };
        let mlist = mmode.fin_mlist(Some(right_noad));
        eqtb.unsave(scanner, logger);
        let mut inner_noad = NormalNoad::new();
        inner_noad.noad_type = NoadType::Inner;
        inner_noad.nucleus = Some(Box::new(NoadField::SubMlist { list: mlist }));
        nest.tail_push(Node::Noad(Noad::Normal(inner_noad)), eqtb);
    } else {
        try_to_recover_from_mismatched_right(token, scanner, eqtb, logger);
    }
}

/// See 1192.
fn try_to_recover_from_mismatched_right(
    token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    if let GroupType::MathShift { .. } = eqtb.cur_group.typ {
        // Scan and discard delimiter.
        scan_delimiter(scanner, eqtb, logger);
        logger.print_err("Extra ");
        logger.print_esc_str(b"right");
        let help = &["I'm ignoring a \\right that had no matching \\left."];
        logger.error(help, scanner, eqtb);
    } else {
        off_save(
            UnexpandableCommand::Math(MathCommand::Right),
            token,
            scanner,
            eqtb,
            logger,
        )
    }
}

/// See 1176.
pub fn superscript(
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    if let Some(noad) = nest.last_noad_mut() {
        match noad {
            Noad::Normal(NormalNoad { superscript, .. })
            | Noad::Radical(RadicalNoad { superscript, .. })
            | Noad::Under(UnderNoad { superscript, .. })
            | Noad::Over(OverNoad { superscript, .. })
            | Noad::Accent(AccentNoad { superscript, .. })
            | Noad::Vcenter(VcenterNoad { superscript, .. }) => {
                if superscript.is_none() {
                    if let Some(noad_field) = scan_math_char(scanner, eqtb, logger) {
                        *superscript = Some(Box::new(noad_field));
                    } else {
                        scan_subformula_enclosed_in_braces(
                            SubformulaType::Superscript,
                            nest,
                            scanner,
                            eqtb,
                            logger,
                        );
                    }
                } else {
                    logger.print_err("Double superscript");
                    let help = &["I treat `x^1^2' essentially like `x^1{}^2'."];
                    logger.error(help, scanner, eqtb);

                    superscript_dummy_noad(nest, scanner, eqtb, logger)
                }
            }
            _ => superscript_dummy_noad(nest, scanner, eqtb, logger),
        }
    } else {
        superscript_dummy_noad(nest, scanner, eqtb, logger);
    }
}

/// See 1177.
fn superscript_dummy_noad(
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let mut noad = NormalNoad::new();
    if let Some(noad_field) = scan_math_char(scanner, eqtb, logger) {
        noad.superscript = Some(Box::new(noad_field));
        nest.tail_push(Node::Noad(Noad::Normal(noad)), eqtb);
    } else {
        nest.tail_push(Node::Noad(Noad::Normal(noad)), eqtb);
        scan_subformula_enclosed_in_braces(
            SubformulaType::Superscript,
            nest,
            scanner,
            eqtb,
            logger,
        );
    }
}

/// See 1176.
pub fn subscript(
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    if let Some(noad) = nest.last_noad_mut() {
        match noad {
            Noad::Normal(NormalNoad { subscript, .. })
            | Noad::Radical(RadicalNoad { subscript, .. })
            | Noad::Under(UnderNoad { subscript, .. })
            | Noad::Over(OverNoad { subscript, .. })
            | Noad::Accent(AccentNoad { subscript, .. })
            | Noad::Vcenter(VcenterNoad { subscript, .. }) => {
                if subscript.is_none() {
                    if let Some(noad_field) = scan_math_char(scanner, eqtb, logger) {
                        *subscript = Some(Box::new(noad_field));
                    } else {
                        scan_subformula_enclosed_in_braces(
                            SubformulaType::Subscript,
                            nest,
                            scanner,
                            eqtb,
                            logger,
                        );
                    }
                } else {
                    logger.print_err("Double subscript");
                    let help = &["I treat `x_1_2' essentially like `x_1{}_2'."];
                    logger.error(help, scanner, eqtb);

                    subscript_dummy_noad(nest, scanner, eqtb, logger)
                }
            }
            _ => subscript_dummy_noad(nest, scanner, eqtb, logger),
        }
    } else {
        subscript_dummy_noad(nest, scanner, eqtb, logger);
    }
}

/// See 1177.
fn subscript_dummy_noad(
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let mut noad = NormalNoad::new();
    if let Some(noad_field) = scan_math_char(scanner, eqtb, logger) {
        noad.subscript = Some(Box::new(noad_field));
        nest.tail_push(Node::Noad(Noad::Normal(noad)), eqtb);
    } else {
        nest.tail_push(Node::Noad(Noad::Normal(noad)), eqtb);
        scan_subformula_enclosed_in_braces(SubformulaType::Subscript, nest, scanner, eqtb, logger);
    }
}

/// See 1172.
pub fn append_choices(
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let choice_node = ChoiceNode {
        display_mlist: Box::default(),
        text_mlist: Box::default(),
        script_mlist: Box::default(),
        script_script_mlist: Box::default(),
    };
    nest.tail_push(Node::Choice(choice_node), eqtb);
    push_math(
        GroupType::MathChoice { t: 0 },
        nest,
        &scanner.input_stack,
        eqtb,
        logger,
    );
    scanner.scan_left_brace(eqtb, logger)
}

/// See 1174.
pub fn build_choices(
    t: u8,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    eqtb.unsave(scanner, logger);
    let old_level = nest.pop_nest(eqtb);
    let RichMode::Math(mmode) = old_level.mode else {
        panic!("We must be in math mode here");
    };
    let mlist = mmode.fin_mlist(None);
    let Some(choice_node) = nest.last_choice_mut() else {
        panic!("The last node must be a ChoiceNode here");
    };
    match t {
        0 => {
            choice_node.display_mlist = Box::new(mlist);
        }
        1 => {
            choice_node.text_mlist = Box::new(mlist);
        }
        2 => {
            choice_node.script_mlist = Box::new(mlist);
        }
        3 => {
            choice_node.script_script_mlist = Box::new(mlist);
            return;
        }
        _ => panic!("Impossible"),
    }
    push_math(
        GroupType::MathChoice { t: t + 1 },
        nest,
        &scanner.input_stack,
        eqtb,
        logger,
    );
    scanner.scan_left_brace(eqtb, logger);
}

/// See 1165.
pub fn math_ac(
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let math_char = Integer::scan_fifteen_bit_int(scanner, eqtb, logger) as u16;
    let accent_fam = if math_char >= VAR_CODE && fam_in_range(eqtb) {
        *eqtb.integers.get(IntegerVariable::CurFam) as u8
    } else {
        (math_char / 256) as u8 % 16
    };

    let mut accent_noad = AccentNoad {
        accent_char: (math_char % 256) as u8,
        accent_fam,
        nucleus: None,
        subscript: None,
        superscript: None,
    };

    if let Some(noad_field) = scan_math_char(scanner, eqtb, logger) {
        accent_noad.nucleus = Some(Box::new(noad_field));
        nest.tail_push(Node::Noad(Noad::Accent(accent_noad)), eqtb);
    } else {
        nest.tail_push(Node::Noad(Noad::Accent(accent_noad)), eqtb);
        scan_subformula_enclosed_in_braces(SubformulaType::Nucleus, nest, scanner, eqtb, logger);
    }
}

/// See 1163.
pub fn math_radical(
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let left_delimiter = scan_radical_delimiter(scanner, eqtb, logger);
    let mut radical_noad = RadicalNoad {
        left_delimiter,
        nucleus: None,
        subscript: None,
        superscript: None,
    };

    if let Some(noad_field) = scan_math_char(scanner, eqtb, logger) {
        radical_noad.nucleus = Some(Box::new(noad_field));
        nest.tail_push(Node::Noad(Noad::Radical(radical_noad)), eqtb);
    } else {
        nest.tail_push(Node::Noad(Noad::Radical(radical_noad)), eqtb);
        scan_subformula_enclosed_in_braces(SubformulaType::Nucleus, nest, scanner, eqtb, logger);
    }
}

/// See 1160.
fn scan_radical_delimiter(
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> DelimiterField {
    // NOTE We really only need the lower 24 bits here.
    let val = Integer::scan_twenty_seven_bit_int(scanner, eqtb, logger);
    DelimiterField {
        small_fam: (val / 0o4000000) as u8 % 16,
        small_char: ((val / 0o10000) % 256) as u8,
        large_fam: (val / 256) as u8 % 16,
        large_char: (val % 256) as u8,
    }
}

/// See 1160.
fn scan_delimiter(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) -> DelimiterField {
    let (unexpandable_command, token) =
        scanner.get_next_non_blank_non_relax_non_call_token(eqtb, logger);
    // NOTE We really only need the lower24 bits here.
    let mut delim = match unexpandable_command {
        UnexpandableCommand::Letter(c) | UnexpandableCommand::Other(c) => eqtb.del_code(c as usize),
        UnexpandableCommand::Math(MathCommand::DelimNum) => {
            Integer::scan_twenty_seven_bit_int(scanner, eqtb, logger)
        }
        _ => -1,
    };
    if delim < 0 {
        logger.print_err("Missing delimiter (. inserted)");
        let help = &[
            "I was expecting to see something like `(' or `\\{' or",
            "`\\}' here. If you typed, e.g., `{' instead of `\\{', you",
            "should probably delete the `{' by typing `1' now, so that",
            "braces don't get unbalanced. Otherwise just proceed.",
            "Acceptable delimiters are characters whose \\delcode is",
            "nonnegative, or you can use `\\delimiter <delimiter code>'.",
        ];
        scanner.back_error(token, help, eqtb, logger);
        delim = 0;
    }
    DelimiterField {
        small_fam: (delim / 0o4000000) as u8 % 16,
        small_char: ((delim / 0o10000) % 256) as u8,
        large_fam: (delim / 256) as u8 % 16,
        large_char: (delim % 256) as u8,
    }
}

/// See 1153.
pub fn scan_subformula_enclosed_in_braces(
    subformula_type: SubformulaType,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    scanner.scan_left_brace(eqtb, logger);
    push_math(
        GroupType::Math { subformula_type },
        nest,
        &scanner.input_stack,
        eqtb,
        logger,
    )
}

/// See 1155.
pub fn set_math_char(mmode: &mut MathMode, c: u16, eqtb: &mut Eqtb) {
    let nucleus = Some(Box::new(NoadField::MathChar {
        fam: if c >= VAR_CODE && fam_in_range(eqtb) {
            *eqtb.integers.get(IntegerVariable::CurFam) as u8
        } else {
            (c / 256) as u8 % 16
        },
        character: (c % 256) as u8,
    }));
    let mut noad = NormalNoad::new();
    noad.nucleus = nucleus;
    noad.noad_type = if c >= VAR_CODE {
        NoadType::Ord
    } else {
        NoadType::from((c / 0o10000) as u8)
    };
    mmode.append_node(Node::Noad(Noad::Normal(noad)), eqtb);
}

/// See 1159.
pub fn math_limit_switch(
    new_limit_type: LimitType,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    if let Some(Noad::Normal(NormalNoad {
        noad_type: NoadType::Op { limit_type },
        ..
    })) = nest.last_noad_mut()
    {
        *limit_type = new_limit_type;
        return;
    }
    logger.print_err("Limit controls must follow a math operator");
    let help = &["I'm ignoring this misplaced \\limits or \\nolimits command."];
    logger.error(help, scanner, eqtb);
}

/// See 1151.
pub fn scan_math_char(
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Option<NoadField> {
    let (mut unexpandable_command, mut token) =
        scanner.get_next_non_blank_non_relax_non_call_token(eqtb, logger);
    loop {
        let math_char = match unexpandable_command {
            UnexpandableCommand::Letter(c)
            | UnexpandableCommand::Other(c)
            | UnexpandableCommand::CharGiven(c) => {
                let math_code = eqtb.math_code(c as usize) as u16;
                if math_code == 0o10_0000 {
                    treat_cur_chr_as_active_character(c, scanner, eqtb, logger);
                    (unexpandable_command, token) =
                        scanner.get_next_non_blank_non_relax_non_call_token(eqtb, logger);
                    continue;
                } else {
                    math_code
                }
            }
            UnexpandableCommand::CharNum => {
                let c = scanner.scan_char_num(eqtb, logger);
                unexpandable_command = UnexpandableCommand::CharGiven(c);
                continue;
            }
            UnexpandableCommand::Math(MathCommand::MathCharNum) => {
                Integer::scan_fifteen_bit_int(scanner, eqtb, logger) as u16
            }
            UnexpandableCommand::Math(MathCommand::MathCharGiven(math_char)) => math_char,
            UnexpandableCommand::Math(MathCommand::DelimNum) => {
                (Integer::scan_twenty_seven_bit_int(scanner, eqtb, logger) / 0o10000) as u16
            }
            _ => {
                scanner.back_input(token, eqtb, logger);
                return None;
            }
        };

        let noad_field = NoadField::MathChar {
            character: (math_char % 256) as u8,
            fam: if math_char >= VAR_CODE && fam_in_range(eqtb) {
                *eqtb.integers.get(IntegerVariable::CurFam) as u8
            } else {
                (math_char / 256) as u8 % 16
            },
        };
        return Some(noad_field);
    }
}

/// See 1151.
fn fam_in_range(eqtb: &Eqtb) -> bool {
    let cur_fam = *eqtb.integers.get(IntegerVariable::CurFam);
    cur_fam >= 0 && cur_fam < 16
}

/// See 1152.
pub fn treat_cur_chr_as_active_character(
    chr: u8,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let cs = ControlSequence::Active(chr);
    let command = eqtb.control_sequences.get(cs).clone();
    let token = Token::CSToken { cs };
    let (_, token) = x_token(command, token, scanner, eqtb, logger);
    scanner.back_input(token, eqtb, logger);
}

/// See 1136.
fn push_math(
    c: GroupType,
    nest: &mut SemanticState,
    input_stack: &InputStack,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    nest.push_nest(
        RichMode::Math(MathMode {
            list: Vec::new(),
            display: false,
            incompleat_noad: None,
        }),
        input_stack,
        eqtb,
        logger,
    );
    eqtb.new_save_level(c, input_stack, logger)
}

/// See 1166.
pub fn complain_that_user_should_have_said_mathaccent(
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    logger.print_err("Please use ");
    logger.print_esc_str(b"mathaccent");
    logger.print_str(" for accents in math mode");
    let help = &[
        "I'm changing \\accent to \\mathaccent here; wish me luck.",
        "(Accents are not the same in formulas as they are in text.)",
    ];
    logger.error(help, scanner, eqtb)
}
