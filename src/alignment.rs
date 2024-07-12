use crate::box_building::{end_graf, normal_paragraph, off_save};
use crate::command::{Command, PrefixableCommand, UnexpandableCommand};
use crate::dimension::{is_running, Dimension, NULL_FLAG};
use crate::eqtb::save_stack::GroupType;
use crate::eqtb::{
    ControlSequence, DimensionVariable, Eqtb, IntegerVariable, SkipVariable, TokenListVariable,
};
use crate::error::fatal_error;
use crate::glue::scan_glue;
use crate::horizontal_mode::{HorizontalMode, HorizontalModeType};
use crate::hyphenation::Hyphenator;
use crate::input::expansion::expand;
use crate::input::token_source::{AlignCommand, TokenSourceType};
use crate::input::{InputStack, Scanner, ScannerStatus};
use crate::logger::Logger;
use crate::math::{check_that_another_mathshift_symbol_follows, resume_after_display};
use crate::math_mode::MathMode;
use crate::mode_independent::do_assignments;
use crate::nodes::{
    DimensionOrder, GlueNode, GlueSign, GlueSpec, GlueType, HigherOrderDimension, HlistOrVlist,
    ListNode, Node, PenaltyNode, RuleNode, UnsetNode,
};
use crate::output::Output;
use crate::packaging::{
    hpack, pack_halign_cell, pack_valign_cell, scan_spec, split_adjust_material_off, vpack,
    TargetSpec,
};
use crate::page_breaking::PageBuilder;
use crate::print::Printer;
use crate::round;
use crate::semantic_nest::{RichMode, SemanticState};
use crate::token::Token;
use crate::token_lists::RcTokenList;
use crate::vertical_mode::VerticalMode;

use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct AlignState {
    alignments: Vec<Alignment>,
}

impl AlignState {
    pub const fn new() -> Self {
        Self {
            alignments: Vec::new(),
        }
    }

    /// See 774.
    pub fn init_align(
        &mut self,
        token: Token,
        hyphenator: &mut Hyphenator,
        page_builder: &mut PageBuilder,
        output: &mut Output,
        nest: &mut SemanticState,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        let Token::CSToken { cs } = token else {
            panic!("Impossible")
        };
        let alignment = Alignment::init(cs, nest, scanner, eqtb, logger);
        self.alignments.push(alignment);
        self.align_peek(
            hyphenator,
            page_builder,
            output,
            nest,
            scanner,
            eqtb,
            logger,
        );
    }

    /// See 785.
    pub fn align_peek(
        &mut self,
        hyphenator: &mut Hyphenator,
        page_builder: &mut PageBuilder,
        output: &mut Output,
        nest: &mut SemanticState,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        scanner.align_state = 1_000_000;
        let (mut unexpandable_command, mut token) =
            scanner.get_next_non_blank_non_call_token(eqtb, logger);
        // We want to ignore any \crcr here.
        while let UnexpandableCommand::CarRet { weak: true } = unexpandable_command {
            scanner.align_state = 1_000_000;
            (unexpandable_command, token) = scanner.get_next_non_blank_non_call_token(eqtb, logger);
        }
        match unexpandable_command {
            UnexpandableCommand::NoAlign => {
                scanner.scan_left_brace(eqtb, logger);
                eqtb.new_save_level(GroupType::NoAlign, &scanner.input_stack, logger);
                if let RichMode::Vertical(_) = nest.mode() {
                    normal_paragraph(eqtb, logger);
                }
            }
            UnexpandableCommand::RightBrace(_) => {
                let mut alignment = self.alignments.pop().unwrap();
                scanner.align_state = alignment.prev_align_state;
                alignment.fin_align(
                    hyphenator,
                    page_builder,
                    output,
                    nest,
                    scanner,
                    eqtb,
                    logger,
                );
            }
            _ => {
                let alignment = self.alignments.last_mut().unwrap();
                alignment.init_row(unexpandable_command, token, nest, scanner, eqtb, logger);
            }
        }
    }

    /// See 1131.
    pub fn do_endv(
        &mut self,
        token: Token,
        hyphenator: &mut Hyphenator,
        page_builder: &mut PageBuilder,
        output: &mut Output,
        nest: &mut SemanticState,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        let ending_command = scanner
            .input_stack
            .find_ending_command_of_cell(eqtb, logger);

        if let GroupType::AlignEntry = eqtb.cur_group.typ {
            // In case of a vertical alignment, we need to finish any paragraphs.
            end_graf(hyphenator, nest, scanner, eqtb, logger);
            let alignment = self.alignments.last_mut().unwrap();
            if alignment.fin_col(ending_command, nest, scanner, eqtb, logger) {
                alignment.fin_row(nest, scanner, eqtb, logger);
                self.align_peek(
                    hyphenator,
                    page_builder,
                    output,
                    nest,
                    scanner,
                    eqtb,
                    logger,
                );
            }
        } else {
            off_save(UnexpandableCommand::Endv, token, scanner, eqtb, logger);
        }
    }
}

#[derive(Debug)]
pub struct Alignment {
    /// Points to beginning of preamble list.
    preamble: Preamble,
    /// Position in preamble list.
    cur_column: usize,
    /// Value of cur_column at the beginning of a span.
    span_start: usize,
    /// Previous level of brace nesting.
    prev_align_state: i32,
    /// Adjustment list.
    adjustments: Vec<Node>,
    /// Indicate whether we are omitting templates for the current cell.
    omitting_templates: bool,
}

/// This stores the templates for the columns and collects size and span information to be used
/// when the alignment is set.
#[derive(Debug)]
struct Preamble {
    left_glue: GlueNode,
    columns: Vec<ColumnRecord>,
    /// Index of the column where an extension of the alignment should start if at all.
    loop_index: Option<usize>,
}

impl Preamble {
    /// Scan the input for a complete preamble specification and return
    /// the generated preamble.
    /// See 777., 778. and 779.
    fn scan_preamble(
        cs: ControlSequence,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> Self {
        scanner.scanner_status = ScannerStatus::Aligning;
        scanner.warning_index = cs;
        scanner.align_state = -1_000_000;
        let left_glue = GlueNode::new_param(SkipVariable::TabSkip, eqtb);
        let mut columns = Vec::new();
        let mut loop_index = None;
        let mut cur_column = 0;
        loop {
            let u_template =
                Self::scan_u_template(cur_column, &mut loop_index, scanner, eqtb, logger);
            let (v_template, preamble_finished) = Self::scan_v_template(scanner, eqtb, logger);

            let right_glue = GlueNode::new_param(SkipVariable::TabSkip, eqtb);
            let column = ColumnRecord::new(u_template, v_template, right_glue);
            columns.push(column);

            cur_column += 1;
            if preamble_finished {
                break;
            }
        }
        scanner.scanner_status = ScannerStatus::Normal;
        Self {
            left_glue,
            columns,
            loop_index,
        }
    }

    /// Scans a u-template and sets the loop index to the current column number if appropriate.
    /// See 783.
    fn scan_u_template(
        cur_column: usize,
        loop_index: &mut Option<usize>,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> Rc<Vec<Token>> {
        scanner.template = Vec::new();
        loop {
            let (command, token) = get_preamble_token(scanner, eqtb, logger);
            if let Command::Unexpandable(UnexpandableCommand::MacParam(_)) = command {
                return Rc::new(scanner.template.clone());
            }
            // Ignore leading whitespace
            if scanner.template.is_empty() {
                if let Command::Unexpandable(UnexpandableCommand::Spacer) = command {
                    continue;
                }
            }
            match command {
                Command::Unexpandable(UnexpandableCommand::TabMark(_))
                    if scanner.align_state == -1_000_000 =>
                {
                    // The first u-template to start with an & is taken as the beginning
                    // of a periodic preamble.
                    if scanner.template.is_empty() && loop_index.is_none() {
                        *loop_index = Some(cur_column);
                    } else {
                        logger.print_err("Missing # inserted in alignment preamble");
                        let help = &[
                            "There should be exactly one # between &'s, when an",
                            "\\halign or \\valign is being set up. In this case you had",
                            "none, so I've put one in; maybe that will work.",
                        ];
                        scanner.back_error(token, help, eqtb, logger);
                        return Rc::new(scanner.template.clone());
                    }
                }
                Command::Unexpandable(UnexpandableCommand::CarRet { .. })
                    if scanner.align_state == -1_000_000 =>
                {
                    logger.print_err("Missing # inserted in alignment preamble");
                    let help = &[
                        "There should be exactly one # between &'s, when an",
                        "\\halign or \\valign is being set up. In this case you had",
                        "none, so I've put one in; maybe that will work.",
                    ];
                    scanner.back_error(token, help, eqtb, logger);
                    return Rc::new(scanner.template.clone());
                }
                _ => {
                    scanner.template.push(token);
                }
            }
        }
    }

    /// Scans a v-template and returns the template and whether the preamble is finished.
    /// 784.
    fn scan_v_template(
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> (Rc<Vec<Token>>, bool) {
        scanner.template = Vec::new();

        let preamble_finished = loop {
            let (command, token) = get_preamble_token(scanner, eqtb, logger);
            match command {
                Command::Unexpandable(UnexpandableCommand::CarRet { .. })
                    if scanner.align_state == -1_000_000 =>
                {
                    break true
                }
                Command::Unexpandable(UnexpandableCommand::TabMark(_))
                    if scanner.align_state == -1_000_000 =>
                {
                    break false
                }
                Command::Unexpandable(UnexpandableCommand::MacParam(_)) => {
                    logger.print_err("Only one # is allowed per tab");
                    let help = &[
                        "There should be exactly one # between &'s, when an",
                        "\\halign or \\valign is being set up. In this case you had",
                        "more than one, so I'm ignoring all but the first.",
                    ];
                    logger.error(help, scanner, eqtb);
                    continue;
                }
                _ => scanner.template.push(token),
            }
        };
        scanner.template.push(END_TEMPLATE_TOKEN);
        (Rc::new(scanner.template.clone()), preamble_finished)
    }

    /// Create a template node list from the preamble.
    /// This is a list of GlueNodes and UnsetNodes where the UnsetNodes have the correct widths
    /// of the corresponding columns.
    /// See 801. and 802.
    fn create_unset_template_list(&mut self) -> Vec<Node> {
        let mut template_list = Vec::new();
        let left_glue = self.left_glue.clone();
        template_list.push(Node::Glue(left_glue));
        for q in 0..self.columns.len() {
            let cur_record = &mut self.columns[q];
            // If the current column has no width set, disable the column by setting its width to
            // zero and making the following zero glue.
            if cur_record.width == NULL_FLAG {
                cur_record.width = 0;
                cur_record.right_glue.glue_spec = std::rc::Rc::new(GlueSpec::ZERO_GLUE.clone());
            }
            // If there are spans starting at the current column, transfer any
            // remaining span widths to the next column.
            if !cur_record.spans.is_empty() {
                self.move_span_info_to_next_node(q);
            }
            let cur_record = &mut self.columns[q];
            let column_template = UnsetNode {
                width: cur_record.width,
                height: 0,
                depth: 0,
                list: Vec::new(),
                stretch: HigherOrderDimension::default(),
                shrink: HigherOrderDimension::default(),
                span_count: 0,
            };
            template_list.push(Node::Unset(column_template));
            let right_glue = cur_record.right_glue.clone();
            template_list.push(Node::Glue(right_glue));
        }
        template_list
    }

    /// Move the span sizes of the current column to the next one.
    /// We subtract the size of the current column and right glue from the span size and update the
    /// corresponding span size in the next node, i.e. a span that spans one column less, if it is
    /// larger. Special care goes to the case for a span of two columns. The remaining size of that
    /// span is used to potentially update the next node's width.
    /// See 803.
    fn move_span_info_to_next_node(&mut self, cur_index: usize) {
        let cur_record = &mut self.columns[cur_index];
        let t = cur_record.width + cur_record.right_glue.glue_spec.width;

        let two_span_width = cur_record.spans.remove(&1);
        let span_widths: Vec<_> = cur_record.spans.drain().collect();
        let next_record = &mut self.columns[cur_index + 1];

        // Treat the special case of a span of 2 columns
        if let Some(w) = two_span_width {
            if w - t > next_record.width {
                next_record.width = w - t;
            }
        }
        // Treat the rest
        for (n, w) in span_widths {
            // Reduce by the number of spanned columns and the width
            let (n, w) = (n - 1, w - t);
            next_record
                .spans
                .entry(n)
                .and_modify(|old_w| {
                    if *old_w < w {
                        *old_w = w
                    }
                })
                .or_insert(w);
        }
    }
}

/// See 780.
const END_TEMPLATE_TOKEN: Token = Token::CSToken {
    cs: ControlSequence::FrozenEndTemplate,
};

/// Gets the next unexpanded token, with two exceptions:
///  * if the next token is \tabskip, scan the necessary information to change its current value
///    and proceed to the next token,
///  * if the next token is \span, expand the following token.
/// See 782.
fn get_preamble_token(
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> (Command, Token) {
    loop {
        let (mut command, mut token) = scanner.get_command_and_token(eqtb, logger);
        // Expand tokens following a \span according to normal expansion rules.
        while let Command::Unexpandable(UnexpandableCommand::Span) = command {
            // Get the next token.
            (command, token) = scanner.get_command_and_token(eqtb, logger);
            if let Command::Expandable(expandable_command) = command {
                expand(expandable_command, token, scanner, eqtb, logger);
                (command, token) = scanner.get_command_and_token(eqtb, logger);
            }
        }
        if let Command::Unexpandable(UnexpandableCommand::Endv) = command {
            fatal_error(
                "(interwoven alignment preambles are not allowed)",
                &scanner.input_stack,
                eqtb,
                logger,
            );
        }
        // If a \tabskip appears, scan enough information to change its current value.
        if let Command::Unexpandable(UnexpandableCommand::Prefixable(PrefixableCommand::Glue(
            SkipVariable::TabSkip,
        ))) = command
        {
            scanner.scan_optional_equals(eqtb, logger);
            let skip = scan_glue(false, scanner, eqtb, logger);
            if eqtb.integer(IntegerVariable::GlobalDefs) > 0 {
                eqtb.skip_define(SkipVariable::TabSkip, skip, true);
            } else {
                eqtb.skip_define(SkipVariable::TabSkip, skip, false);
            }
            continue;
        }
        return (command, token);
    }
}

/// Stores the templates for a column as well as size and span information.
#[derive(Debug)]
struct ColumnRecord {
    right_glue: GlueNode,
    u_template: RcTokenList,
    v_template: RcTokenList,
    spans: HashMap<usize, Dimension>,
    width: Dimension,
}

impl ColumnRecord {
    fn new(u_template: RcTokenList, v_template: RcTokenList, right_glue: GlueNode) -> Self {
        Self {
            u_template,
            v_template,
            right_glue,
            spans: HashMap::new(),
            width: NULL_FLAG,
        }
    }
}

/// See 775.
fn determine_mode(nest: &SemanticState) -> RichMode {
    // We keep the parameters from the outer mode.
    match nest.mode() {
        RichMode::Vertical(vmode) => {
            RichMode::Vertical(VerticalMode::new_internal_with_prev_depth(vmode.prev_depth))
        }
        RichMode::Horizontal(hmode) => RichMode::Horizontal(HorizontalMode {
            list: Vec::new(),
            subtype: HorizontalModeType::Restricted,
            space_factor: hmode.space_factor,
        }),
        RichMode::Math(_) => {
            let RichMode::Vertical(outer_vmode) = nest.outer_mode() else {
                panic!("We expect the outer mode to be vertical");
            };
            RichMode::Vertical(VerticalMode::new_internal_with_prev_depth(
                outer_vmode.prev_depth,
            ))
        }
    }
}

/// See 776.
fn check_for_improper_alignment_in_displayed_math(
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    if let RichMode::Math(mmode) = nest.mode_mut() {
        if !mmode.list.is_empty() || mmode.incompleat_noad.is_some() {
            logger.print_err("Improper ");
            logger.print_esc_str(b"halign");
            logger.print_str(" inside $$'s");
            let help = &[
                "Displays can use special alignments (like \\eqalignno)",
                "only if nothing but the alignment itself is between $$'s.",
                "So I've deleted the formulas that preceded this alignment.",
            ];
            logger.error(help, scanner, eqtb);
            mmode.flush_math(eqtb);
        }
    }
}

impl Alignment {
    /// See 774.
    fn init(
        cs: ControlSequence,
        nest: &mut SemanticState,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> Self {
        let prev_align_state = scanner.align_state;
        scanner.align_state = -1_000_000;
        check_for_improper_alignment_in_displayed_math(nest, scanner, eqtb, logger);
        let mode = determine_mode(nest);
        nest.push_nest(mode, &scanner.input_stack, eqtb, logger);
        let spec = scan_spec(scanner, eqtb, logger);
        eqtb.new_save_level(GroupType::Align { spec }, &scanner.input_stack, logger);
        scanner.scan_left_brace(eqtb, logger);
        let preamble = Preamble::scan_preamble(cs, scanner, eqtb, logger);
        eqtb.new_save_level(GroupType::AlignEntry, &scanner.input_stack, logger);
        if let Some(every_cr) = eqtb.token_lists.get(TokenListVariable::EveryCr) {
            scanner.input_stack.begin_token_list(
                every_cr.clone(),
                TokenSourceType::EveryCrText,
                eqtb,
                logger,
            );
        }
        Self {
            cur_column: 0,
            preamble,
            span_start: 0,
            prev_align_state,
            adjustments: Vec::new(),
            omitting_templates: false,
        }
    }

    /// See 786.
    fn init_row(
        &mut self,
        unexpandable_command: UnexpandableCommand,
        token: Token,
        nest: &mut SemanticState,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        let mode = match nest.mode() {
            RichMode::Vertical(_) => RichMode::Horizontal(HorizontalMode {
                list: Vec::new(),
                subtype: HorizontalModeType::Restricted,
                // The space_factor is not used but we keep this for backward compatibility in
                // diagnostic messages.
                space_factor: 0,
            }),
            RichMode::Horizontal(_) => {
                // The prev_depth is not used but we keep this for backward compatibility in
                // diagnostic messages.
                RichMode::Vertical(VerticalMode::new_internal_with_prev_depth(0))
            }
            _ => panic!("Should not happen"),
        };
        nest.push_nest(mode, &scanner.input_stack, eqtb, logger);
        let left_glue = self.preamble.left_glue.clone();
        nest.tail_push(Node::Glue(left_glue), eqtb);
        self.cur_column = 0;
        self.init_span(0, nest, &scanner.input_stack, eqtb, logger);
        self.init_col(unexpandable_command, token, scanner, eqtb, logger);
    }

    /// See 787.
    fn init_span(
        &mut self,
        span_start: usize,
        nest: &mut SemanticState,
        input_stack: &InputStack,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        match nest.mode() {
            RichMode::Horizontal(_) => {
                let span_mode = RichMode::Horizontal(HorizontalMode::new_restricted());
                nest.push_nest(span_mode, input_stack, eqtb, logger);
            }
            RichMode::Vertical(_) => {
                let span_mode = RichMode::Vertical(VerticalMode::new_internal());
                nest.push_nest(span_mode, input_stack, eqtb, logger);
                normal_paragraph(eqtb, logger);
            }
            _ => panic!("We expect only horizontal or vertical mode here"),
        }
        self.span_start = span_start;
    }

    /// See 788. and 789.
    fn init_col(
        &mut self,
        unexpandable_command: UnexpandableCommand,
        token: Token,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        let cur_record = &mut self.preamble.columns[self.cur_column];
        if let UnexpandableCommand::Omit = unexpandable_command {
            self.omitting_templates = true;
            scanner.align_state = 0;

            // Use an empty v-template.
            let v_template = std::rc::Rc::new(vec![END_TEMPLATE_TOKEN]);
            scanner.v_templates.push(v_template);
        } else {
            self.omitting_templates = false;
            scanner.back_input(token, eqtb, logger);
            scanner.input_stack.begin_token_list(
                cur_record.u_template.clone(),
                TokenSourceType::UTemplate,
                eqtb,
                logger,
            );
            scanner.v_templates.push(cur_record.v_template.clone());
        }
    }

    /// Finish the current column.
    /// Returns true if both a row and a column are completed.
    /// See 791.
    pub fn fin_col(
        &mut self,
        mut ending_command: AlignCommand,
        nest: &mut SemanticState,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> bool {
        if scanner.align_state < 500000 {
            fatal_error(
                "(interwoven alignment preambles are not allowed)",
                &scanner.input_stack,
                eqtb,
                logger,
            );
        }
        if self.cur_column >= self.preamble.columns.len() {
            panic!("We always expect cur_align to point into an existing column");
        }
        // If we reached the end of the alignment but not with a \cr.
        if self.cur_column == self.preamble.columns.len() - 1
            && (ending_command == AlignCommand::Tab || ending_command == AlignCommand::Span)
        {
            self.extend_alignment_or_complain(&mut ending_command, scanner, eqtb, logger);
        }
        if ending_command != AlignCommand::Span {
            eqtb.unsave(scanner, logger);
            eqtb.new_save_level(GroupType::AlignEntry, &scanner.input_stack, logger);
            self.package_unset_box_for_current_column_and_record_width(nest, eqtb);
            let cur_record = &mut self.preamble.columns[self.cur_column];
            nest.tail_push(Node::Glue(cur_record.right_glue.clone()), eqtb);
            if ending_command == AlignCommand::CarRet {
                return true;
            }
            self.init_span(
                self.cur_column + 1,
                nest,
                &scanner.input_stack,
                eqtb,
                logger,
            );
        }
        scanner.align_state = 1_000_000;
        let (command, token) = scanner.get_next_non_blank_non_call_token(eqtb, logger);
        self.cur_column += 1;
        self.init_col(command, token, scanner, eqtb, logger);
        false
    }

    /// See 792. and 795.
    fn extend_alignment_or_complain(
        &mut self,
        ending_command: &mut AlignCommand,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        match self.preamble.loop_index {
            Some(loop_index) => {
                self.lengthen_preamble_periodically(loop_index);
            }
            None => {
                logger.print_err("Extra alignment tab has been changed to ");
                logger.print_esc_str(b"cr");
                let help = &[
                    "You have given more \\span or & marks than there were",
                    "in the preamble to the \\halign or \\valign now in progress.",
                    "So I'll assume that you meant to type \\cr instead.",
                ];
                *ending_command = AlignCommand::CarRet;
                logger.error(help, scanner, eqtb);
            }
        }
    }

    /// See 793.
    fn lengthen_preamble_periodically(&mut self, loop_index: usize) {
        let loop_record = &self.preamble.columns[loop_index];
        let u_template = loop_record.u_template.clone();
        let v_template = loop_record.v_template.clone();
        let right_glue = loop_record.right_glue.clone();
        self.preamble
            .columns
            .push(ColumnRecord::new(u_template, v_template, right_glue));
        self.preamble.loop_index = Some(self.preamble.loop_index.unwrap() + 1);
    }

    /// See 796.
    fn package_unset_box_for_current_column_and_record_width(
        &mut self,
        nest: &mut SemanticState,
        eqtb: &mut Eqtb,
    ) {
        let mut unset_node;
        let w;
        let old_level = nest.pop_nest(eqtb);
        match old_level.mode {
            RichMode::Horizontal(hmode) => {
                let mut hlist = hmode.list;
                let mut adjustment_material = split_adjust_material_off(&mut hlist);
                unset_node = pack_halign_cell(hlist);
                w = unset_node.width;
                self.adjustments.append(&mut adjustment_material);
            }
            RichMode::Vertical(vmode) => {
                let vlist = vmode.list;
                unset_node = pack_valign_cell(vlist);
                w = unset_node.height;
            }
            RichMode::Math(_) => panic!("MathMode should not be possible here"),
        }
        if self.span_start != self.cur_column {
            let span_count = self.update_width_entry_for_spanned_columns(w);
            unset_node.span_count = span_count;
        } else {
            let cur_record = &mut self.preamble.columns[self.cur_column];
            if w > cur_record.width {
                cur_record.width = w;
            }
            unset_node.span_count = 0;
        }
        nest.tail_push(Node::Unset(unset_node), eqtb);
    }

    /// Returns the number of columns spanned minus one.
    /// See 798.
    fn update_width_entry_for_spanned_columns(&mut self, w: i32) -> usize {
        // Determine how many columns were spanned.
        // n will correspond to the spanned columns + 1
        let span_count = self.cur_column - self.span_start;

        let spans = &mut self.preamble.columns[self.span_start].spans;
        // Update the old width if it exists and is smaller or create a new entry.
        spans
            .entry(span_count)
            .and_modify(|old_w| {
                if *old_w < w {
                    *old_w = w
                }
            })
            .or_insert(w);
        span_count
    }

    /// See 799.
    pub fn fin_row(
        &mut self,
        nest: &mut SemanticState,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        let old_level = nest.pop_nest(eqtb);
        match old_level.mode {
            RichMode::Horizontal(hmode) => {
                let hlist = hmode.list;
                let list_node = hpack(hlist, TargetSpec::Natural, eqtb, logger);
                let unset_node = UnsetNode::from_list_node(list_node);
                let RichMode::Vertical(vmode) = nest.mode_mut() else {
                    panic!("We expected to be in vertical mode here");
                };
                vmode.append_unset_node_to_vlist(unset_node, eqtb);
                let adjustments = std::mem::take(&mut self.adjustments);
                nest.tail_append(adjustments, eqtb);
            }
            RichMode::Vertical(vmode) => {
                let vlist = vmode.list;
                let list_node = vpack(vlist, TargetSpec::Natural, eqtb, logger);
                let unset_node = UnsetNode::from_list_node(list_node);
                nest.tail_push(Node::Unset(unset_node), eqtb);
                let RichMode::Horizontal(hmode) = nest.mode_mut() else {
                    panic!("We expect to be back in horizontal mode here");
                };
                hmode.set_space_factor(1000, eqtb);
            }
            RichMode::Math(_) => panic!("MathMode should not be possible here"),
        }
        if let Some(every_cr) = eqtb.token_lists.get(TokenListVariable::EveryCr) {
            scanner.input_stack.begin_token_list(
                every_cr.clone(),
                TokenSourceType::EveryCrText,
                eqtb,
                logger,
            );
        }
    }

    /// See 800.
    fn fin_align(
        &mut self,
        hyphenator: &mut Hyphenator,
        page_builder: &mut PageBuilder,
        output: &mut Output,
        nest: &mut SemanticState,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        if let GroupType::AlignEntry = eqtb.cur_group.typ {
        } else {
            panic!("We always expect to be in an AlignEntry group here");
        }
        eqtb.unsave(scanner, logger);
        let GroupType::Align { spec } = eqtb.cur_group.typ else {
            panic!("We always expect to be in an Align group here");
        };
        eqtb.unsave(scanner, logger);
        let indent = if let RichMode::Math(MathMode { .. }) = nest.outer_mode() {
            eqtb.dimen(DimensionVariable::DisplayIndent)
        } else {
            0
        };
        let unset_template_list = self.preamble.create_unset_template_list();
        let template_box =
            self.package_unset_template_list(unset_template_list, spec, nest, eqtb, logger);
        set_glue_in_all_unset_boxes_of_current_list(template_box, indent, nest, eqtb, logger);

        insert_current_list_into_its_environment(
            hyphenator,
            page_builder,
            output,
            nest,
            scanner,
            eqtb,
            logger,
        );
    }

    /// Use the target spec to package the unset template list into a box with
    /// the correct glue setting.
    /// See 804.
    fn package_unset_template_list(
        &self,
        mut unset_template_list: Vec<Node>,
        spec: TargetSpec,
        nest: &SemanticState,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> ListNode {
        let mut template_box;
        logger.pack_begin_line = -nest.mode_line();
        if let RichMode::Vertical(_) = nest.mode() {
            // Temporarily set \overfullrule to 0 to avoid printing overfull rules.
            let rule_save = eqtb.dimen(DimensionVariable::OverfullRule);
            eqtb.dimensions.set(DimensionVariable::OverfullRule, 0);
            template_box = hpack(unset_template_list, spec, eqtb, logger);
            eqtb.dimensions
                .set(DimensionVariable::OverfullRule, rule_save);
        } else {
            // Swap width and height so that we can use vpack instead of hpack
            // and get correct error messages.
            let mut q = 1;
            loop {
                if let Node::Unset(unset_node) = &mut unset_template_list[q] {
                    unset_node.height = unset_node.width;
                    unset_node.width = 0;
                } else {
                    panic!("Impossible");
                }
                q += 2;
                if q >= unset_template_list.len() {
                    break;
                }
            }
            template_box = vpack(unset_template_list, spec, eqtb, logger);
            // Swap back width and height.
            match &mut template_box.list {
                HlistOrVlist::Vlist(vlist) => {
                    let mut q = 1;
                    loop {
                        if let Node::Unset(unset_node) = &mut vlist[q] {
                            unset_node.width = unset_node.height;
                            unset_node.height = 0;
                        }
                        q += 2;
                        if q >= vlist.len() {
                            break;
                        }
                    }
                }
                _ => panic!("Impossible"),
            }
        }
        logger.pack_begin_line = 0;
        template_box
    }
}

/// See 805.
fn set_glue_in_all_unset_boxes_of_current_list(
    template_box: ListNode,
    indent: i32,
    nest: &mut SemanticState,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let nodes = std::mem::take(nest.cur_list_mut());
    let mut packed_nodes = Vec::new();
    // We go over the Nodes of the current list. It consists of the UnsetNodes that correspond to
    // the rows, and any material that has been inserted using \noalign.
    for node in nodes {
        let packed_node = match node {
            Node::Unset(row) => Node::List(set_row_and_unset_boxes_in_it(
                &template_box,
                row,
                indent,
                nest,
            )),
            Node::Rule(rule_node) => {
                adapt_rule_dimensions_to_alignment(&template_box, rule_node, indent, eqtb, logger)
            }
            node => node,
        };
        packed_nodes.push(packed_node);
    }
    nest.tail_append(packed_nodes, eqtb);
}

/// See 806.
fn adapt_rule_dimensions_to_alignment(
    template_box: &ListNode,
    mut rule_node: RuleNode,
    indent: i32,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Node {
    if is_running(rule_node.width) {
        rule_node.width = template_box.width;
    }
    if is_running(rule_node.height) {
        rule_node.height = template_box.height;
    }
    if is_running(rule_node.depth) {
        rule_node.depth = template_box.depth;
    }
    // If the rule needs to be indented, we pack it into a ListNode which we can indent.
    if indent != 0 {
        let mut boxed_rule = hpack(
            vec![Node::Rule(rule_node)],
            TargetSpec::Natural,
            eqtb,
            logger,
        );
        boxed_rule.shift_amount = indent;
        Node::List(boxed_rule)
    } else {
        Node::Rule(rule_node)
    }
}

/// See 807.
fn set_row_and_unset_boxes_in_it(
    template_box: &ListNode,
    mut row: UnsetNode,
    indent: i32,
    nest: &SemanticState,
) -> ListNode {
    let mut cell_index = 1;
    let mut template_cell_index = 1;
    loop {
        set_glue_node_and_change_it_from_unset_node(
            template_box,
            &mut row,
            &mut cell_index,
            &mut template_cell_index,
            nest,
        );
        cell_index += 2;
        template_cell_index += 2;
        if cell_index >= row.list.len() {
            break;
        }
    }
    let packed_list = row.list;

    let mut packed_box = ListNode {
        list: HlistOrVlist::Hlist(Vec::new()),
        width: row.width,
        height: row.height,
        depth: row.depth,
        shift_amount: indent,
        glue_set: template_box.glue_set,
        glue_sign: template_box.glue_sign,
        glue_order: template_box.glue_order,
    };
    if let RichMode::Vertical(_) = nest.mode() {
        packed_box.list = HlistOrVlist::Hlist(packed_list);
        packed_box.width = template_box.width;
    } else {
        packed_box.list = HlistOrVlist::Vlist(packed_list);
        packed_box.height = template_box.height;
    }
    packed_box
}

/// See 808.
fn set_glue_node_and_change_it_from_unset_node(
    template_box: &ListNode,
    row: &mut UnsetNode,
    cell_index: &mut usize,
    template_cell_index: &mut usize,
    nest: &SemanticState,
) {
    let template_list = match &template_box.list {
        HlistOrVlist::Hlist(list) | HlistOrVlist::Vlist(list) => list,
    };
    let cell = match &mut row.list[*cell_index] {
        Node::Unset(cell) => cell,
        _ => panic!("Should not happen"),
    };
    let template_cell = match &template_list[*template_cell_index] {
        Node::Unset(cell) => cell,
        _ => panic!("Should not happen"),
    };

    let mut cell_span = cell.span_count;
    let mut inner_cell_size = template_cell.width;
    let outer_cell_size = inner_cell_size;
    let mut appendix = Vec::new();
    while cell_span > 0 {
        cell_span -= 1;
        append_tabskip_glue_and_empty_box_to_list(
            template_box,
            template_cell_index,
            &mut appendix,
            &mut inner_cell_size,
            nest,
        );
    }
    let cell = match std::mem::replace(
        &mut row.list[*cell_index],
        Node::List(ListNode::new_empty_hbox()),
    ) {
        Node::Unset(cell) => cell,
        _ => panic!("Should not happen"),
    };
    let packed_cell = if let RichMode::Vertical(_) = nest.mode() {
        make_unset_node_into_hlist_node(
            cell,
            row.height,
            row.depth,
            inner_cell_size,
            outer_cell_size,
        )
    } else {
        make_unset_node_into_vlist_node(cell, row.width, inner_cell_size, outer_cell_size)
    };
    row.list[*cell_index] = Node::List(packed_cell);
    if !appendix.is_empty() {
        for node in appendix {
            *cell_index += 1;
            row.list.insert(*cell_index, node);
        }
    }
}

/// See 809.
fn append_tabskip_glue_and_empty_box_to_list(
    template_box: &ListNode,
    template_cell_index: &mut usize,
    appendix: &mut Vec<Node>,
    t: &mut i32,
    nest: &SemanticState,
) {
    let template_list = match &template_box.list {
        HlistOrVlist::Hlist(list) | HlistOrVlist::Vlist(list) => list,
    };

    // Copy the right glue.
    *template_cell_index += 1;
    let right_glue = match &template_list[*template_cell_index] {
        Node::Glue(glue_node) => glue_node.glue_spec.clone(),
        _ => panic!("Should not happen"),
    };
    *t += right_glue.width;
    if template_box.glue_sign == GlueSign::Stretching {
        if right_glue.stretch.order == template_box.glue_order {
            *t += round(template_box.glue_set * right_glue.stretch.value as f64);
        }
    } else if template_box.glue_sign == GlueSign::Shrinking {
        if right_glue.shrink.order == template_box.glue_order {
            *t -= round(template_box.glue_set * right_glue.shrink.value as f64);
        }
    }
    let mut glue_copy = GlueNode::new(right_glue);
    glue_copy.subtype = GlueType::Skip(SkipVariable::TabSkip);
    appendix.push(Node::Glue(glue_copy));

    // Copy an empty box.
    *template_cell_index += 1;
    let next_cell = match &template_list[*template_cell_index] {
        Node::Unset(unset_node) => unset_node,
        _ => panic!("Should not happen"),
    };
    *t += next_cell.width;
    let mut empty_box = ListNode::new_empty_hbox();
    if let RichMode::Vertical(_) = nest.mode() {
        empty_box.list = HlistOrVlist::Hlist(Vec::new());
        empty_box.width = next_cell.width;
    } else {
        empty_box.list = HlistOrVlist::Vlist(Vec::new());
        empty_box.height = next_cell.width;
    }
    appendix.push(Node::List(empty_box));
}

/// See 810.
fn make_unset_node_into_hlist_node(
    cell: UnsetNode,
    row_height: Dimension,
    row_depth: Dimension,
    inner_cell_size: i32,
    outer_cell_size: i32,
) -> ListNode {
    let glue_sign;
    let glue_order;
    let glue_set;
    if inner_cell_size == cell.width {
        glue_sign = GlueSign::Normal;
        glue_order = DimensionOrder::Normal;
        glue_set = 0.0;
    } else if inner_cell_size > cell.width {
        glue_sign = GlueSign::Stretching;
        glue_order = cell.stretch.order;
        if cell.stretch.value == 0 {
            glue_set = 0.0;
        } else {
            glue_set = (inner_cell_size - cell.width) as f64 / cell.stretch.value as f64;
        }
    } else {
        glue_sign = GlueSign::Shrinking;
        glue_order = cell.shrink.order;
        if cell.shrink.value == 0 {
            glue_set = 0.0;
        } else if glue_order == DimensionOrder::Normal
            && cell.width - inner_cell_size > cell.shrink.value
        {
            glue_set = 1.0;
        } else {
            glue_set = (cell.width - inner_cell_size) as f64 / cell.shrink.value as f64;
        }
    }
    ListNode {
        list: HlistOrVlist::Hlist(cell.list),
        width: outer_cell_size,
        height: row_height,
        depth: row_depth,
        glue_sign,
        glue_order,
        glue_set,
        shift_amount: 0,
    }
}

/// See 811.
fn make_unset_node_into_vlist_node(
    cell: UnsetNode,
    row_width: Dimension,
    inner_cell_size: i32,
    outer_cell_size: i32,
) -> ListNode {
    let glue_sign;
    let glue_order;
    let glue_set;
    if inner_cell_size == cell.height {
        glue_sign = GlueSign::Normal;
        glue_order = DimensionOrder::Normal;
        glue_set = 0.0;
    } else if inner_cell_size > cell.height {
        glue_sign = GlueSign::Stretching;
        glue_order = cell.stretch.order;
        if cell.stretch.value == 0 {
            glue_set = 0.0;
        } else {
            glue_set = (inner_cell_size - cell.height) as f64 / cell.stretch.value as f64;
        }
    } else {
        glue_sign = GlueSign::Shrinking;
        glue_order = cell.shrink.order;
        if cell.shrink.value == 0 {
            glue_set = 0.0;
        } else if glue_order == DimensionOrder::Normal
            && cell.height - inner_cell_size > cell.shrink.value
        {
            glue_set = 1.0;
        } else {
            glue_set = (cell.height - inner_cell_size) as f64 / cell.shrink.value as f64;
        }
    }
    ListNode {
        list: HlistOrVlist::Vlist(cell.list),
        width: row_width,
        height: outer_cell_size,
        depth: cell.depth,
        glue_sign,
        glue_order,
        glue_set,
        shift_amount: 0,
    }
}

/// See 812.
fn insert_current_list_into_its_environment(
    hyphenator: &mut Hyphenator,
    page_builder: &mut PageBuilder,
    output: &mut Output,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let old_level = nest.pop_nest(eqtb);
    if let RichMode::Math(_) = nest.mode() {
        let RichMode::Vertical(former_vmode) = old_level.mode else {
            panic!("We expected the inner state to be vertical in display math mode");
        };
        let vlist = former_vmode.list;
        let (unexpandable_command, token) = do_assignments(
            hyphenator,
            page_builder,
            output,
            nest,
            scanner,
            eqtb,
            logger,
        );
        if let UnexpandableCommand::MathShift(_) = unexpandable_command {
            check_that_another_mathshift_symbol_follows(scanner, eqtb, logger);
        } else {
            pontificate_about_improper_alignment(token, scanner, eqtb, logger);
        }
        finish_alignment_in_display(
            vlist,
            former_vmode.prev_depth,
            hyphenator,
            page_builder,
            output,
            nest,
            scanner,
            eqtb,
            logger,
        );
    } else {
        // Fill the current parameters with the ones from the inner align level.
        if let RichMode::Horizontal(current_hmode) = nest.mode_mut() {
            let RichMode::Horizontal(former_hmode) = old_level.mode else {
                panic!("We expected the inner state to be horizontal as well");
            };
            current_hmode.set_space_factor(former_hmode.space_factor, eqtb);
            nest.tail_append(former_hmode.list, eqtb);
        } else if let RichMode::Vertical(current_vmode) = nest.mode_mut() {
            let RichMode::Vertical(former_vmode) = old_level.mode else {
                panic!("We expected the inner state to be vertical as well");
            };
            current_vmode.set_prev_depth(former_vmode.prev_depth, eqtb);
            nest.tail_append(former_vmode.list, eqtb);

            let RichMode::Vertical(current_vmode) = nest.mode_mut() else {
                panic!("Invariant");
            };
            if !current_vmode.internal {
                page_builder.build_page(output, nest, scanner, eqtb, logger);
            }
        }
    }
}

/// See 1206.
fn finish_alignment_in_display(
    cur_list: Vec<Node>,
    inner_prev_depth: Dimension,
    hyphenator: &mut Hyphenator,
    page_builder: &mut PageBuilder,
    output: &mut Output,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    nest.pop_nest(eqtb);
    let pre_display_penalty = *eqtb.integers.get(IntegerVariable::PreDisplayPenalty);
    nest.tail_push(Node::Penalty(PenaltyNode::new(pre_display_penalty)), eqtb);
    nest.tail_push(
        Node::Glue(GlueNode::new_param(SkipVariable::AboveDisplaySkip, eqtb)),
        eqtb,
    );
    nest.tail_append(cur_list, eqtb);
    let post_display_penalty = *eqtb.integers.get(IntegerVariable::PostDisplayPenalty);
    nest.tail_push(Node::Penalty(PenaltyNode::new(post_display_penalty)), eqtb);
    nest.tail_push(
        Node::Glue(GlueNode::new_param(SkipVariable::BelowDisplaySkip, eqtb)),
        eqtb,
    );
    let RichMode::Vertical(outer_vmode) = nest.mode_mut() else {
        panic!("We expect the outer mode to be a vertical mode");
    };
    outer_vmode.set_prev_depth(inner_prev_depth, eqtb);
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

/// See 1207.
fn pontificate_about_improper_alignment(
    token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    logger.print_err("Missing $$ inserted");
    let help = &[
        "Displays can use special alignments (like \\eqalignno)",
        "only if nothing but the alignment itself is between $$'s.",
    ];
    scanner.back_error(token, help, eqtb, logger)
}
