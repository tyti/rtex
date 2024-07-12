use super::expansion::get_x_token;
use super::{Scanner, ScannerStatus};

use crate::command::{
    Command, ExpandableCommand, FiOrElse, IfTest, MathCommand, UnexpandableCommand,
};
use crate::dimension::scan_normal_dimen;
use crate::eqtb::{CatCode, ControlSequence, Eqtb, IntegerVariable};
use crate::integer::{Integer, IntegerExt};
use crate::logger::Logger;
use crate::print::Printer;
use crate::semantic_nest::Mode;
use crate::token::Token;

/// This is used to indicate which one of \or, \else, or \fi can appear now.
/// See 489.
#[derive(Clone, Copy, PartialEq)]
pub enum IfLimit {
    /// Not currently processing any conditional.
    Normal,
    /// Currently evaluating the truth value of an \if.
    If,
    /// Only a \fi is expected now.
    Fi,
    /// An \else or a \fi could appear.
    Else,
    /// An \or, an \else or a \fi could appear.
    Or,
}

/// See 489.
pub struct ConditionState {
    pub if_limit: IfLimit,
    pub cur_if: IfTest,
    pub if_line: usize,
}

impl Scanner {
    /// Consumes all Token until an \or, \else, or \fi at the current if-depth is found. Any \if's
    /// appearing before that matching FiOrElse token are "closed" by finding their corresponding
    /// \fi's. Returns matching FiOrElse.
    /// See 494.
    fn pass_text(&mut self, eqtb: &mut Eqtb, logger: &mut Logger) -> FiOrElse {
        let save_scanner_status =
            std::mem::replace(&mut self.scanner_status, ScannerStatus::Skipping);
        let mut depth = 0;
        self.skip_line = eqtb.line_number();
        let matching_fi_or_else = loop {
            let (command, _) = self.get_next(false, eqtb, logger);
            if let Command::Expandable(ExpandableCommand::FiOrElse(fi_or_else)) = command {
                if depth == 0 {
                    break fi_or_else;
                }
                if let FiOrElse::Fi = fi_or_else {
                    depth -= 1;
                }
            } else if let Command::Expandable(ExpandableCommand::IfTest(_)) = command {
                depth += 1;
            }
        };
        self.scanner_status = save_scanner_status;
        matching_fi_or_else
    }

    /// See 495.
    fn push_condition_stack(&mut self, if_test: IfTest, eqtb: &Eqtb) {
        self.cond_stack.push(ConditionState {
            if_limit: self.if_limit,
            cur_if: self.cur_if,
            if_line: self.if_line,
        });

        self.cur_if = if_test;
        self.if_limit = IfLimit::If;
        self.if_line = eqtb.line_number();
    }

    /// See 496.
    fn pop_condition_stack(&mut self) {
        let condition = self.cond_stack.pop().unwrap();
        self.if_limit = condition.if_limit;
        self.cur_if = condition.cur_if;
        self.if_line = condition.if_line;
    }

    /// Changes the if_limit of the if corresponding to `p` to `l`.
    /// 497.
    fn change_if_limit(&mut self, new_limit: IfLimit, condition_level: usize) {
        if condition_level == self.cond_stack.len() {
            self.if_limit = new_limit;
        } else if condition_level < self.cond_stack.len() {
            self.cond_stack[condition_level].if_limit = new_limit;
        } else {
            panic!("condition_level should not exceed condition levels");
        }
    }
}

/// See 498.
pub fn conditional(if_test: IfTest, scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) {
    scanner.push_condition_stack(if_test, eqtb);
    let condition_level = scanner.cond_stack.len();
    let this_if = if_test;
    let finishing_fi_or_else = if this_if == IfTest::IfCase {
        match select_appropriate_case(condition_level, scanner, eqtb, logger) {
            None => return,
            Some(finishing_chr) => finishing_chr,
        }
    } else {
        let b = process_ifcase(this_if, scanner, eqtb, logger);
        if eqtb.integer(IntegerVariable::TracingCommands) > 1 {
            display_value_of_b(b, eqtb, logger);
        }
        if b {
            scanner.change_if_limit(IfLimit::Else, condition_level);
            return;
        }
        skip_to_else_or_fi(condition_level, scanner, eqtb, logger)
    };
    // common_ending:
    if finishing_fi_or_else == FiOrElse::Fi {
        scanner.pop_condition_stack();
    } else {
        scanner.if_limit = IfLimit::Fi;
    }
}

/// Pass over input until an \else or \fi is found that corresponds
/// to the if level `save_cond_ptr`. Return the finishing FiOrElse.
/// See 500.
fn skip_to_else_or_fi(
    condition_level: usize,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> FiOrElse {
    loop {
        let fi_or_else = scanner.pass_text(eqtb, logger);
        if scanner.cond_stack.len() == condition_level {
            if fi_or_else != FiOrElse::Or {
                return fi_or_else;
            }
            logger.print_err("Extra ");
            logger.print_esc_str(b"or");
            let help = &["I'm ignoring this; it doesn't match any \\if."];
            logger.error(help, scanner, eqtb);
        } else if fi_or_else == FiOrElse::Fi {
            scanner.pop_condition_stack();
        }
    }
}

/// Return the truth value of the current if statement.
/// See 501.
fn process_ifcase(
    this_if: IfTest,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> bool {
    match this_if {
        IfTest::IfChar | IfTest::IfCat => {
            test_if_two_characters_match(this_if, scanner, eqtb, logger)
        }
        IfTest::IfInt | IfTest::IfDim => {
            test_relation_between_integers_or_dimensions(this_if, scanner, eqtb, logger)
        }
        IfTest::IfOdd => test_if_integer_is_odd(scanner, eqtb, logger),
        IfTest::IfVmode => eqtb.mode().base() == Mode::Vertical && !scanner.scanning_write_tokens,
        IfTest::IfHmode => eqtb.mode().base() == Mode::Horizontal && !scanner.scanning_write_tokens,
        IfTest::IfMmode => {
            eqtb.mode().base() == Mode::DisplayMath && !scanner.scanning_write_tokens
        }
        IfTest::IfInner => match eqtb.mode() {
            Mode::InternalVertical | Mode::RestrictedHorizontal | Mode::Math => {
                !scanner.scanning_write_tokens
            }
            _ => false,
        },
        IfTest::IfVoid | IfTest::IfHbox | IfTest::IfVbox => {
            test_box_register_status(this_if, scanner, eqtb, logger)
        }
        IfTest::Ifx => test_if_two_tokens_match(scanner, eqtb, logger),
        IfTest::IfEof => {
            let stream_number = Integer::scan_four_bit_int(scanner, eqtb, logger);
            scanner.read_file[stream_number as usize].is_none()
        }
        IfTest::IfTrue => true,
        IfTest::IfFalse => false,
        IfTest::IfCase => panic!("Impossible"),
    }
}

/// See 502.
fn display_value_of_b(b: bool, eqtb: &Eqtb, logger: &mut Logger) {
    logger.begin_diagnostic(eqtb.tracing_online());
    if b {
        logger.print_str("{true}");
    } else {
        logger.print_str("{false}");
    }
    logger.end_diagnostic(false);
}

/// Scans a <, =, or > relation between integers or dimensions.
/// Returns the truth value of that relation.
/// See 503.
fn test_relation_between_integers_or_dimensions(
    this_if: IfTest,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> bool {
    let n = if this_if == IfTest::IfInt {
        Integer::scan_int(scanner, eqtb, logger)
    } else {
        scan_normal_dimen(scanner, eqtb, logger)
    };
    let (_, token) = scanner.get_next_non_blank_non_call_token(eqtb, logger);
    let r = match token {
        Token::OtherChar(relation @ (b'<' | b'=' | b'>')) => relation,
        _ => {
            logger.print_err("Missing = inserted for ");
            this_if.display(logger);
            let help = &["I was expecting to see `<', `=', or `>'. Didn't."];
            scanner.back_error(token, help, eqtb, logger);
            b'='
        }
    };
    let m = if this_if == IfTest::IfInt {
        Integer::scan_int(scanner, eqtb, logger)
    } else {
        scan_normal_dimen(scanner, eqtb, logger)
    };
    match r {
        b'<' => n < m,
        b'=' => n == m,
        b'>' => n > m,
        _ => panic!("Impossible"),
    }
}

/// Scans an integer and returns whether the integer is odd.
/// See 504.
fn test_if_integer_is_odd(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) -> bool {
    let n = Integer::scan_int(scanner, eqtb, logger);
    n.rem_euclid(2) == 1
}

/// Scans a box register number and returns whether the corresponding
/// box is of the expected type.
/// See 505.
fn test_box_register_status(
    this_if: IfTest,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> bool {
    let register = scanner.scan_register_index(eqtb, logger);
    let p = eqtb.boks(register);
    if this_if == IfTest::IfVoid {
        p.is_none()
    } else {
        match p {
            None => false,
            Some(list_node) => {
                if this_if == IfTest::IfHbox {
                    list_node.list.is_hlist()
                } else {
                    list_node.list.is_vlist()
                }
            }
        }
    }
}

/// Gets the next unexpandable token and returns its command code and character code,
/// after mapping all non-character commands to the same values, the only exception
/// being expansion-protected active characters.
/// See 506.
fn get_x_token_or_active_char(
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> (CatCode, u16) {
    let (unexpandable_command, token) = get_x_token(scanner, eqtb, logger);
    match unexpandable_command {
        UnexpandableCommand::Relax { no_expand: true } => {
            let cs = match token {
                Token::CSToken { cs } => cs,
                _ => panic!("Impossible"),
            };
            // If the control sequence is an active char
            if let ControlSequence::Active(c) = cs {
                (CatCode::ActiveChar, c as u16)
            } else {
                (CatCode::Escape, 256)
            }
        }
        UnexpandableCommand::TabMark(c) => (CatCode::TabMark, c as u16),
        UnexpandableCommand::MacParam(c) => (CatCode::MacParam, c as u16),
        UnexpandableCommand::LeftBrace(c) => (CatCode::LeftBrace, c as u16),
        UnexpandableCommand::RightBrace(c) => (CatCode::RightBrace, c as u16),
        UnexpandableCommand::MathShift(c) => (CatCode::MathShift, c as u16),
        UnexpandableCommand::Math(MathCommand::Superscript(c)) => (CatCode::SupMark, c as u16),
        UnexpandableCommand::Math(MathCommand::Subscript(c)) => (CatCode::SubMark, c as u16),
        UnexpandableCommand::Spacer => (CatCode::Spacer, b' ' as u16),
        UnexpandableCommand::Letter(c) => (CatCode::Letter, c as u16),
        UnexpandableCommand::Other(c) => (CatCode::OtherChar, c as u16),
        // Everything else is mapped to one distinct value.
        _ => (CatCode::Escape, 256),
    }
}

/// See 506.
fn test_if_two_characters_match(
    this_if: IfTest,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> bool {
    let (m1, n1) = get_x_token_or_active_char(scanner, eqtb, logger);
    let (m2, n2) = get_x_token_or_active_char(scanner, eqtb, logger);
    if this_if == IfTest::IfChar {
        n1 == n2
    } else {
        m1 == m2
    }
}

/// See 507.
fn test_if_two_tokens_match(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) -> bool {
    let save_scanner_status = std::mem::replace(&mut scanner.scanner_status, ScannerStatus::Normal);
    let (first_command, _) = scanner.get_next(false, eqtb, logger);
    let (second_command, _) = scanner.get_next(false, eqtb, logger);
    scanner.scanner_status = save_scanner_status;
    first_command == second_command
}

/// Returns None if we found the correct case following
/// an \or, or Some(finishing_fi_or_else) if we encountered an \else or \fi.
/// See 509.
fn select_appropriate_case(
    condition_level: usize,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Option<FiOrElse> {
    let mut n = Integer::scan_int(scanner, eqtb, logger);
    if eqtb.integer(IntegerVariable::TracingCommands) > 1 {
        logger.begin_diagnostic(eqtb.tracing_online());
        logger.print_str("{case ");
        logger.print_int(n);
        logger.print_char(b'}');
        logger.end_diagnostic(false);
    }
    // Try to find the case corresponding to n.
    // When n reaches zero, we have reached that case.
    while n != 0 {
        let fi_or_else = scanner.pass_text(eqtb, logger);
        // We are only concerned with \or, \else and \fi of our level.
        if scanner.cond_stack.len() == condition_level {
            // If we found another \or.
            if fi_or_else == FiOrElse::Or {
                n -= 1;
            // We found an \else or \fi.
            } else {
                return Some(fi_or_else);
            }
        // We close any ifs that have been opened while scanning
        // for an int.
        } else if fi_or_else == FiOrElse::Fi {
            scanner.pop_condition_stack();
        }
    }
    scanner.change_if_limit(IfLimit::Or, condition_level);
    None
}

/// See 510.
pub fn terminate_current_conditional_and_skip_to_fi(
    mut fi_or_else: FiOrElse,
    token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    match (fi_or_else, scanner.if_limit) {
        // If an \or, \else, or \fi appears before we are done evaluating
        // the truth value of the \if, we push it back onto the stack and insert
        // a \relax.
        (_, IfLimit::If) => scanner.insert_relax(token, eqtb, logger),
        // If we are currently having no open \if, or if the type of FiOrElse
        // does not fit to the limit.
        (_, IfLimit::Normal)
        | (FiOrElse::Else, IfLimit::Fi)
        | (FiOrElse::Or, IfLimit::Fi)
        | (FiOrElse::Or, IfLimit::Else) => {
            logger.print_err("Extra ");
            fi_or_else.display(logger);
            let help = &["I'm ignoring this; it doesn't match any \\if."];
            logger.error(help, scanner, eqtb);
        }
        // If the \or, \else, or \fi is expected
        (FiOrElse::Or, IfLimit::Or)
        | (FiOrElse::Else, IfLimit::Or)
        | (FiOrElse::Else, IfLimit::Else)
        | (FiOrElse::Fi, IfLimit::Or)
        | (FiOrElse::Fi, IfLimit::Else)
        | (FiOrElse::Fi, IfLimit::Fi) => {
            // We already went through the correct clause. All that
            // is left to do is find the closing \fi, passing over
            // all valid or invalid \or and \else.
            while fi_or_else != FiOrElse::Fi {
                fi_or_else = scanner.pass_text(eqtb, logger);
            }
            scanner.pop_condition_stack();
        }
    }
}

impl Scanner {
    /// Clear the condition stack for final cleanup.
    /// See 1335.
    pub fn clear_cond_stack(&mut self, logger: &mut Logger) {
        while let Some(condition) = self.cond_stack.pop() {
            logger.print_nl_str("(");
            logger.print_esc_str(b"end occurred ");
            logger.print_str("when ");
            self.cur_if.display(logger);
            if self.if_line != 0 {
                logger.print_str(" on line ");
                logger.print_int(self.if_line as i32);
            }
            logger.print_str(" was incomplete)");
            self.if_line = condition.if_line;
            self.cur_if = condition.cur_if;
        }
    }
}
