use crate::command::UnexpandableCommand;
use crate::eqtb::{ControlSequence, Eqtb};
use crate::input::expansion::get_x_token;
use crate::input::Scanner;
use crate::logger::Logger;
use crate::scan_internal::scan_internal_integer;
use crate::token::Token;

pub type Integer = i32;

/// See 445.
const INFINITY: i32 = 0o17777777777;

/// Helper trait to extend Integer.
pub trait IntegerExt
where
    Self: Sized,
{
    fn scan_int(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) -> Self;
    fn scan_int_radix(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) -> (Self, u8);
    fn scan_four_bit_int(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) -> Self;
    fn scan_fifteen_bit_int(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) -> Self;
    fn scan_twenty_seven_bit_int(
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> Self;
}

impl IntegerExt for Integer {
    /// Returns the scanned integer.
    /// See 440.
    fn scan_int(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) -> i32 {
        let (value, _) = Self::scan_int_radix(scanner, eqtb, logger);
        value
    }

    /// Returns the scanned integer and radix.
    /// See 440.
    fn scan_int_radix(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) -> (i32, u8) {
        let (unexpandable_command, token, negative) =
            scanner.get_next_non_blank_non_sign_token(eqtb, logger);
        let mut value;
        let radix;
        if token == Token::ALPHA_TOKEN {
            value = scan_alphabetic_character_code(scanner, eqtb, logger);
            radix = 0;
        } else if let Some(internal_command) = unexpandable_command.try_to_internal() {
            value = scan_internal_integer(internal_command, token, scanner, eqtb, logger);
            radix = 0;
        } else {
            let (v, r) = scan_numeric_constant(unexpandable_command, token, scanner, eqtb, logger);
            value = v;
            radix = r;
        }

        if negative {
            value = -value;
        }
        (value, radix)
    }

    /// See 435.
    fn scan_four_bit_int(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) -> Self {
        let value = Self::scan_int(scanner, eqtb, logger);
        if value < 0 || value > 15 {
            logger.print_err("Bad number");
            let help = &[
                "Since I expected to read a number between 0 and 15,",
                "I changed this one to zero.",
            ];
            logger.int_error(value, help, scanner, eqtb);
            return 0;
        }
        value
    }

    /// See 436.
    fn scan_fifteen_bit_int(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) -> Self {
        let value = Self::scan_int(scanner, eqtb, logger);
        if value < 0 || value > 0o77777 {
            logger.print_err("Bad mathchar");
            let help = &[
                "A mathchar number must be between 0 and 32767.",
                "I changed this one to zero.",
            ];
            logger.int_error(value, help, scanner, eqtb);
            return 0;
        }
        value
    }

    /// See 437.
    fn scan_twenty_seven_bit_int(
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> Self {
        let value = Self::scan_int(scanner, eqtb, logger);
        if value < 0 || value > 0o777777777 {
            logger.print_err("Bad delimiter code");
            let help = &[
                "A numeric delimiter code must be between 0 and 2^{27}-1.",
                "I changed this one to zero.",
            ];
            logger.int_error(value, help, scanner, eqtb);
            return 0;
        }
        value
    }
}

/// See 442.
fn scan_alphabetic_character_code(
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> i32 {
    let token = scanner.get_token(eqtb, logger);
    let character_code = match token {
        Token::LeftBrace(c) => {
            scanner.align_state -= 1;
            Some(c as i32)
        }
        Token::RightBrace(c) => {
            scanner.align_state += 1;
            Some(c as i32)
        }
        Token::MathShift(c)
        | Token::TabMark(c)
        | Token::MacParam(c)
        | Token::SuperMark(c)
        | Token::SubMark(c)
        | Token::Spacer(c)
        | Token::Letter(c)
        | Token::OtherChar(c) => Some(c as i32),

        Token::CSToken { cs } => match cs {
            ControlSequence::Active(c) => Some(c as i32),
            ControlSequence::Single(c) => Some(c as i32),
            _ => None,
        },
        Token::Null => {
            panic!("Should not appear here")
        }
    };
    let Some(code) = character_code else {
        logger.print_err("Improper alphabetic constant");
        let help = &[
            "A one-character control sequence belongs after a ` mark.",
            "So I'm essentially inserting \\0 here.",
        ];
        scanner.back_error(token, help, eqtb, logger);
        return b'0' as i32;
    };
    scanner.scan_optional_space(eqtb, logger);
    code
}

/// Returns the scanned value and the radix.
/// The radix is zero for non-numeric constants
/// and otherwise 8, 10, or 16.
/// See 444. and 446.
fn scan_numeric_constant(
    mut unexpandable_command: UnexpandableCommand,
    mut token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> (i32, u8) {
    let radix;
    let (value, unexpandable_command, token, vacuous) = match token {
        Token::OCTAL_TOKEN => {
            radix = 8;
            let m = 0o2000000000; // (1 << 31) / RADIX
            (unexpandable_command, token) = get_x_token(scanner, eqtb, logger);
            accumulate_constant(radix, m, unexpandable_command, token, scanner, eqtb, logger)
        }
        Token::HEX_TOKEN => {
            radix = 16;
            let m = 0o1000000000; // (1 << 31) / RADIX
            (unexpandable_command, token) = get_x_token(scanner, eqtb, logger);
            accumulate_constant(radix, m, unexpandable_command, token, scanner, eqtb, logger)
        }
        _ => {
            radix = 10;
            let m = 214748364; // (1 << 31) / RADIX
            accumulate_constant(radix, m, unexpandable_command, token, scanner, eqtb, logger)
        }
    };
    if vacuous {
        logger.print_err("Missing number, treated as zero");
        let help = &[
            "A number should have been here; I inserted `0'.",
            "(If you can't figure out why I needed to see a number,",
            "look up `weird error' in the index to The TeXbook.)",
        ];
        scanner.back_error(token, help, eqtb, logger)
    } else if let UnexpandableCommand::Spacer = unexpandable_command {
        // Do nothing.
    } else {
        scanner.back_input(token, eqtb, logger);
    }
    (value, radix)
}

/// Returns the value, the finishing token, and whether no digit where scanned at all.
/// See 445.
fn accumulate_constant(
    radix: u8,
    m: i32,
    mut unexpandable_command: UnexpandableCommand,
    mut token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> (i32, UnexpandableCommand, Token, bool) {
    let mut digit;
    let mut vacuous = true;
    let mut ok_so_far = true;
    let mut value = 0;
    loop {
        let number = match token {
            Token::OtherChar(c @ b'0'..=b'9') => c - b'0',
            Token::Letter(c @ b'A'..=b'F') | Token::OtherChar(c @ b'A'..=b'F') => c - b'A' + 10,
            _ => break,
        };
        if number < radix {
            digit = number;
        } else {
            break;
        }

        vacuous = false;
        if value >= m && (value > m || digit > 7 || radix != 10) {
            if ok_so_far {
                logger.print_err("Number too big");
                let help = &[
                    "I can only go up to 2147483647='17777777777=\"7FFFFFFF,",
                    "so I'm using that number instead of yours.",
                ];
                logger.error(help, scanner, eqtb);
                value = INFINITY;
                ok_so_far = false;
            }
        } else {
            value = value * radix as i32 + digit as i32;
        }
        (unexpandable_command, token) = get_x_token(scanner, eqtb, logger);
    }
    (value, unexpandable_command, token, vacuous)
}
