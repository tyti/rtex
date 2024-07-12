use crate::command::{InternalCommand, UnexpandableCommand};
use crate::eqtb::{Eqtb, IntegerVariable};
use crate::error::mu_error;
use crate::input::expansion::get_x_token;
use crate::input::Scanner;
use crate::integer::{Integer, IntegerExt};
use crate::logger::Logger;
use crate::nodes::{DimensionOrder, HigherOrderDimension};
use crate::print::Printer;
use crate::scaled::{nx_plus_y, xn_over_d, xn_over_d_with_remainder, Scaled, TWO, UNITY};
use crate::scan_internal::{scan_internal_dimension, scan_internal_mu_glue, InternalValue};
use crate::token::Token;

pub const MAX_DIMEN: i32 = 0o7777777777;
pub const NULL_FLAG: i32 = -0o10000000000;

pub type Dimension = i32;

/// See 138.
pub fn is_running(dim: i32) -> bool {
    dim == NULL_FLAG
}

enum IntOrDimen {
    Int(i32),
    Dimen(Dimension),
}

/// See 448.
pub fn scan_normal_dimen(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) -> Dimension {
    let (dimension, _) = scan_dimen(false, false, scanner, eqtb, logger);
    dimension
}

/// See 448.
pub fn scan_mu_dimen(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) -> Dimension {
    let (dimension, _) = scan_dimen(true, false, scanner, eqtb, logger);
    dimension
}

pub fn scan_higher_order_dimen(
    mu: bool,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> HigherOrderDimension {
    let (value, order) = scan_dimen(mu, true, scanner, eqtb, logger);
    HigherOrderDimension { order, value }
}

/// `mu` indicates that the unit should be mu's.
/// `inf` indicates that inifinite unite are allowed.
/// If `precomputed_value` contains an integer, we only scan the unit.
/// See 448.
fn scan_dimen(
    mu: bool,
    inf: bool,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> (Scaled, DimensionOrder) {
    // Read any possibly leading signs and the first following token.
    let (unexpandable_command, token, mut negative) =
        scanner.get_next_non_blank_non_sign_token(eqtb, logger);

    let (mut integer, fraction) =
        if let Some(internal_command) = unexpandable_command.try_to_internal() {
            let int_or_dimen = if mu {
                fetch_internal_mu_dimension(internal_command, token, scanner, eqtb, logger)
            } else {
                fetch_internal_dimension(internal_command, token, scanner, eqtb, logger)
            };
            let integer = match int_or_dimen {
                IntOrDimen::Dimen(dimen) => {
                    return attach_sign(
                        dimen,
                        DimensionOrder::Normal,
                        negative,
                        scanner,
                        eqtb,
                        logger,
                    )
                }
                IntOrDimen::Int(integer) => integer,
            };
            (integer, 0)
        } else {
            scanner.back_input(token, eqtb, logger);
            scan_integer_and_fraction(token, scanner, eqtb, logger)
        };

    if integer < 0 {
        negative = !negative;
        integer = -integer;
    }

    let mut order = DimensionOrder::Normal;
    let dimension = if inf {
        order = scan_for_fil_units(scanner, eqtb, logger);
        if let DimensionOrder::Normal = order {
            if mu {
                scan_mu_units(integer, fraction, scanner, eqtb, logger)
            } else {
                scan_units(integer, fraction, scanner, eqtb, logger)
            }
        } else {
            scanner.scan_optional_space(eqtb, logger);
            // attach_fraction:
            if integer >= 0o40000 {
                report_out_of_range_error(scanner, eqtb, logger);
                MAX_DIMEN
            } else {
                integer * UNITY + fraction
            }
        }
    } else {
        if mu {
            scan_mu_units(integer, fraction, scanner, eqtb, logger)
        } else {
            scan_units(integer, fraction, scanner, eqtb, logger)
        }
    };

    attach_sign(dimension, order, negative, scanner, eqtb, logger)
}

/// Scan a fractional number.
/// See 448.
fn scan_integer_and_fraction(
    token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> (i32, i32) {
    // If the number starts with a point or comma, the integral part is zero and the radix is 10.
    let (integer, radix) = if let Token::POINT_TOKEN | Token::CONTINENTAL_POINT_TOKEN = token {
        (0, 10)
    } else {
        // Otherwise determine the integral part and the radix.
        Integer::scan_int_radix(scanner, eqtb, logger)
    };

    // Only in the case of decimal numbers are fractional parts allowed.
    let fraction = if radix == 10 {
        let (_, token) = get_x_token(scanner, eqtb, logger);
        if let Token::POINT_TOKEN | Token::CONTINENTAL_POINT_TOKEN = token {
            scan_decimal_fraction(scanner, eqtb, logger)
        } else {
            scanner.back_input(token, eqtb, logger);
            0
        }
    } else {
        0
    };
    (integer, fraction)
}

/// See 460.
fn report_out_of_range_error(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) {
    logger.print_err("Dimension too large");
    let help = &[
        "I can't work with sizes bigger than about 19 feet.",
        "Continue and I'll use the largest value I can.",
    ];
    logger.error(help, scanner, eqtb)
}

fn attach_sign(
    mut dimension: Dimension,
    order: DimensionOrder,
    negative: bool,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> (Dimension, DimensionOrder) {
    if dimension.abs() >= 0o10000000000 {
        report_out_of_range_error(scanner, eqtb, logger);
        dimension = MAX_DIMEN;
    }
    if negative {
        (-dimension, order)
    } else {
        (dimension, order)
    }
}

/// Scans an internal dimension.
/// Returns the value and true if we already have the unit
/// or false if we still need to scan the unit.
/// See 449.
fn fetch_internal_dimension(
    internal_command: InternalCommand,
    token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> IntOrDimen {
    let value = scan_internal_dimension(internal_command, token, scanner, eqtb, logger);
    match value {
        InternalValue::Dimen(w) => IntOrDimen::Dimen(w),
        InternalValue::Int(w) => IntOrDimen::Int(w),
        _ => panic!("Impossible"),
    }
}

/// Scans an internal mu dimension.
/// Returns the value and true if we already have the unit
/// or false if we still need to scan the unit.
/// See 449.
fn fetch_internal_mu_dimension(
    internal_command: InternalCommand,
    token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> IntOrDimen {
    let value = scan_internal_mu_glue(internal_command, token, scanner, eqtb, logger);
    match value {
        InternalValue::MuGlue(glue_spec) => {
            let w = glue_spec.width;
            IntOrDimen::Dimen(w)
        }
        InternalValue::Glue(glue_spec) => {
            let w = glue_spec.width;
            mu_error(scanner, eqtb, logger);
            IntOrDimen::Int(w)
        }
        InternalValue::Dimen(w) => {
            mu_error(scanner, eqtb, logger);
            IntOrDimen::Int(w)
        }
        InternalValue::Int(w) => IntOrDimen::Int(w),
        _ => panic!("Impossible"),
    }
}

/// See 452.
fn scan_decimal_fraction(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) -> Scaled {
    // We store the decimal digits here.
    let mut digs = Vec::new();
    // Get the decimal digits.
    let (mut unexpandable_command, mut token);
    loop {
        (unexpandable_command, token) = get_x_token(scanner, eqtb, logger);
        let dig = match token {
            Token::OtherChar(digit @ b'0'..=b'9') => digit - b'0',
            _ => break,
        };
        // We only need to consider the first 17 digits after the period.
        if digs.len() < 17 {
            digs.push(dig);
        }
    }
    // Create a Scaled from the decimal digits.
    let fraction = round_decimals(&digs);
    if let UnexpandableCommand::Spacer = unexpandable_command {
        // Consume the space.
    } else {
        scanner.back_input(token, eqtb, logger);
    }
    fraction
}

/// Returns true for goto attach_sign, false otherwise.
/// See 453. and 457.
fn scan_units(
    mut integer: i32,
    mut fraction: i32,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Dimension {
    if let Some(scaled) = scan_for_units_that_are_internal_dimensions(scanner, eqtb, logger) {
        let scaled_fraction =
            xn_over_d(scaled, fraction, UNITY).expect("fraction is at most UNITY");
        let Ok(dimension) = nx_plus_y(integer, scaled, scaled_fraction) else {
            report_out_of_range_error(scanner, eqtb, logger);
            return MAX_DIMEN;
        };
        return dimension;
    }
    let mut arith_error = false;
    if scanner.scan_keyword(b"true", eqtb, logger) {
        // See 457.
        eqtb.prepare_mag(scanner, logger);
        let mag = eqtb.integer(IntegerVariable::Mag);
        if mag != 1000 {
            if let Ok((v, remainder)) = xn_over_d_with_remainder(integer, 1000, mag) {
                integer = v;
                fraction = (1000 * fraction + UNITY * remainder) / mag;
                integer += fraction / UNITY;
                fraction %= UNITY;
            } else {
                // NOTE If we had an overflow here, we still want to finish reading the units.
                arith_error = true;
            }
        }
    }
    if !scanner.scan_keyword(b"pt", eqtb, logger) {
        if let Some((num, denom)) = scan_for_other_units(scanner, eqtb, logger) {
            let Ok((v, remainder)) = xn_over_d_with_remainder(integer, num, denom) else {
                scanner.scan_optional_space(eqtb, logger);
                report_out_of_range_error(scanner, eqtb, logger);
                return MAX_DIMEN;
            };
            integer = v;
            fraction = (num * fraction + UNITY * remainder) / denom;
            integer += fraction / UNITY;
            fraction %= UNITY;
        } else if scanner.scan_keyword(b"sp", eqtb, logger) {
            scanner.scan_optional_space(eqtb, logger);
            return integer;
        } else {
            complain_about_unknown_unit(scanner, eqtb, logger);
        }
    }
    scanner.scan_optional_space(eqtb, logger);
    // attach_fraction:
    if integer >= 0o40000 || arith_error {
        report_out_of_range_error(scanner, eqtb, logger);
        MAX_DIMEN
    } else {
        integer * UNITY + fraction
    }
}

/// Returns true for goto attach_sign, false otherwise.
/// See 453.
fn scan_mu_units(
    integer: i32,
    fraction: i32,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Dimension {
    if let Some(scaled) = scan_for_mu_units_that_are_internal_dimensions(scanner, eqtb, logger) {
        let scaled_fraction =
            xn_over_d(scaled, fraction, UNITY).expect("fraction is at most UNITY");
        let Ok(dimension) = nx_plus_y(integer, scaled, scaled_fraction) else {
            report_out_of_range_error(scanner, eqtb, logger);
            return MAX_DIMEN;
        };
        return dimension;
    }
    scan_for_mu_units(scanner, eqtb, logger);
    scanner.scan_optional_space(eqtb, logger);
    // attach_fraction:
    if integer >= 0o40000 {
        report_out_of_range_error(scanner, eqtb, logger);
        MAX_DIMEN
    } else {
        integer * UNITY + fraction
    }
}

/// Return the "fil" type if any is found.
/// See 454.
fn scan_for_fil_units(
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> DimensionOrder {
    if scanner.scan_keyword(b"fil", eqtb, logger) {
        let mut l_count = 1;
        while scanner.scan_keyword(b"l", eqtb, logger) {
            if l_count == 3 {
                logger.print_err("Illegal unit of measure (");
                logger.print_str("replaced by filll)");
                let help = &["I dddon't go any higher than filll."];
                logger.error(help, scanner, eqtb);
            } else {
                l_count += 1;
            }
        }
        match l_count {
            1 => DimensionOrder::Fil,
            2 => DimensionOrder::Fill,
            3 => DimensionOrder::Filll,
            _ => panic!("Should not be possible"),
        }
    } else {
        DimensionOrder::Normal
    }
}

/// See 455.
fn scan_for_units_that_are_internal_dimensions(
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Option<i32> {
    let (unexpandable_command, token) = scanner.get_next_non_blank_non_call_token(eqtb, logger);
    let scaled;
    if let Some(internal_command) = unexpandable_command.try_to_internal() {
        scaled = match scan_internal_dimension(internal_command, token, scanner, eqtb, logger) {
            InternalValue::Dimen(w) => w,
            InternalValue::Int(w) => w,
            _ => panic!("Impossible"),
        };
    } else {
        scanner.back_input(token, eqtb, logger);
        scaled = if scanner.scan_keyword(b"em", eqtb, logger) {
            eqtb.em_width_for_cur_font()
        } else if scanner.scan_keyword(b"ex", eqtb, logger) {
            eqtb.x_height_for_cur_font()
        } else {
            return None;
        };
        scanner.scan_optional_space(eqtb, logger);
    }
    Some(scaled)
}

/// See 455.
fn scan_for_mu_units_that_are_internal_dimensions(
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Option<Scaled> {
    let (unexpandable_command, token) = scanner.get_next_non_blank_non_call_token(eqtb, logger);
    if let Some(internal_command) = unexpandable_command.try_to_internal() {
        let scaled = match scan_internal_mu_glue(internal_command, token, scanner, eqtb, logger) {
            InternalValue::MuGlue(glue_spec) => glue_spec.width,
            InternalValue::Glue(glue_spec) => {
                let w = glue_spec.width;
                mu_error(scanner, eqtb, logger);
                w
            }
            InternalValue::Dimen(w) => {
                mu_error(scanner, eqtb, logger);
                w
            }
            InternalValue::Int(w) => {
                mu_error(scanner, eqtb, logger);
                w
            }
            _ => panic!("Impossible"),
        };
        Some(scaled)
    } else {
        scanner.back_input(token, eqtb, logger);
        None
    }
}

/// See 456.
fn scan_for_mu_units(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) {
    if !scanner.scan_keyword(b"mu", eqtb, logger) {
        logger.print_err("Illegal unit of measure (");
        logger.print_str("mu inserted)");
        let help = &[
            "The unit of measurement in math glue must be mu.",
            "To recover gracefully from this error, it's best to",
            "delete the erroneous units; e.g., type `2' to delete",
            "two letters. (See Chapter 27 of The TeXbook.)",
        ];
        logger.error(help, scanner, eqtb);
    }
}

/// See 458.
fn scan_for_other_units(
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Option<(i32, i32)> {
    let (num, denom) = if scanner.scan_keyword(b"in", eqtb, logger) {
        (7227, 100)
    } else if scanner.scan_keyword(b"pc", eqtb, logger) {
        (12, 1)
    } else if scanner.scan_keyword(b"cm", eqtb, logger) {
        (7227, 254)
    } else if scanner.scan_keyword(b"mm", eqtb, logger) {
        (7227, 2540)
    } else if scanner.scan_keyword(b"bp", eqtb, logger) {
        (7227, 7200)
    } else if scanner.scan_keyword(b"dd", eqtb, logger) {
        (1238, 1157)
    } else if scanner.scan_keyword(b"cc", eqtb, logger) {
        (14856, 1157)
    } else {
        return None;
    };
    Some((num, denom))
}

/// See 459.
fn complain_about_unknown_unit(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) {
    logger.print_err("Illegal unit of measure (");
    logger.print_str("pt inserted)");
    let help = &[
        "Dimensions can be in units of em, ex, in, pt, pc,",
        "cm, mm, dd, cc, bp, or sp; but yours is a new one!",
        "I'll assume that you meant to say pt, for printer's points.",
        "To recover gracefully from this error, it's best to",
        "delete the erroneous units; e.g., type `2' to delete",
        "two letters. (See Chapter 27 of The TeXbook.)",
    ];
    logger.error(help, scanner, eqtb)
}

/// `mu` indicates that the unit should be mu's.
/// See 448.
pub fn scan_dimen_with_given_value(
    mu: bool,
    mut integer: i32,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Scaled {
    let mut negative = false;

    if integer < 0 {
        negative = !negative;
        integer = -integer;
    }

    let dimension = if mu {
        scan_mu_units(integer, 0, scanner, eqtb, logger)
    } else {
        scan_units(integer, 0, scanner, eqtb, logger)
    };

    let (dimension, _) = attach_sign(
        dimension,
        DimensionOrder::Normal,
        negative,
        scanner,
        eqtb,
        logger,
    );
    dimension
}

/// Calculates a Scaled number from the fractional part of a decimal floating point number.
/// The digits need to be decimal, i.e. they need to be non-negative and less than 10.
/// The returned value will at most be `UNITY`.
/// See 102.
fn round_decimals(digs: &[u8]) -> Scaled {
    let mut a = 0;
    for &dig in digs.iter().rev() {
        a = (a + dig as i32 * TWO) / 10;
    }
    (a + 1) / 2
}
