use crate::dimension::{
    scan_dimen_with_given_value, scan_higher_order_dimen, scan_mu_dimen, scan_normal_dimen,
};
use crate::eqtb::Eqtb;
use crate::error::mu_error;
use crate::input::Scanner;
use crate::logger::Logger;
use crate::nodes::GlueSpec;
use crate::scan_internal::{scan_internal_glue, scan_internal_mu_glue, InternalValue};

use std::rc::Rc;

/// Returns a pointer to the scanned glue.
/// See 461. and 430.
pub fn scan_glue(
    mu: bool,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Rc<GlueSpec> {
    let (unexpandable_command, token, negative) =
        scanner.get_next_non_blank_non_sign_token(eqtb, logger);
    let value = if let Some(internal_command) = unexpandable_command.try_to_internal() {
        let mut value = if mu {
            scan_internal_mu_glue(internal_command, token, scanner, eqtb, logger)
        } else {
            scan_internal_glue(internal_command, token, scanner, eqtb, logger)
        };
        if negative {
            match &mut value {
                InternalValue::Glue(glue_spec) | InternalValue::MuGlue(glue_spec) => {
                    let negated_glue = glue_spec.negate();
                    *glue_spec = Rc::new(negated_glue);
                }
                InternalValue::Int(value) | InternalValue::Dimen(value) => *value = -*value,
                _ => {}
            }
        }
        match value {
            InternalValue::MuGlue(glue_spec) => {
                if !mu {
                    mu_error(scanner, eqtb, logger);
                }
                return glue_spec;
            }
            InternalValue::Glue(glue_spec) => {
                if mu {
                    mu_error(scanner, eqtb, logger);
                }
                return glue_spec;
            }
            InternalValue::Dimen(w) => {
                if mu {
                    mu_error(scanner, eqtb, logger);
                }
                w
            }
            InternalValue::Int(w) => scan_dimen_with_given_value(mu, w, scanner, eqtb, logger),
            _ => panic!("Impossible"),
        }
    } else {
        scanner.back_input(token, eqtb, logger);
        let mut value = if mu {
            scan_mu_dimen(scanner, eqtb, logger)
        } else {
            scan_normal_dimen(scanner, eqtb, logger)
        };
        if negative {
            value = -value;
        }
        value
    };
    create_new_glue_specification_whose_width_is_cur_val(value, mu, scanner, eqtb, logger)
}

/// See 462.
fn create_new_glue_specification_whose_width_is_cur_val(
    value: i32,
    mu: bool,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Rc<GlueSpec> {
    let mut glue_spec = GlueSpec::ZERO_GLUE;
    glue_spec.width = value;
    if scanner.scan_keyword(b"plus", eqtb, logger) {
        glue_spec.stretch = scan_higher_order_dimen(mu, scanner, eqtb, logger);
    }
    if scanner.scan_keyword(b"minus", eqtb, logger) {
        glue_spec.shrink = scan_higher_order_dimen(mu, scanner, eqtb, logger);
    }
    Rc::new(glue_spec)
}
