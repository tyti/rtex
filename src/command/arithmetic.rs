use crate::command::{PrefixableCommand, UnexpandableCommand};
use crate::dimension::scan_normal_dimen;
use crate::eqtb::{DimensionVariable, Eqtb, IntegerVariable, SkipVariable};
use crate::format::{Dumpable, FormatError};
use crate::glue::scan_glue;
use crate::input::expansion::get_x_token;
use crate::input::Scanner;
use crate::integer::{Integer, IntegerExt};
use crate::logger::Logger;
use crate::print::Printer;
use crate::scaled::{mult_integers, nx_plus_y, ArithError, DimenMult};
use crate::scan_internal::ValueType;

use std::io::Write;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArithCommand {
    Advance,
    Multiply,
    Divide,
}

impl ArithCommand {
    pub fn display(&self, printer: &mut impl Printer) {
        match self {
            Self::Advance => printer.print_esc_str(b"advance"),
            Self::Divide => printer.print_esc_str(b"divide"),
            Self::Multiply => printer.print_esc_str(b"multiply"),
        }
    }

    /// See 1236. and 1237.
    pub fn do_register_command(
        self,
        global: bool,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        let (unexpandable_command, _) = get_x_token(scanner, eqtb, logger);
        match unexpandable_command {
            UnexpandableCommand::Prefixable(PrefixableCommand::Integer(target)) => {
                self.do_int_command(target, global, scanner, eqtb, logger);
            }
            UnexpandableCommand::Prefixable(PrefixableCommand::Dimension(target)) => {
                self.do_dimen_command(target, global, scanner, eqtb, logger);
            }
            UnexpandableCommand::Prefixable(PrefixableCommand::Glue(target)) => {
                self.do_glue_command(false, target, global, scanner, eqtb, logger);
            }
            UnexpandableCommand::Prefixable(PrefixableCommand::MuGlue(target)) => {
                self.do_glue_command(true, target, global, scanner, eqtb, logger);
            }
            UnexpandableCommand::Prefixable(PrefixableCommand::Register(value_type)) => {
                let register = scanner.scan_register_index(eqtb, logger);
                match value_type {
                    ValueType::Int => {
                        let target = IntegerVariable::Count(register);
                        self.do_int_command(target, global, scanner, eqtb, logger);
                    }
                    ValueType::Dimen => {
                        let target = DimensionVariable::Dimen(register);
                        self.do_dimen_command(target, global, scanner, eqtb, logger);
                    }
                    ValueType::Glue => {
                        let target = SkipVariable::Skip(register);
                        self.do_glue_command(false, target, global, scanner, eqtb, logger);
                    }
                    ValueType::Mu => {
                        let target = SkipVariable::MuSkip(register);
                        self.do_glue_command(true, target, global, scanner, eqtb, logger);
                    }
                }
            }
            // Otherwise we need a free register.
            _ => {
                logger.print_err("You can't use `");
                unexpandable_command.display(&eqtb.fonts, logger);
                logger.print_str("' after ");
                self.display(logger);
                let help = &["I'm forgetting what you said and not changing anything."];
                logger.error(help, scanner, eqtb);
            }
        }
    }

    /// See 1238. and 1240.
    fn do_int_command(
        self,
        target: IntegerVariable,
        global: bool,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        if scanner.scan_keyword(b"by", eqtb, logger) {
            // Do nothing.
        }
        let result = match self {
            Self::Advance => {
                let value = Integer::scan_int(scanner, eqtb, logger);
                Ok(value + eqtb.integer(target))
            }
            Self::Multiply => {
                let factor = Integer::scan_int(scanner, eqtb, logger);
                mult_integers(eqtb.integer(target), factor)
            }
            Self::Divide => {
                let divisor = Integer::scan_int(scanner, eqtb, logger);
                if divisor == 0 {
                    Err(ArithError)
                } else {
                    Ok(eqtb.integer(target) / divisor)
                }
            }
        };
        match result {
            Ok(value) => eqtb.int_define(target, value, global, logger),
            Err(ArithError) => {
                logger.print_err("Arithmetic overflow");
                let help = &[
                    "I can't carry out that multiplication or division,",
                    "since the result is out of range.",
                ];
                logger.error(help, scanner, eqtb);
            }
        }
    }

    /// See 1238. and 1240.
    fn do_dimen_command(
        self,
        target: DimensionVariable,
        global: bool,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        if scanner.scan_keyword(b"by", eqtb, logger) {
            // Do nothing.
        }
        let result = match self {
            Self::Advance => {
                let value = scan_normal_dimen(scanner, eqtb, logger);
                Ok(value + eqtb.dimen(target))
            }
            Self::Multiply => {
                let factor = Integer::scan_int(scanner, eqtb, logger);
                nx_plus_y(eqtb.dimen(target), factor, 0)
            }
            Self::Divide => {
                let divisor = Integer::scan_int(scanner, eqtb, logger);
                if divisor == 0 {
                    Err(ArithError)
                } else {
                    Ok(eqtb.dimen(target) / divisor)
                }
            }
        };
        match result {
            Ok(value) => eqtb.dimen_define(target, value, global),
            Err(_) => {
                logger.print_err("Arithmetic overflow");
                let help = &[
                    "I can't carry out that multiplication or division,",
                    "since the result is out of range.",
                ];
                logger.error(help, scanner, eqtb);
            }
        }
    }

    /// See 1238., 1239. and 1240.
    fn do_glue_command(
        self,
        mu: bool,
        target: SkipVariable,
        global: bool,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        if scanner.scan_keyword(b"by", eqtb, logger) {
            // Do nothing.
        }
        let result = match self {
            Self::Advance => {
                let second_glue = if mu {
                    scan_glue(true, scanner, eqtb, logger)
                } else {
                    scan_glue(false, scanner, eqtb, logger)
                };
                let mut res = (*second_glue).clone();
                let first = eqtb.skips.get(target);
                res.width += first.width;
                res.stretch += first.stretch;
                res.shrink += first.shrink;
                Ok(Rc::new(res))
            }
            Self::Multiply => {
                let factor = Integer::scan_int(scanner, eqtb, logger);
                let mut r = (**eqtb.skips.get(target)).clone();
                r.width.multiply(factor).and(
                    r.stretch
                        .multiply(factor)
                        .and(r.shrink.multiply(factor).and(Ok(Rc::new(r)))),
                )
            }
            Self::Divide => {
                let divisor = Integer::scan_int(scanner, eqtb, logger);
                if divisor == 0 {
                    Err(ArithError)
                } else {
                    let mut r = (**eqtb.skips.get(target)).clone();
                    r.width /= divisor;
                    r.stretch.value /= divisor;
                    r.shrink.value /= divisor;
                    Ok(Rc::new(r))
                }
            }
        };
        match result {
            Ok(value) => {
                eqtb.skip_define(target, value, global);
            }
            Err(ArithError) => {
                logger.print_err("Arithmetic overflow");
                let help = &[
                    "I can't carry out that multiplication or division,",
                    "since the result is out of range.",
                ];
                logger.error(help, scanner, eqtb);
            }
        }
    }
}

impl Dumpable for ArithCommand {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::Advance => writeln!(target, "Advance")?,
            Self::Multiply => writeln!(target, "Multiply")?,
            Self::Divide => writeln!(target, "Divide")?,
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "Advance" => Ok(Self::Advance),
            "Multiply" => Ok(Self::Multiply),
            "Divide" => Ok(Self::Divide),
            _ => Err(FormatError::ParseError),
        }
    }
}
