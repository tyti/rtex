use crate::eqtb::Eqtb;
use crate::input::{InputStack, Scanner};
use crate::logger::Logger;
use crate::print::Printer;

/// Different to TeX82, this does not complain if this fatal error set the error count to 100.
/// See 93.
fn succumb(help: &[&str], input_stack: &InputStack, eqtb: &Eqtb, logger: &mut Logger) -> ! {
    if logger.log_file.is_some() {
        logger.print_char(b'.');
        input_stack.show_context(logger, eqtb);
        logger.put_help_message_on_transcript_file(help, eqtb);
    }
    jump_out(logger);
}

/// See 93.
pub fn fatal_error(cause: &str, input_stack: &InputStack, eqtb: &Eqtb, logger: &mut Logger) -> ! {
    logger.print_err("Emergency stop");
    let help = &[cause];
    succumb(help, input_stack, eqtb, logger);
}

/// See 94.
pub fn overflow(
    resource: &str,
    value: usize,
    input_stack: &InputStack,
    eqtb: &Eqtb,
    logger: &mut Logger,
) -> ! {
    logger.print_err("TeX capacity exceeded, sorry [");
    logger.print_str(resource);
    logger.print_char(b'=');
    logger.print_int(value as i32);
    logger.print_char(b']');
    let help = &[
        "If you really absolutely need more capacity,",
        "you can ask a wizard to enlarge me.",
    ];
    succumb(help, input_stack, eqtb, logger);
}

/// See 1304.
pub fn dump_in_group_error(input_stack: &InputStack, eqtb: &Eqtb, logger: &mut Logger) -> ! {
    logger.print_err("You can't dump inside a group");
    let help = &["`{...\\dump}' is a no-no."];
    succumb(help, input_stack, eqtb, logger);
}

/// Shuts down the Logger and exits with an error status of 1.
/// Differently to how TeX82 does it, this produces no statistics, does not finish the DVI file,
/// and does not flush the write streams.
pub fn jump_out(logger: &mut Logger) -> ! {
    logger.shutdown();
    std::process::exit(1);
}

/// See 408.
pub fn mu_error(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) {
    logger.print_err("Incompatible glue units");
    let help = &["I'm going to assume that 1mu=1pt when they're mixed."];
    logger.error(help, scanner, eqtb)
}
