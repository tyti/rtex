use crate::eqtb::Eqtb;
use crate::format::{Dumpable, FormatError};
use crate::print::pseudo::PseudoPrinter;
use crate::print::Printer;
use crate::token::Token;

use std::io::Write;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Macro {
    pub parameter_text: Vec<ParamToken>,
    pub replacement_text: Vec<MacroToken>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamToken {
    Normal(Token),
    Match(u8),
    End,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MacroToken {
    Normal(Token),
    OutParam(u8),
}

/// See 295.
pub fn macro_show(macro_def: &Macro, printer: &mut impl Printer, eqtb: &Eqtb) {
    show_macro_def(macro_def, 10_000_000, printer, eqtb);
}

/// Prints a token list to the selected Printer.
/// See 292.
pub fn show_macro_def(macro_def: &Macro, limit: usize, printer: &mut impl Printer, eqtb: &Eqtb) {
    printer.reset_tally();
    let match_chr = print_macro_parameters(macro_def, limit, printer, eqtb);
    print_replacement_text(&macro_def.replacement_text, match_chr, limit, printer, eqtb);
}

/// Pseudo prints the given macro.
/// See 292.
pub fn show_macro_pseudo(
    macro_def: &Macro,
    next_node: usize,
    pseudo_printer: &mut PseudoPrinter,
    eqtb: &Eqtb,
) {
    pseudo_printer.reset_tally();
    // The arbitrary bound 100_000 comes from 319.
    let limit = 100_000;
    let match_chr = print_macro_parameters(macro_def, limit, pseudo_printer, eqtb);
    let (read_text, unread_text) = &macro_def.replacement_text.split_at(next_node);
    print_replacement_text(read_text, match_chr, limit, pseudo_printer, eqtb);
    pseudo_printer.switch_to_unread_part();
    print_replacement_text(unread_text, match_chr, limit, pseudo_printer, eqtb);
}

/// Prints the parameter list of a macro to the selected Printer and returns the last use match
/// character.
/// See 292.
fn print_macro_parameters(
    macro_def: &Macro,
    limit: usize,
    printer: &mut impl Printer,
    eqtb: &Eqtb,
) -> u8 {
    let mut match_chr = b'#';
    let mut n = b'0';
    for &param_token in &macro_def.parameter_text {
        if printer.get_tally() >= limit {
            printer.print_esc_str(b"ETC.");
            return match_chr;
        }
        match param_token {
            ParamToken::Normal(token) => token.display(printer, eqtb),
            ParamToken::Match(c) => {
                match_chr = c;
                printer.print(c);
                n += 1;
                printer.print_char(n);
                if n > b'9' {
                    return match_chr;
                }
            }
            ParamToken::End => printer.print_str("->"),
        }
    }
    match_chr
}

/// Prints the replacement text of a macro to the selected Printer.
/// See 292.
fn print_replacement_text(
    replacement_text: &[MacroToken],
    match_chr: u8,
    limit: usize,
    printer: &mut impl Printer,
    eqtb: &Eqtb,
) {
    for &macro_token in replacement_text {
        if printer.get_tally() >= limit {
            printer.print_esc_str(b"ETC.");
            return;
        }
        match macro_token {
            MacroToken::Normal(token) => token.display(printer, eqtb),
            MacroToken::OutParam(number) => {
                printer.print(match_chr);
                if number <= 9 {
                    printer.print_char(number + b'0');
                } else {
                    printer.print_char(b'!');
                    return;
                }
            }
        }
    }
}

impl Dumpable for Macro {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.parameter_text.dump(target)?;
        self.replacement_text.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let parameter_text = Vec::undump(lines)?;
        let replacement_text = Vec::undump(lines)?;
        Ok(Self {
            parameter_text,
            replacement_text,
        })
    }
}

impl Dumpable for ParamToken {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::Normal(token) => {
                writeln!(target, "Normal")?;
                token.dump(target)?;
            }
            Self::Match(c) => {
                writeln!(target, "Match")?;
                c.dump(target)?;
            }
            Self::End => writeln!(target, "End")?,
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "Normal" => {
                let token = Token::undump(lines)?;
                Ok(Self::Normal(token))
            }
            "Match" => {
                let c = u8::undump(lines)?;
                Ok(Self::Match(c))
            }
            "End" => Ok(Self::End),
            _ => Err(FormatError::ParseError),
        }
    }
}

impl Dumpable for MacroToken {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::Normal(token) => {
                writeln!(target, "Normal")?;
                token.dump(target)?;
            }
            Self::OutParam(c) => {
                writeln!(target, "OutParam")?;
                c.dump(target)?;
            }
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "Normal" => {
                let token = Token::undump(lines)?;
                Ok(Self::Normal(token))
            }
            "OutParam" => {
                let c = u8::undump(lines)?;
                Ok(Self::OutParam(c))
            }
            _ => Err(FormatError::ParseError),
        }
    }
}
