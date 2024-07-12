use crate::print::Printer;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConvertCommand {
    Number,
    RomanNumeral,
    String,
    Meaning,
    FontName,
    JobName,
}

impl ConvertCommand {
    pub fn display(&self, printer: &mut impl Printer) {
        let s: &[u8] = match self {
            Self::Number => b"number",
            Self::RomanNumeral => b"romannumeral",
            Self::String => b"string",
            Self::Meaning => b"meaning",
            Self::FontName => b"fontname",
            Self::JobName => b"jobname",
        };
        printer.print_esc_str(s);
    }
}
