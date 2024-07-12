use crate::print::Printer;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IfTest {
    IfChar,
    IfCat,
    IfInt,
    IfDim,
    IfOdd,
    IfVmode,
    IfHmode,
    IfMmode,
    IfInner,
    IfVoid,
    IfHbox,
    IfVbox,
    Ifx,
    IfEof,
    IfTrue,
    IfFalse,
    IfCase,
}

impl IfTest {
    pub fn display(&self, printer: &mut impl Printer) {
        let s: &[u8] = match self {
            Self::IfChar => b"if",
            Self::IfCat => b"ifcat",
            Self::IfInt => b"ifnum",
            Self::IfDim => b"ifdim",
            Self::IfOdd => b"ifodd",
            Self::IfVmode => b"ifvmode",
            Self::IfHmode => b"ifhmode",
            Self::IfMmode => b"ifmmode",
            Self::IfInner => b"ifinner",
            Self::IfVoid => b"ifvoid",
            Self::IfHbox => b"ifhbox",
            Self::IfVbox => b"ifvbox",
            Self::Ifx => b"ifx",
            Self::IfEof => b"ifeof",
            Self::IfTrue => b"iftrue",
            Self::IfFalse => b"iffalse",
            Self::IfCase => b"ifcase",
        };
        printer.print_esc_str(s);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FiOrElse {
    Fi,
    Or,
    Else,
}

impl FiOrElse {
    pub fn display(&self, printer: &mut impl Printer) {
        let s: &[u8] = match self {
            Self::Fi => b"fi",
            Self::Or => b"or",
            Self::Else => b"else",
        };
        printer.print_esc_str(s);
    }
}
