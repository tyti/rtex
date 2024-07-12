use crate::print::Printer;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MarkCommand {
    Top,
    First,
    Bot,
    SplitFirst,
    SplitBot,
}

impl MarkCommand {
    pub fn display(&self, printer: &mut impl Printer) {
        let s: &[u8] = match self {
            Self::Top => b"topmark",
            Self::First => b"firstmark",
            Self::Bot => b"botmark",
            Self::SplitFirst => b"splitfirstmark",
            Self::SplitBot => b"splitbotmark",
        };
        printer.print_esc_str(s);
    }
}
