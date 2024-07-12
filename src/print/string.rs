use super::Printer;

pub struct StringPrinter {
    tally: usize,
    string: Vec<u8>,
    escape_char: Option<u8>,
}

impl StringPrinter {
    pub fn new(escape_char: Option<u8>) -> Self {
        Self {
            tally: 0,
            string: Vec::new(),
            escape_char,
        }
    }

    pub fn into_string(self) -> Vec<u8> {
        self.string
    }
}

impl Printer for StringPrinter {
    /// See 57.
    fn print_ln(&mut self) {
        // Do nothing.
    }

    /// See 58.
    fn print_char(&mut self, c: u8) {
        self.string.push(c);
        self.tally += 1;
    }

    /// Prints a single character. As this is for internal printing, the character is not escaped.
    /// See 59.
    fn print(&mut self, c: u8) {
        self.print_char(c);
    }

    /// Prints a string. As this is for internal printing the string is not escaped.
    /// See 59.
    fn print_str(&mut self, s: &str) {
        let s = s.as_bytes();
        for c in s {
            self.print_char(*c);
        }
    }

    /// See 243.
    fn current_escape_character(&self) -> Option<u8> {
        self.escape_char
    }

    /// Get the number of printed characters since the start or the last reset.
    fn get_tally(&self) -> usize {
        self.tally
    }

    /// Set the count of characters that have been printed so far back to zero.
    fn reset_tally(&mut self) {
        self.tally = 0;
    }
}
