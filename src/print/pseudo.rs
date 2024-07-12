use super::{cannot_be_printed, to_hex_char, Printer};

#[derive(Debug)]
pub struct PseudoPrinter {
    pub read_part: Vec<u8>,
    pub unread_part: Vec<u8>,
    write_to_unread: bool,
    escape_char: Option<u8>,
    tally: usize,
}

impl PseudoPrinter {
    pub fn new(escape_char: Option<u8>) -> Self {
        Self {
            read_part: Vec::new(),
            unread_part: Vec::new(),
            write_to_unread: false,
            escape_char,
            tally: 0,
        }
    }

    /// Prints a char as it is.
    /// See 58.
    fn print_raw_char(&mut self, c: u8) {
        if !self.write_to_unread {
            self.read_part.push(c)
        } else {
            self.unread_part.push(c)
        }
        self.tally += 1;
    }

    /// See 316.
    pub fn switch_to_unread_part(&mut self) {
        self.write_to_unread = true;
    }

    pub fn into_parts(self) -> (Vec<u8>, Vec<u8>) {
        (self.read_part, self.unread_part)
    }
}

impl Printer for PseudoPrinter {
    /// See 57.
    fn print_ln(&mut self) {
        // Do nothing.
    }

    /// Prints a char as it is.
    /// See 58.
    fn print_char(&mut self, c: u8) {
        self.print_raw_char(c);
    }

    /// Prints a single character, escaped if deemed necessary.
    ///
    /// For printable ASCII values this is just the corresponding character. For non-printable
    /// characters ASCII characters (value less than 128), the character is escaped by first
    /// printing two `^` characters and then the character plus 64 if the character value is below
    /// 64, or minus 64 if the character value is above 64.
    ///
    /// Thus Carriage Return, with a value of 13, is represented as `^^M` while Delete, with a
    /// value of 127, is represented as `^^?`.
    ///
    /// Non-ASCII bytes, i.e. with a value larger or equal to 128, are represented by two `^`
    /// characters followed by the hex respresentation of the value in number and lowercase
    /// letters.
    ///
    /// Thus the byte 255 is represented as `^^ff` and the byte 128 is represented as `^^80`.
    /// See 59. and 48.
    fn print(&mut self, c: u8) {
        // NOTE: We don't need to disable the newlinechar as `print_char` does not
        // treat it differently in a `PseudoPrinter`.
        if cannot_be_printed(c) {
            self.print_raw_char(b'^');
            self.print_raw_char(b'^');
            if c < 64 {
                self.print_raw_char(c + 64);
            } else if c < 128 {
                self.print_raw_char(c - 64);
            } else {
                self.print_raw_char(to_hex_char(c / 16));
                self.print_raw_char(to_hex_char(c % 16));
            }
        } else {
            self.print_raw_char(c);
        }
    }

    /// Prints a string. The string is expected to be printable.
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
