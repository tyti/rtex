use super::{cannot_be_printed, to_hex_char, Printer};
use std::fs::File;
use std::io::Write;
use std::io::BufWriter;

pub struct StreamPrinter<'a> {
    file: &'a mut BufWriter<File>,
    escape_char: Option<u8>,
    newline_char: Option<u8>,
    tally: usize,
}

impl<'a> StreamPrinter<'a> {
    pub fn from_file(
        file: &'a mut BufWriter<File>,
        newline_char: Option<u8>,
        escape_char: Option<u8>,
    ) -> Self {
        Self {
            file,
            newline_char,
            escape_char,
            tally: 0,
        }
    }

    /// Like `print_char` but will not treat new_line_char differently.
    /// See 58.
    fn print_raw_char(&mut self, c: u8) {
        write!(self.file, "{}", c as char).unwrap();
        self.tally += 1;
    }
}

impl<'a> Printer for StreamPrinter<'a> {
    fn print_ln(&mut self) {
        writeln!(self.file).unwrap();
    }

    /// Prints the character as-is unless it is the designated internal newline character in which
    /// case it print a new line.
    /// See 58.
    fn print_char(&mut self, c: u8) {
        match self.newline_char {
            Some(nl) if c == nl => {
                self.print_ln();
            }
            _ => {
                self.print_raw_char(c);
            }
        }
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
        match self.newline_char {
            Some(nl) if c == nl => {
                self.print_ln();
            }
            _ => {
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
