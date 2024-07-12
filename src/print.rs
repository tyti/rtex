pub mod pseudo;
pub mod stream;
pub mod string;

use crate::scaled::{Scaled, UNITY};

/// Width of context lines on terminal error messages.
/// Original: 72
/// TeXlive: 79
/// See 11.
pub const ERROR_LINE: usize = if cfg!(feature = "trip") { 64 } else { 79 };

/// Width of first line of when displaying context in error messages.
/// Should be between 30 and ERROR_LINE - 15
/// Original: 42
/// TeXlive: 50
/// See 11.
pub const HALF_ERROR_LINE: usize = if cfg!(feature = "trip") { 32 } else { 50 };
const _: () = assert!(
    HALF_ERROR_LINE >= 30,
    "HALF_ERROR_LINE below minimum length"
);
const _: () = assert!(
    HALF_ERROR_LINE + 15 <= ERROR_LINE,
    "HALF_ERROR_LINE too long relative to ERROR_LINE"
);

/// Width of longest text lines output. Should be at least 60
/// Original: 79
/// TeXlive: 79
/// See 11.
pub const MAX_PRINT_LINE: usize = if cfg!(feature = "trip") { 72 } else { 79 };
const _: () = assert!(MAX_PRINT_LINE >= 60, "MAX_PRINT_LINE below minimum length");

pub trait Printer: Sized {
    /// Print a newline character.
    /// See 57.
    fn print_ln(&mut self);

    /// Prints a single character. This assumes that the character is printable.
    /// See 58.
    fn print_char(&mut self, c: u8);

    /// Prints a single character, it is escaped if necessary.
    /// See 59.
    fn print(&mut self, c: u8);

    /// Prints a string. The string is expected to be printable.
    /// See 59.
    fn print_str(&mut self, s: &str);

    /// See 243.
    fn current_escape_character(&self) -> Option<u8>;

    /// Prints a string where all non-printable characters are escaped.
    /// See 60.
    fn slow_print_str(&mut self, s: &[u8]) {
        for c in s {
            self.print(*c);
        }
    }

    /// Prints a string preceeded by the current escape character. All characters are escaped as
    /// necessary.
    /// See 63.
    fn print_esc_str(&mut self, s: &[u8]) {
        if let Some(c) = self.current_escape_character() {
            self.print(c);
        }
        self.slow_print_str(s);
    }

    /// Prints an integer to the Printer.
    /// We use the built-in `Display` implementation on `i32` as it is equivalent
    /// to the TeX82 implementation.
    /// See 65.
    fn print_int(&mut self, n: i32) {
        for &c in n.to_string().as_bytes() {
            self.print_char(c);
        }
    }

    /// Print a non-negative integer as a hex number.
    /// We use the built-in `UpperHex` implementation on `i32`. When preceding
    /// it with `'"'`, it is equivalent to the TeX82 implementation.
    /// See 67.
    fn print_hex(&mut self, n: i32) {
        self.print_char(b'"');
        for &c in format!("{:X}", n).as_bytes() {
            self.print_char(c);
        }
    }

    /// Prints a non-negative number as Roman numeral.
    /// Check out the original implementation for a puzzle variant of this code.
    /// See 69.
    fn print_roman_int(&mut self, mut n: i32) {
        while n >= 1000 {
            self.print_char(b'm');
            n -= 1000;
        }

        // Below 1000
        if n >= 900 {
            self.print_char(b'c');
            self.print_char(b'm');
            n -= 900;
        } else if n >= 500 {
            self.print_char(b'd');
            n -= 500;
        } else if n >= 400 {
            self.print_char(b'c');
            self.print_char(b'd');
            n -= 400;
        }
        while n >= 100 {
            self.print_char(b'c');
            n -= 100;
        }

        // Below 100
        if n >= 90 {
            self.print_char(b'x');
            self.print_char(b'c');
            n -= 90;
        } else if n >= 50 {
            self.print_char(b'l');
            n -= 50;
        } else if n >= 40 {
            self.print_char(b'x');
            self.print_char(b'l');
            n -= 40;
        }
        while n >= 10 {
            self.print_char(b'x');
            n -= 10;
        }

        // Below 10
        if n >= 9 {
            self.print_char(b'i');
            self.print_char(b'x');
            n -= 9;
        } else if n >= 5 {
            self.print_char(b'v');
            n -= 5;
        } else if n >= 4 {
            self.print_char(b'i');
            self.print_char(b'v');
            n -= 4;
        }
        while n >= 1 {
            self.print_char(b'i');
            n -= 1;
        }
    }

    /// Prints a Scaled number as a decimal fraction.
    /// Prints the number such that `round_decimals` will
    /// restore it exactly.
    /// See 103.
    fn print_scaled(&mut self, mut s: Scaled) {
        if s < 0 {
            self.print_char(b'-');
            s = -s;
        }
        self.print_int(s / UNITY);
        self.print_char(b'.');
        s = 10 * (s % UNITY) + 5;
        let mut delta = 10;
        loop {
            if delta > UNITY {
                s += 0x8000 - 50000;
            }
            self.print_char(b'0' + (s / UNITY) as u8);
            s = 10 * (s % UNITY);
            delta *= 10;
            if s <= delta {
                break;
            }
        }
    }

    /// Get the number of printed characters since the start or the last reset.
    fn get_tally(&self) -> usize;

    /// Set the count of characters that have been printed so far back to zero.
    fn reset_tally(&mut self);
}

/// Takes a 4-bit value and maps it to the corresponding hex character.
/// See 48.
pub fn to_hex_char(c: u8) -> u8 {
    if c < 10 {
        c + b'0'
    } else {
        c - 10 + b'a'
    }
}

/// See 49.
pub const fn cannot_be_printed(k: u8) -> bool {
    k < b' ' || k > b'~'
}
