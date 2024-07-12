use crate::eqtb::{CatCode, ControlSequence, Eqtb};
use crate::token::Token;

/// Scans an input line and produces tokens.
///
/// The produced tokens are quite primitive and have not yet resolved
/// the command words, command symbols and active characters.
///
/// See 303.
#[derive(Debug)]
pub struct LineLexer {
    /// Indicates whether we are ignoring spaces or are at the beginning of a
    /// new line.
    state: LineLexerState,
    /// The index of the current character.
    /// Note: this is called `loc` in the TeX82 code.
    pos: usize,
    /// The current line of input.
    /// Note: In TeX82 this is stored in the `buffer` array. Because
    /// we know the size of this line, we also no longer need the TeX82
    /// variables `start` and `limit` that were used as indices into `buffer`.
    line: Vec<u8>,
}

/// Determines how whitespace is dealt with.
/// See 303.
#[derive(Debug, Clone, Copy)]
enum LineLexerState {
    /// The default value
    Midline,
    /// In this state we are ignoring spaces
    SkipBlanks,
    /// The state when we just started a new line
    NewLine,
}

impl LineLexer {
    #[inline]
    pub const fn new(line: Vec<u8>) -> Self {
        Self {
            state: LineLexerState::NewLine,
            pos: 0,
            line,
        }
    }

    #[inline]
    pub const fn new_with_pos(line: Vec<u8>, pos: usize) -> Self {
        Self {
            state: LineLexerState::NewLine,
            pos,
            line,
        }
    }

    #[inline]
    pub const fn new_midline(line: Vec<u8>) -> Self {
        Self {
            state: LineLexerState::Midline,
            pos: 0,
            line,
        }
    }

    /// Returns true if the whole line has been lexed.
    pub fn is_finished(&self) -> bool {
        self.pos >= self.line.len()
    }

    pub fn line_len(&self) -> usize {
        self.line.len()
    }

    /// Try to get the next token from the current line and change state if necessary.
    /// Returns a [`LexerToken`] if there is one, `None` if the line has been depleted, or
    /// an `Err` if there has been an invalid char.
    /// See 344., 345., 347., 348., 349., 350., 351., and 353.
    pub fn scan_next_token(
        &mut self,
        cat_code: &impl Fn(u8) -> CatCode,
    ) -> Result<Option<LexerToken>, ()> {
        loop {
            // Get the next unexpanded character or return empty-handed.
            let (chr, cat, next_pos) = match self.next_unexpanded_character(cat_code) {
                None => return Ok(None),
                Some(val) => val,
            };
            self.pos = next_pos;

            // Create a token from the character, potentially consuming more in the process,
            // and change state if necessary.
            use CatCode::*;
            use LineLexerState::{Midline, NewLine, SkipBlanks};
            let token = match (self.state, cat) {
                (_, Letter) => {
                    self.state = Midline;
                    LexerToken::Letter(chr)
                }
                // A space while not skipping spaces.
                (Midline, Spacer) => {
                    self.state = LineLexerState::SkipBlanks;
                    // All Spacers are treated as if they were space characters.
                    LexerToken::Spacer
                }
                // If we ignore the current character.
                (_, Ignore) | (SkipBlanks, Spacer) | (NewLine, Spacer) => continue,

                (_, OtherChar) => {
                    self.state = Midline;
                    LexerToken::OtherChar(chr)
                }
                // An active char.
                (_, ActiveChar) => {
                    // Don't ignore following spaces.
                    self.state = LineLexerState::Midline;
                    LexerToken::ActiveChar(chr)
                }
                // A control sequence.
                (_, Escape) => self.scan_control_sequence(cat_code),

                // An end-of-line character while not skipping spaces.
                (Midline, CarRet) => {
                    // Skip rest of line.
                    self.pos = self.line.len();
                    // Treat end of line like a single space
                    LexerToken::Spacer
                }
                // An end-of-line character while skipping spaces or the start of a comment.
                (SkipBlanks, CarRet) | (_, Comment) => {
                    self.pos = self.line.len();
                    return Ok(None);
                }
                // An end-of-line character while still at the beginning of line.
                (NewLine, CarRet) => {
                    // Finish line.
                    self.pos = self.line.len();
                    LexerToken::Par
                }

                // All other cases
                (_, LeftBrace) => {
                    self.state = Midline;
                    LexerToken::LeftBrace(chr)
                }
                (_, RightBrace) => {
                    self.state = Midline;
                    LexerToken::RightBrace(chr)
                }
                (_, MathShift) => {
                    self.state = Midline;
                    LexerToken::MathShift(chr)
                }
                (_, TabMark) => {
                    self.state = Midline;
                    LexerToken::TabMark(chr)
                }
                (_, MacParam) => {
                    self.state = Midline;
                    LexerToken::MacParam(chr)
                }
                (_, SupMark) => {
                    self.state = Midline;
                    LexerToken::SuperMark(chr)
                }
                (_, SubMark) => {
                    self.state = Midline;
                    LexerToken::SubMark(chr)
                }
                // An invalid char.
                (_, InvalidChar) => {
                    return Err(());
                }
            };
            return Ok(Some(token));
        }
    }

    /// Returns the next unexpanded character, its catcode, and the position just after it.
    /// Or None if the line has ended.
    /// See 352 and 355.
    #[inline(always)]
    fn next_unexpanded_character(
        &mut self,
        cat_code: &impl Fn(u8) -> CatCode,
    ) -> Option<(u8, CatCode, usize)> {
        if self.pos >= self.line.len() {
            return None;
        }
        let mut chr = self.line[self.pos];
        let mut pos = self.pos + 1;
        loop {
            let cat = cat_code(chr);
            // If the next character is a "sup_mark" and is followed by the same character and
            // then an ASCII character, we have an expanded character.
            if cat == CatCode::SupMark
                && pos + 1 < self.line.len()
                && self.line[pos] == chr
                && self.line[pos + 1].is_ascii()
            {
                let c = self.line[pos + 1];
                pos += 2;
                // Could this be a hex code?
                // Do we have a second character?
                // Is that one also a hex character?
                // Then we have a hex expanded character (like ^^0d).
                if is_hex(c) && pos < self.line.len() && is_hex(self.line[pos]) {
                    let cc = self.line[pos];
                    pos += 1;
                    chr = double_hex_to_byte(c, cc);
                // We have a traditionally expanded character (like ^^M).
                } else {
                    chr = if c < 64 { c + 64 } else { c - 64 };
                }
            } else {
                return Some((chr, cat, pos));
            }
        }
    }

    /// Returns the next unexpanded character, its catcode, and the position just after it.
    /// Or None if the line has ended.
    /// In addition it changes the current line by substituting the escape sequence with the
    /// unexpanded character.
    /// NOTE This version is needed both to allow us to use a reference for
    /// `LexerToken::CommandWord` and to have the exact same output as TeX82 when the current line
    /// is printed in diagnostic messages.
    /// See 352 and 355.
    #[inline(always)]
    fn next_unexpanded_character_with_replacement(
        &mut self,
        cat_code: &impl Fn(u8) -> CatCode,
    ) -> Option<(u8, CatCode, usize)> {
        if self.pos >= self.line.len() {
            return None;
        }
        let mut chr = self.line[self.pos];
        loop {
            let mut pos = self.pos + 1;
            let cat = cat_code(chr);
            // If the next character is a "sup_mark" and is followed by the same character and
            // then an ASCII character, we have an expanded character.
            if cat == CatCode::SupMark
                && pos + 1 < self.line.len()
                && self.line[pos] == chr
                && self.line[pos + 1].is_ascii()
            {
                let c = self.line[pos + 1];
                pos += 2;
                // Could this be a hex code?
                // Do we have a second character?
                // Is that one also a hex character?
                // Then we have a hex expanded character (like ^^0d).
                if is_hex(c) && pos < self.line.len() && is_hex(self.line[pos]) {
                    let cc = self.line[pos];
                    chr = double_hex_to_byte(c, cc);
                    self.line.drain(self.pos..self.pos + 3);
                    self.line[self.pos] = chr;
                // We have a traditionally expanded character (like ^^M).
                } else {
                    chr = if c < 64 { c + 64 } else { c - 64 };
                    self.line.drain(self.pos..self.pos + 2);
                    self.line[self.pos] = chr;
                }
            } else {
                return Some((chr, cat, pos));
            }
        }
    }

    /// Creates a control sequence token from the input.
    /// See 354 and 356.
    fn scan_control_sequence(&mut self, cat_code: &impl Fn(u8) -> CatCode) -> LexerToken {
        match self.next_unexpanded_character_with_replacement(cat_code) {
            // If there are no more characters.
            None => LexerToken::CommandWord(&[]),
            Some((c, cat, next_pos)) => {
                let start = self.pos;
                self.pos = next_pos;
                match cat {
                    // This could be a single-letter or multi-letter control sequence.
                    CatCode::Letter => {
                        self.state = LineLexerState::SkipBlanks;
                        while let Some((_, CatCode::Letter, next_pos)) =
                            self.next_unexpanded_character_with_replacement(cat_code)
                        {
                            self.pos = next_pos;
                        }
                        let end = self.pos;
                        if end - start > 1 {
                            LexerToken::CommandWord(&self.line[start..end])
                        } else {
                            LexerToken::CommandSymbol(self.line[start])
                        }
                    }
                    // We want to ignore spaces following a command like "\ "
                    CatCode::Spacer => {
                        self.state = LineLexerState::SkipBlanks;
                        LexerToken::CommandSymbol(c)
                    }
                    // For other non-letter control symbols, we don't want to ignore following
                    // spaces.
                    _ => {
                        self.state = LineLexerState::Midline;
                        LexerToken::CommandSymbol(c)
                    }
                }
            }
        }
    }

    /// See 318.
    pub fn get_read_and_unread_parts_of_line(&self, end_line_char: i32) -> (&[u8], &[u8]) {
        let mut line = &self.line[..];
        // If an endlinechar has been added, ignore it.
        if let Some(&c) = self.line.last() {
            if c as i32 == end_line_char {
                line = &line[..line.len() - 1];
            }
        }
        if self.pos >= line.len() {
            (line, &[] as &[u8])
        } else {
            (&line[..self.pos], &line[self.pos..])
        }
    }
}

/// Returns true if the given character is a hex digit.
fn is_hex(c: u8) -> bool {
    c.is_ascii_digit() || (c.is_ascii_hexdigit() && c.is_ascii_lowercase())
}

/// Creates a number from a single hex digit.
fn hex_to_number(c: u8) -> u8 {
    if c <= b'9' {
        c - b'0'
    } else {
        c - b'a' + 10
    }
}

/// Creates a number from two hex digits.
fn double_hex_to_byte(c: u8, cc: u8) -> u8 {
    let c = hex_to_number(c);
    let cc = hex_to_number(cc);
    16 * c + cc
}

/// A token as returned from [`LineLexer`].
#[derive(Debug, Clone, Copy)]
pub enum LexerToken<'a> {
    LeftBrace(u8),
    RightBrace(u8),
    MathShift(u8),
    TabMark(u8),
    MacParam(u8),
    SuperMark(u8),
    SubMark(u8),
    /// A spacer is always treated like a space internally.
    Spacer,
    Letter(u8),
    OtherChar(u8),
    ActiveChar(u8),
    CommandSymbol(u8),
    CommandWord(&'a [u8]),
    /// End-of-paragraph token
    Par,
}

impl<'a> LexerToken<'a> {
    pub fn to_token(self, allow_new_cs: bool, eqtb: &mut Eqtb) -> Result<Token, ()> {
        use LexerToken::*;
        let token = match self {
            LeftBrace(c) => Token::LeftBrace(c),
            RightBrace(c) => Token::RightBrace(c),
            MathShift(c) => Token::MathShift(c),
            TabMark(c) => Token::TabMark(c),
            MacParam(c) => Token::MacParam(c),
            SuperMark(c) => Token::SuperMark(c),
            SubMark(c) => Token::SubMark(c),
            Spacer => Token::Spacer(b' '),
            Letter(c) => Token::Letter(c),
            OtherChar(c) => Token::OtherChar(c),
            ActiveChar(c) => Token::CSToken {
                cs: ControlSequence::Active(c),
            },
            CommandSymbol(c) => Token::CSToken {
                cs: ControlSequence::Single(c),
            },
            CommandWord([]) => Token::CSToken {
                cs: ControlSequence::NullCs,
            },
            CommandWord(name) => Token::CSToken {
                cs: if allow_new_cs {
                    eqtb.lookup_or_create(name)?
                } else {
                    match eqtb.lookup(name) {
                        Some(cs) => cs,
                        None => ControlSequence::Undefined,
                    }
                },
            },
            Par => eqtb.par_token,
        };
        Ok(token)
    }
}
