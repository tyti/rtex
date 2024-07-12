use crate::command::{Command, UnexpandableCommand};
use crate::eqtb::{Eqtb, FontIndex, IntegerVariable};
use crate::fonts::{char_warning, CharTag, LigKernCommand};
use crate::horizontal_mode::{HorizontalMode, HorizontalModeType};
use crate::input::expansion::{get_x_token, x_token};
use crate::input::Scanner;
use crate::logger::Logger;
use crate::nodes::{CharNode, DiscNode, KernNode, LanguageNode, LigatureNode, Node, WhatsitNode};
use crate::norm_min;
use crate::token::Token;

#[derive(Debug)]
enum WordScanError {
    MissingChar,
}

/// Consumes a word and adds it to the current tail. Creates ligature nodes along the way.
/// Returns an unused token, if we have one.
/// See 1034.
pub fn main_loop(
    hmode: &mut HorizontalMode,
    first_char: u8,
    cancel_boundary: bool,
    word_scanner: &mut WordScanner,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> (UnexpandableCommand, Token) {
    adjust_space_factor(hmode, first_char, eqtb);
    fix_language(hmode, eqtb);

    word_scanner.initialize(first_char, eqtb);

    match word_scanner.scan(hmode, cancel_boundary, scanner, eqtb, logger) {
        Ok(command_token) => command_token,
        Err(WordScanError::MissingChar) => get_x_token(scanner, eqtb, logger),
    }
}

pub struct WordScanner {
    font_index: FontIndex,

    cur_l: LeftNode,
    lig_stack: Vec<QueueItem>,

    finished: Option<(UnexpandableCommand, Token)>,
    left_hit: bool,
    right_hit: bool,

    hyphen_char: i32,
    bchar: Option<u8>,
    false_bchar: Option<u8>,
}

#[derive(Debug)]
struct RightLigItem {
    character: u8,
    consumed_char: Option<u8>,
}

#[derive(Debug)]
struct LeftLigItem {
    character: u8,
    consumed_chars: Vec<u8>,
}

enum QueueItem {
    /// A character in the queue.
    Char(u8),
    /// A ligature in the queue that potentially has consumed a previous character.
    Lig(RightLigItem),
}

enum LeftNode {
    Char(u8),
    Lig(LeftLigItem),
    None,
}

enum CharacterOrFinalCommand {
    Character(u8),
    FinalCommand(UnexpandableCommand, Token),
}

impl WordScanner {
    pub fn new() -> Self {
        Self {
            font_index: 0,
            cur_l: LeftNode::None,
            lig_stack: Vec::new(),
            finished: None,
            left_hit: false,
            right_hit: false,
            hyphen_char: 0,
            bchar: None,
            false_bchar: None,
        }
    }

    fn initialize(&mut self, first_char: u8, eqtb: &Eqtb) {
        self.font_index = eqtb.cur_font();
        let font = &eqtb.fonts[self.font_index as usize];
        self.cur_l = LeftNode::None;
        self.lig_stack.clear();
        self.lig_stack.push(QueueItem::Char(first_char));
        self.finished = None;
        self.left_hit = false;
        self.right_hit = false;
        self.hyphen_char = font.hyphen_char;
        self.bchar = font.bchar;
        self.false_bchar = font.false_bchar;
    }

    /// Consumes a word and adds it to the current tail. Creates ligature nodes along the way.
    /// Returns an unused token, if we have one.
    /// See 1034.
    fn scan(
        &mut self,
        hmode: &mut HorizontalMode,
        cancel_boundary: bool,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> Result<(UnexpandableCommand, Token), WordScanError> {
        // Potentially ignore a left boundary
        if cancel_boundary {
            self.move_right(hmode, scanner, eqtb, logger)?;
        }

        loop {
            if let LeftNode::None = self.cur_l {
                if let Some(finishing_command) = self.finished {
                    return Ok(finishing_command);
                }
            }

            let lig_kern_command = self.lookup_lig_kern_command(eqtb);

            if let Some(lig_kern_command) = lig_kern_command {
                // From 1040.
                match lig_kern_command {
                    LigKernCommand::Kern(w) => {
                        self.move_right(hmode, scanner, eqtb, logger)?;
                        // Append a kern after the left node has moved to the current list.
                        hmode.append_node(Node::Kern(KernNode::new(w)), eqtb);
                        continue;
                    }
                    LigKernCommand::Lig {
                        ligature,
                        delete_left,
                        delete_right,
                        moves,
                    } => {
                        if let LeftNode::None = self.cur_l {
                            self.left_hit = true;
                        } else if self.lig_stack.is_empty() {
                            self.right_hit = true;
                        }
                        // Allow a user to interrupt in case of an infinite ligature loop.
                        logger.check_interrupt(scanner, eqtb);
                        match (delete_left, delete_right, moves) {
                            //  =:|,  =:|>
                            (true, false, _) => {
                                let old_cur_l = std::mem::replace(&mut self.cur_l, LeftNode::None);
                                // Substitute left node
                                self.cur_l = LeftNode::Lig(LeftLigItem {
                                    character: ligature,
                                    consumed_chars: match old_cur_l {
                                        LeftNode::None => Vec::new(),
                                        LeftNode::Char(character) => vec![character],
                                        LeftNode::Lig(LeftLigItem { consumed_chars, .. }) => {
                                            consumed_chars
                                        }
                                    },
                                });
                                if moves == 0 {
                                    // Goto ligloop
                                    continue;
                                }
                            }
                            //  |=:,  |=:>
                            (false, true, _) => {
                                // Substitute right node (top of lig_stack)
                                match self.lig_stack.pop() {
                                    None => {
                                        self.lig_stack.push(QueueItem::Lig(RightLigItem {
                                            character: ligature,
                                            consumed_char: None,
                                        }));
                                        self.bchar = None;
                                    }
                                    Some(QueueItem::Char(char_node)) => {
                                        self.lig_stack.push(QueueItem::Lig(RightLigItem {
                                            character: ligature,
                                            consumed_char: Some(char_node),
                                        }));
                                    }
                                    Some(QueueItem::Lig(RightLigItem {
                                        consumed_char, ..
                                    })) => {
                                        self.lig_stack.push(QueueItem::Lig(RightLigItem {
                                            character: ligature,
                                            consumed_char,
                                        }));
                                    }
                                }
                                if moves == 0 {
                                    // Goto ligloop
                                    continue;
                                }
                            }
                            //  |=:|, |=:|>, |=:|>>
                            (false, false, _) => {
                                // Make ligature new right node and move old one back (push to
                                // lig_stack)
                                self.lig_stack.push(QueueItem::Lig(RightLigItem {
                                    character: ligature,
                                    consumed_char: None,
                                }));
                                match moves {
                                    0 => continue,
                                    1 => {}
                                    // moves == 2
                                    _ => self.move_right(hmode, scanner, eqtb, logger)?,
                                }
                            }
                            (true, true, _) => {
                                // Replace left node and consume right node.
                                let old_cur_l = std::mem::replace(&mut self.cur_l, LeftNode::None);
                                let mut left_lig_item = LeftLigItem {
                                    character: ligature,
                                    consumed_chars: match old_cur_l {
                                        LeftNode::None => Vec::new(),
                                        LeftNode::Char(character) => vec![character],
                                        LeftNode::Lig(LeftLigItem { consumed_chars, .. }) => {
                                            consumed_chars
                                        }
                                    },
                                };
                                match self.lig_stack.pop() {
                                    None => {
                                        self.cur_l = LeftNode::Lig(left_lig_item);
                                        self.wrap_up(hmode, eqtb);
                                        // Goto reswitch
                                        return Ok(self.finished.unwrap());
                                    }
                                    Some(QueueItem::Lig(RightLigItem {
                                        consumed_char, ..
                                    })) => {
                                        if let Some(character) = consumed_char {
                                            left_lig_item.consumed_chars.push(character);
                                            self.cur_l = LeftNode::Lig(left_lig_item);
                                            // We need to fill up the lig_stack
                                            self.lookahead(hmode, scanner, eqtb, logger);
                                        } else {
                                            // If the last node on the lig_stack is a lig item that did not consume
                                            // a character node, it must have been created from the right boundary
                                            // character. We thus don't want to fetch a new right
                                            // character.
                                            self.cur_l = LeftNode::Lig(left_lig_item);
                                        }
                                        // Goto ligloop
                                        continue;
                                    }
                                    Some(QueueItem::Char(character)) => {
                                        left_lig_item.consumed_chars.push(character);
                                        self.cur_l = LeftNode::Lig(left_lig_item);
                                        self.lookahead(hmode, scanner, eqtb, logger);
                                        continue;
                                    }
                                }
                            }
                        }
                    }
                }
            }

            self.move_right(hmode, scanner, eqtb, logger)?;
        }
    }

    /// Move everything one to the left. The leftmost Node is pushed to the current list.
    /// If there is no right Node, return the finishing command; otherwise return None.
    /// In case that the character of a CharNode moved from right to left is not valid
    /// in the current font, return an Err.
    /// See 1036. and 1037.
    fn move_right(
        &mut self,
        hmode: &mut HorizontalMode,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> Result<(), WordScanError> {
        self.wrap_up(hmode, eqtb);

        match self.lig_stack.pop() {
            None => {
                return Ok(());
            }
            Some(QueueItem::Lig(lig_item)) => {
                self.cur_l = LeftNode::Lig(LeftLigItem {
                    character: lig_item.character,
                    consumed_chars: match lig_item.consumed_char {
                        None => Vec::new(),
                        Some(character) => vec![character],
                    },
                });
            }
            Some(QueueItem::Char(character)) => {
                let font = &eqtb.fonts[self.font_index as usize];
                if character < font.bc || character > font.ec || !font.char_exists(character) {
                    char_warning(font, character, eqtb, logger);
                    return Err(WordScanError::MissingChar);
                }

                self.cur_l = LeftNode::Char(character);
            }
        }
        // If we need to get the next character.
        if self.lig_stack.last().is_none() {
            if self.finished.is_none() {
                self.lookahead(hmode, scanner, eqtb, logger);
            }
        }
        Ok(())
    }

    /// Append the left node to the current list.
    /// Also add a discretionary if appropriate.
    /// See 1035.
    fn wrap_up(&mut self, hmode: &mut HorizontalMode, eqtb: &mut Eqtb) {
        let font = &eqtb.fonts[self.font_index as usize];
        let left_node = std::mem::replace(&mut self.cur_l, LeftNode::None);
        match left_node {
            LeftNode::None => {}
            LeftNode::Char(character) => {
                hmode.append_node(
                    Node::Char(CharNode {
                        font_index: self.font_index,
                        character,
                        width: font.width(character),
                        height: font.height(character),
                        depth: font.depth(character),
                        italic: font.italic(character),
                    }),
                    eqtb,
                );
                if character as i32 == self.hyphen_char {
                    if let HorizontalModeType::Unrestricted { .. } = hmode.subtype {
                        hmode.append_node(Node::Disc(DiscNode::new()), eqtb);
                    }
                }
            }
            LeftNode::Lig(lig_item) => {
                let last_char = lig_item.consumed_chars.last().copied();
                let left_boundary = if self.left_hit {
                    // We are only considering the very leftmost ligature to be a "left hit".
                    self.left_hit = false;
                    true
                } else {
                    false
                };
                let right_boundary = self.right_hit && self.lig_stack.is_empty();
                hmode.append_node(
                    Node::Ligature(LigatureNode {
                        font_index: self.font_index,
                        character: lig_item.character,
                        width: font.width(lig_item.character),
                        height: font.height(lig_item.character),
                        depth: font.depth(lig_item.character),
                        italic: font.italic(lig_item.character),
                        lig: lig_item.consumed_chars,
                        left_boundary,
                        right_boundary,
                    }),
                    eqtb,
                );
                if let Some(chr) = last_char {
                    if chr as i32 == self.hyphen_char {
                        if let HorizontalModeType::Unrestricted { .. } = hmode.subtype {
                            hmode.append_node(Node::Disc(DiscNode::new()), eqtb);
                        }
                    }
                }
            }
        }
    }

    /// Get the next cur_r value and create the corresponding character node on lig_stack.
    /// See 1038.
    fn lookahead(
        &mut self,
        hmode: &mut HorizontalMode,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        match fetch_next_character(scanner, eqtb, logger) {
            CharacterOrFinalCommand::Character(character) => {
                adjust_space_factor(hmode, character, eqtb);
                self.lig_stack.push(QueueItem::Char(character));
            }
            CharacterOrFinalCommand::FinalCommand(command, token) => {
                self.finished = Some((command, token));
            }
        }
    }

    /// Lookup whether there is a ligature or kerning command for the current letter pair.
    /// From 1039.
    fn lookup_lig_kern_command(&self, eqtb: &Eqtb) -> Option<LigKernCommand> {
        let left_char = match &self.cur_l {
            &LeftNode::Char(character) => Some(character),
            LeftNode::Lig(ligature_node) => Some(ligature_node.character),
            LeftNode::None => None,
        };
        let font = &eqtb.fonts[self.font_index as usize];
        let right_char = match self.lig_stack.last() {
            None => {
                if let Some((UnexpandableCommand::NoBoundary, _)) = &self.finished {
                    None
                } else {
                    self.bchar
                }
            }
            Some(&QueueItem::Char(character)) => {
                if Some(character) == self.false_bchar {
                    None
                } else {
                    Some(character)
                }
            }
            Some(QueueItem::Lig(lig_item)) => Some(lig_item.character),
        };
        match (left_char, right_char) {
            (_, None) => None,
            (Some(left_char), Some(right_char)) => {
                if let CharTag::Lig(lig_index) = font.tag(left_char) {
                    font.lig_kerns[lig_index].get(&right_char).copied()
                } else {
                    None
                }
            }
            (None, Some(right_char)) => {
                if let Some(lig_index) = font.bchar_label {
                    font.lig_kerns[lig_index].get(&right_char).copied()
                } else {
                    None
                }
            }
        }
    }
}

/// Get the next character of the command that finished the word.
fn fetch_next_character(
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> CharacterOrFinalCommand {
    let (command, token) = scanner.get_next(false, eqtb, logger);
    if let Command::Unexpandable(
        UnexpandableCommand::Letter(c)
        | UnexpandableCommand::Other(c)
        | UnexpandableCommand::CharGiven(c),
    ) = command
    {
        CharacterOrFinalCommand::Character(c)
    } else {
        let (unexpandable_command, token) = x_token(command, token, scanner, eqtb, logger);
        match unexpandable_command {
            UnexpandableCommand::Letter(c)
            | UnexpandableCommand::Other(c)
            | UnexpandableCommand::CharGiven(c) => CharacterOrFinalCommand::Character(c),
            UnexpandableCommand::CharNum => {
                let c = scanner.scan_char_num(eqtb, logger);
                CharacterOrFinalCommand::Character(c)
            }

            // If we hit the end of the word.
            _ => {
                CharacterOrFinalCommand::FinalCommand(unexpandable_command, token)
            }
        }
    }
}

/// See 1376.
fn fix_language(hmode: &mut HorizontalMode, eqtb: &mut Eqtb) {
    if let HorizontalModeType::Unrestricted { clang, .. } = &mut hmode.subtype {
        let language = eqtb.integer(IntegerVariable::Language);
        if language != *clang as i32 {
            let normalized_language = if language <= 0 || language > 255 {
                0
            } else {
                language
            };
            if normalized_language != *clang as i32 {
                *clang = normalized_language as u16;
                let language_node = LanguageNode {
                    language: normalized_language as usize,
                    left_hyphen_min: norm_min(eqtb.integer(IntegerVariable::LeftHyphenMin)),
                    right_hyphen_min: norm_min(eqtb.integer(IntegerVariable::RightHyphenMin)),
                };
                hmode.append_node(Node::Whatsit(WhatsitNode::Language(language_node)), eqtb);
            }
        }
    }
}

/// Adjust the space factor using the given character.
/// See 1034.
fn adjust_space_factor(hmode: &mut HorizontalMode, chr: u8, eqtb: &mut Eqtb) {
    let sf = eqtb.sf_code(chr as usize);
    if sf == 1000 {
        hmode.set_space_factor(1000, eqtb);
    } else if sf < 1000 {
        if sf > 0 {
            hmode.set_space_factor(sf as u16, eqtb);
        }
    } else {
        // Don't raise from below 1000 to above 1000; use 1000 instead.
        if hmode.space_factor < 1000 {
            hmode.set_space_factor(1000, eqtb);
        } else {
            hmode.set_space_factor(sf as u16, eqtb);
        }
    }
}
