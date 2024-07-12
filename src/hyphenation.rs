mod hyph_table;

use crate::dimension::Dimension;
use crate::eqtb::{Eqtb, FontIndex, IntegerVariable};
use crate::fonts::{new_character, CharTag, FontInfo, LigKernCommand};
use crate::input::Scanner;
use crate::logger::Logger;
use crate::nodes::{
    CharNode, DiscNode, KernNode, KernSubtype, LigatureNode, MathNodeKind, Node, WhatsitNode,
};
use crate::INIT;
pub use hyph_table::Hyphenator;

use std::convert::TryFrom;

const NON_CHAR: u16 = 256;

impl Hyphenator {
    /// See 863.
    pub fn hyphenate_hlist(
        &mut self,
        hlist: Vec<Node>,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> Vec<Node> {
        self.initialize_for_hyphenating_paragraph();

        let mut hyphenated_hlist = Vec::new();
        let mut nodes = hlist.into_iter();
        let mut auto_breaking = true;

        let mut word_hyphenator = WordHyphenator::new();

        let mut next_node = nodes.next();
        while let Some(node) = next_node {
            match node {
                Node::Char(_)
                | Node::List(_)
                | Node::Rule(_)
                | Node::Kern(_)
                | Node::Ligature(_)
                | Node::Disc(_)
                | Node::Penalty(_)
                | Node::Mark(_)
                | Node::Ins(_)
                | Node::Adjust(_) => {
                    hyphenated_hlist.push(node);
                }
                Node::Whatsit(ref whatsit_node) => {
                    self.advance_past_whatsit_node(whatsit_node);
                    hyphenated_hlist.push(node);
                }
                Node::Glue(_) => {
                    if auto_breaking {
                        hyphenated_hlist.push(node);
                        let l = hyphenated_hlist.len();
                        word_hyphenator.try_to_hyphenate_following_word(
                            &mut nodes,
                            &mut hyphenated_hlist,
                            self,
                            scanner,
                            eqtb,
                            logger,
                        );
                        // The last Node might be a MathNode or a LanguageNode that we need to process but we
                        // do not want to reprocess the initial GlueNode.
                        if hyphenated_hlist.len() > l {
                            next_node = hyphenated_hlist.pop();
                            continue;
                        }
                    } else {
                        hyphenated_hlist.push(node);
                    }
                }
                Node::Math(ref math_node) => {
                    auto_breaking = match math_node.kind {
                        MathNodeKind::Before => false,
                        MathNodeKind::After => true,
                    };
                    hyphenated_hlist.push(node);
                }
                Node::Unset(_) | Node::Style(_) | Node::Choice(_) | Node::Noad(_) => {
                    panic!("These nodes should never appear in a paragraph");
                }
            }
            next_node = nodes.next();
        }
        hyphenated_hlist
    }

    /// See 891.
    fn initialize_for_hyphenating_paragraph(&mut self) {
        if INIT && self.trie.is_none() {
            self.init_trie();
        }
        self.cur_lang = self.init_cur_lang;
        self.l_hyf = self.init_l_hyf;
        self.r_hyf = self.init_r_hyf;
    }

    /// See 1362. and 1363.
    fn advance_past_whatsit_node(&mut self, whatsit_node: &WhatsitNode) {
        if let WhatsitNode::Language(language_node) = whatsit_node {
            self.cur_lang = language_node.language;
            self.l_hyf = language_node.left_hyphen_min;
            self.r_hyf = language_node.right_hyphen_min;
        }
    }
}

struct WordHyphenator {
    // Information and structures used only during hyphenation of one word.
    // We keep reusing them to avoid new allocations.
    prefix: Vec<Node>,
    word: Vec<Node>,
    suffix: Vec<Node>,
    letters: Vec<u16>,
    lc_letters: Vec<u8>,
    pattern: Vec<u8>,
    hyphens: [bool; 65],

    /// Number of individual letters in the hyphenatable word.
    letter_count: usize,
    hyphen_char: u8,
    right_boundary_char: Option<u8>,
}

impl WordHyphenator {
    fn new() -> Self {
        Self {
            prefix: Vec::new(),
            word: Vec::new(),
            suffix: Vec::new(),
            letters: Vec::new(),
            lc_letters: Vec::new(),
            pattern: Vec::new(),
            hyphens: [false; 65],

            letter_count: 0,
            hyphen_char: 0,
            right_boundary_char: None,
        }
    }

    /// Clear reused structure for next word.
    fn clear(&mut self) {
        self.prefix.clear();
        self.word.clear();
        self.suffix.clear();
        self.letters.clear();
        self.lc_letters.clear();
        self.pattern.clear();
    }

    /// See 894.
    pub fn try_to_hyphenate_following_word(
        &mut self,
        nodes: &mut impl Iterator<Item = Node>,
        hyphenated_hlist: &mut Vec<Node>,
        hyphenator: &mut Hyphenator,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        // Clear internal data structures.
        self.clear();

        let Some((first_node, hyph_font_index)) = self.find_first_letter(nodes, hyphenator, eqtb)
        else {
            hyphenated_hlist.append(&mut self.prefix);
            return;
        };
        if hyphenator.l_hyf + hyphenator.r_hyf > 63 {
            hyphenated_hlist.append(&mut self.prefix);
            hyphenated_hlist.push(first_node);
            return;
        }
        let first_suffix_node = self.find_last_letter(first_node, hyph_font_index, nodes, eqtb);
        self.determine_right_boundary_char(&first_suffix_node, hyph_font_index, &eqtb.fonts);
        self.create_letters(eqtb);
        let hyphenation_allowed =
            self.check_that_suffix_allows_hyphenation(first_suffix_node, nodes, hyphenator);
        if !hyphenation_allowed {
            hyphenated_hlist.append(&mut self.prefix);
            hyphenated_hlist.append(&mut self.word);
            hyphenated_hlist.append(&mut self.suffix);
            return;
        }

        let Some(mut hyphenated_word) =
            self.hyphenate(hyph_font_index, hyphenator, scanner, eqtb, logger)
        else {
            hyphenated_hlist.append(&mut self.prefix);
            hyphenated_hlist.append(&mut self.word);
            hyphenated_hlist.append(&mut self.suffix);
            return;
        };

        hyphenated_hlist.append(&mut self.prefix);
        hyphenated_hlist.append(&mut hyphenated_word);
        hyphenated_hlist.append(&mut self.suffix);
    }

    /// Find the first node of the hyphenatable sequence and return this node and the associated
    /// font.
    /// See 896.
    fn find_first_letter(
        &mut self,
        nodes: &mut impl Iterator<Item = Node>,
        hyphenator: &mut Hyphenator,
        eqtb: &Eqtb,
    ) -> Option<(Node, FontIndex)> {
        let (first_node, hyph_font_index, character) = loop {
            let Some(node) = nodes.next() else {
                return None;
            };
            // Look for a character in a CharNode or the first consumed character in a LigatureNode
            // with a non-zero lc_code.
            match node {
                Node::Char(CharNode {
                    font_index,
                    character,
                    ..
                }) => {
                    let lc_code = eqtb.lc_code(character as usize);
                    if lc_code != 0 {
                        break (node, font_index, character);
                    }
                }
                Node::Ligature(LigatureNode {
                    font_index,
                    ref lig,
                    ..
                }) => {
                    if let Some(&character) = lig.first() {
                        let lc_code = eqtb.lc_code(character as usize);
                        if lc_code != 0 {
                            break (node, font_index, character);
                        }
                    }
                }
                // Implicit kerns and whatsit nodes are allowed to appear.
                Node::Kern(KernNode {
                    subtype: KernSubtype::Normal,
                    ..
                }) => {}
                Node::Whatsit(ref whatsit_node) => {
                    hyphenator.advance_past_whatsit_node(whatsit_node)
                }
                // Anything else aborts the hyphenation attempt.
                _ => {
                    self.prefix.push(node);
                    return None;
                }
            }
            self.prefix.push(node);
        };

        let lc_code = eqtb.lc_code(character as usize);
        if lc_code != character as i32 && eqtb.integer(IntegerVariable::UcHyph) <= 0 {
            self.prefix.push(first_node);
            return None;
        }
        let hyphen_char = eqtb.fonts[hyph_font_index as usize].hyphen_char;
        self.hyphen_char = match u8::try_from(hyphen_char) {
            Ok(hc) => hc,
            Err(_) => {
                self.prefix.push(first_node);
                return None;
            }
        };
        Some((first_node, hyph_font_index))
    }

    /// See 897. and 898.
    fn find_last_letter(
        &mut self,
        first_node: Node,
        hyph_font_index: FontIndex,
        nodes: &mut impl Iterator<Item = Node>,
        eqtb: &Eqtb,
    ) -> Node {
        self.letter_count = 0;
        let mut node = first_node;
        loop {
            match node {
                Node::Char(char_node) => {
                    if char_node.font_index != hyph_font_index {
                        break;
                    }
                    let c = char_node.character;
                    if eqtb.lc_code(c as usize) == 0 {
                        break;
                    }
                    if self.letter_count == 63 {
                        break;
                    }
                    self.word.push(node);
                    self.letter_count += 1;
                }
                Node::Ligature(ref ligature_node) => {
                    if ligature_node.font_index != hyph_font_index {
                        break;
                    }
                    if ligature_node
                        .lig
                        .iter()
                        .any(|&c| eqtb.lc_code(c as usize) == 0)
                    {
                        break;
                    }
                    if self.letter_count + ligature_node.lig.len() >= 63 {
                        break;
                    }
                    self.letter_count += ligature_node.lig.len();
                    self.word.push(node);
                }
                Node::Kern(KernNode {
                    subtype: KernSubtype::Normal,
                    ..
                }) => {
                    self.word.push(node);
                }
                _ => break,
            }
            node = match nodes.next() {
                Some(node) => node,
                None => {
                    panic!("Should not happen")
                }
            }
        }
        node
    }

    /// See 897. and 898.
    fn determine_right_boundary_char(
        &mut self,
        first_suffix_node: &Node,
        hyph_font_index: FontIndex,
        fonts: &[FontInfo],
    ) {
        self.right_boundary_char = None;
        match self.word.last().unwrap() {
            Node::Char(_) => {}
            Node::Ligature(ligature_node) => {
                if ligature_node.right_boundary {
                    self.right_boundary_char = fonts[hyph_font_index as usize].bchar;
                }
            }
            Node::Kern(KernNode {
                subtype: KernSubtype::Normal,
                ..
            }) => {
                self.right_boundary_char = fonts[hyph_font_index as usize].bchar;
            }
            _ => panic!("Should not happen"),
        }

        match first_suffix_node {
            Node::Char(char_node) => {
                if char_node.font_index == hyph_font_index {
                    self.right_boundary_char = Some(char_node.character);
                }
            }
            Node::Ligature(ligature_node) => {
                if ligature_node.font_index == hyph_font_index {
                    let lig = &ligature_node.lig;
                    if let Some(&character) = lig.first() {
                        self.right_boundary_char = Some(character);
                    }
                }
            }
            _ => {}
        }
    }

    /// See 897. and 898.
    fn create_letters(&mut self, eqtb: &Eqtb) {
        // Beginning and end of the word are marked with a zero byte.
        self.letters.push(0);
        self.lc_letters.push(0);
        for node in &self.word {
            match node {
                Node::Char(char_node) => {
                    let c = char_node.character;
                    self.letters.push(c as u16);
                    self.lc_letters.push(eqtb.lc_code(c as usize) as u8);
                }
                Node::Ligature(ligature_node) => {
                    let lig = &ligature_node.lig;
                    for &c in lig {
                        self.letters.push(c as u16);
                        self.lc_letters.push(eqtb.lc_code(c as usize) as u8);
                    }
                }
                Node::Kern(KernNode {
                    subtype: KernSubtype::Normal,
                    ..
                }) => {}
                _ => panic!("Should not happen"),
            }
        }
        // Beginning and end of the word are marked with a zero byte.
        self.lc_letters.push(0);
    }

    /// Return false for goto done1, true else.
    /// See 899.
    fn check_that_suffix_allows_hyphenation(
        &mut self,
        first_suffix_node: Node,
        nodes: &mut impl Iterator<Item = Node>,
        hyphenator: &Hyphenator,
    ) -> bool {
        if self.letter_count < hyphenator.l_hyf + hyphenator.r_hyf {
            self.suffix.push(first_suffix_node);
            return false;
        }
        let mut node = first_suffix_node;
        let hyphenation_allowed = loop {
            match node {
                Node::Char(_)
                | Node::Ligature(_)
                | Node::Kern(KernNode {
                    subtype: KernSubtype::Normal,
                    ..
                }) => {}
                Node::Kern(_)
                | Node::Whatsit(_)
                | Node::Glue(_)
                | Node::Penalty(_)
                | Node::Ins(_)
                | Node::Adjust(_)
                | Node::Mark(_) => break true,
                _ => break false,
            }
            self.suffix.push(node);
            node = match nodes.next() {
                Some(node) => node,
                None => panic!("Should not happen"),
            }
        };
        self.suffix.push(node);
        hyphenation_allowed
    }

    /// See 895.
    fn hyphenate(
        &mut self,
        hyph_font_index: FontIndex,
        hyphenator: &Hyphenator,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> Option<Vec<Node>> {
        if !hyphenator.find_hyphen_locations(&self.lc_letters, &mut self.pattern) {
            return None;
        };
        if self.no_hyphens_were_found(hyphenator) {
            return None;
        }
        for i in 0..self.pattern.len() {
            self.hyphens[i] = self.pattern[i] % 2 == 1;
        }
        Some(self.replace_letter_nodes(hyph_font_index, scanner, eqtb, logger))
    }

    /// Return true for goto return and else false.
    /// See 902.
    fn no_hyphens_were_found(&self, hyphenator: &Hyphenator) -> bool {
        for j in hyphenator.l_hyf..=(self.letter_count - hyphenator.r_hyf) {
            if self.pattern[j] % 2 == 1 {
                return false;
            }
        }
        true
    }

    /// See 903.
    fn replace_letter_nodes(
        &mut self,
        hyph_font_index: FontIndex,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> Vec<Node> {
        let initial_left_node = if let Some(&Node::Char(CharNode {
            font_index: font,
            character,
            ..
        })) = self.prefix.last()
        {
            if font != hyph_font_index {
                // The initial left node is a normal word boundary.
                Some(LeftNode::None)
            } else {
                // The initial left node is a char node.
                self.prefix.pop();
                Some(LeftNode::Char(character))
            }
        } else if let Some(&mut Node::Ligature(LigatureNode {
            font_index,
            character,
            ref mut lig,
            left_boundary,
            ..
        })) = self.prefix.last_mut()
        {
            if font_index != hyph_font_index {
                Some(LeftNode::None)
            } else {
                // The initial left node is a ligature.

                // If the ligature has been created only from a left boundary, we set the
                // initial left node to a normal left word boundary and just reconstruct the
                // ligature later.
                if lig.is_empty() && left_boundary {
                    self.prefix.pop();
                    Some(LeftNode::None)
                } else {
                    let consumed_chars = std::mem::take(lig);
                    self.prefix.pop();
                    Some(LeftNode::Lig(LeftLigItem {
                        character,
                        consumed_chars,
                        left_boundary,
                    }))
                }
            }
        } else if let Node::Ligature(LigatureNode {
            left_boundary: true,
            ..
        }) = self.word[0]
        {
            // The initial left node is a normal word boundary.
            Some(LeftNode::None)
        } else {
            // Start as if in the middle of a word.
            None
        };

        // common_ending:
        self.reconstitute_nodes_for_hyphenated_word(
            initial_left_node,
            hyph_font_index,
            scanner,
            eqtb,
            logger,
        )
    }

    /// Returns the reconstituted word.
    /// See 913.
    fn reconstitute_nodes_for_hyphenated_word(
        &mut self,
        initial_left_node: Option<LeftNode>,
        hyph_font_index: FontIndex,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> Vec<Node> {
        let mut s = Vec::new();
        let mut j = 0;
        loop {
            let mut l = j;
            let mut cursor = Cursor::new(
                hyph_font_index,
                &mut self.letters,
                &self.hyphens,
                initial_left_node.clone(),
            );
            let (tmp_j, mut cut_prefix, mut hyphen_passed) = cursor.reconstitute(
                j,
                self.letter_count,
                Some(self.hyphen_char),
                self.right_boundary_char,
                scanner,
                eqtb,
                logger,
            );
            j = tmp_j + 1;
            if hyphen_passed == 0 {
                s.append(&mut cut_prefix);
                if self.hyphens[j - 1] {
                    l = j;
                    hyphen_passed = j - 1;
                    cut_prefix = Vec::new();
                }
            }
            if hyphen_passed > 0 {
                self.create_and_append_discretionary(
                    &mut j,
                    l,
                    &mut s,
                    cut_prefix,
                    hyph_font_index,
                    initial_left_node.clone(),
                    hyphen_passed,
                    scanner,
                    eqtb,
                    logger,
                );
            }
            if j > self.letter_count {
                break;
            }
        }
        s
    }

    /// See 914.
    fn create_and_append_discretionary(
        &mut self,
        j: &mut usize,
        mut l: usize,
        s: &mut Vec<Node>,
        mut cut_prefix: Vec<Node>,
        hyph_font_index: FontIndex,
        initial_left_node: Option<LeftNode>,
        mut hyphen_passed: usize,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        loop {
            let mut major_tail = cut_prefix;
            let i = hyphen_passed;
            self.hyphens[i] = false;
            let pre_break = self.put_characters_and_hyphen(
                i,
                &mut l,
                hyph_font_index,
                initial_left_node.clone(),
                scanner,
                eqtb,
                logger,
            );
            let post_break = self.put_characters_into_post_break(
                &mut l,
                j,
                &mut major_tail,
                hyph_font_index,
                initial_left_node.clone(),
                scanner,
                eqtb,
                logger,
            );
            move_point_to_end_of_current_list(s, major_tail, pre_break, post_break);
            hyphen_passed = *j - 1;
            cut_prefix = Vec::new();
            if !self.hyphens[*j - 1] {
                break;
            }
        }
    }

    /// Return the pre_break.
    /// See 915.
    fn put_characters_and_hyphen(
        &mut self,
        mut i: usize,
        l: &mut usize,
        hyph_font_index: FontIndex,
        initial_left_node: Option<LeftNode>,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> Vec<Node> {
        let mut pre_break = Vec::new();
        let hyf_node = new_character(hyph_font_index, self.hyphen_char, eqtb, logger);
        let mut c = 0;
        // Temporarily store a hyphen in letters after the prefix text.
        if let Some(char_node) = hyf_node {
            i += 1;
            c = self.letters[i];
            self.letters[i] = char_node.character as u16;
        }
        while *l <= i {
            let bchar = eqtb.fonts[hyph_font_index as usize].bchar;
            let mut cursor = Cursor::new(
                hyph_font_index,
                &mut self.letters,
                &self.hyphens,
                initial_left_node.clone(),
            );
            let (tmp_j, mut cut_prefix, _) =
                cursor.reconstitute(*l, i, None, bchar, scanner, eqtb, logger);
            *l = tmp_j + 1;
            if !cut_prefix.is_empty() {
                pre_break.append(&mut cut_prefix);
            }
        }
        // Restore letters to its original state and move l one down.
        if hyf_node.is_some() {
            self.letters[i] = c;
            *l = i;
        }
        pre_break
    }

    /// Returns the post_break.
    /// See 916. and 917.
    fn put_characters_into_post_break(
        &mut self,
        l: &mut usize,
        j: &mut usize,
        major_tail: &mut Vec<Node>,
        hyph_font_index: FontIndex,
        initial_left_node: Option<LeftNode>,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> Vec<Node> {
        let mut post_break = Vec::new();
        let mut c_loc = 0;
        let mut c = 0;
        if eqtb.fonts[hyph_font_index as usize].bchar_label.is_some() {
            *l -= 1;
            c = self.letters[*l];
            c_loc = *l;
            self.letters[*l] = 256;
        }
        while *l < *j {
            loop {
                let mut cursor = Cursor::new(
                    hyph_font_index,
                    &mut self.letters,
                    &self.hyphens,
                    initial_left_node.clone(),
                );
                let (tmp_j, mut cut_prefix, _) = cursor.reconstitute(
                    *l,
                    self.letter_count,
                    None,
                    self.right_boundary_char,
                    scanner,
                    eqtb,
                    logger,
                );
                *l = tmp_j + 1;
                if c_loc > 0 {
                    self.letters[c_loc] = c;
                    c_loc = 0;
                }
                if !cut_prefix.is_empty() {
                    post_break.append(&mut cut_prefix);
                }
                if *l >= *j {
                    break;
                }
            }
            while *l > *j {
                let mut cursor = Cursor::new(
                    hyph_font_index,
                    &mut self.letters,
                    &self.hyphens,
                    initial_left_node.clone(),
                );
                let (tmp_j, mut cut_prefix, _) = cursor.reconstitute(
                    *j,
                    self.letter_count,
                    None,
                    self.right_boundary_char,
                    scanner,
                    eqtb,
                    logger,
                );
                *j = tmp_j + 1;
                major_tail.append(&mut cut_prefix);
            }
        }
        post_break
    }
}

/// See 905. and 907.
struct Cursor<'a> {
    font_index: FontIndex,
    letters: &'a mut Vec<u16>,
    hyphens: &'a [bool; 65],

    initial_left_node: Option<LeftNode>,
    cur_l: LeftNode,
    lig_stack: Vec<RightLigItem>,
    cur_r: Option<u8>,
    cur_rh: Option<u8>,

    right_hit: bool,

    cut_prefix: Vec<Node>,
    kerning: Dimension,
}

struct RightLigItem {
    character: u8,
    consumed_char: Option<u8>,
}

#[derive(Debug, Clone)]
enum LeftNode {
    Char(u8),
    Lig(LeftLigItem),
    None,
}

#[derive(Debug, Clone)]
struct LeftLigItem {
    character: u8,
    consumed_chars: Vec<u8>,
    left_boundary: bool,
}

impl<'a> Cursor<'a> {
    fn new(
        font_index: FontIndex,
        letters: &'a mut Vec<u16>,
        hyphens: &'a [bool; 65],
        initial_left_node: Option<LeftNode>,
    ) -> Self {
        Self {
            font_index,
            letters,
            hyphens,
            initial_left_node,
            cur_l: LeftNode::None,
            lig_stack: Vec::new(),
            cur_r: None,
            right_hit: false,
            cur_rh: None,
            cut_prefix: Vec::new(),
            kerning: 0,
        }
    }

    /// See 906. and 910.
    fn reconstitute(
        &mut self,
        mut pos: usize,
        last_pos: usize,
        mut hchar: Option<u8>,
        mut bchar: Option<u8>,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> (usize, Vec<Node>, usize) {
        let mut hyphen_passed = 0;
        self.kerning = 0;
        self.set_up_data_structures_with_cursor(&mut pos, last_pos, hchar, bchar);
        loop {
            let lig_kern_command = self.lookup_lig_kern_command(
                pos,
                &mut hchar,
                &mut hyphen_passed,
                &eqtb.fonts[self.font_index as usize],
            );
            if let Some(lig_kern_command) = lig_kern_command {
                match lig_kern_command {
                    LigKernCommand::Lig {
                        ligature,
                        delete_left,
                        delete_right,
                        moves,
                    } => {
                        if !self.carry_out_ligature_replacement(
                            ligature,
                            delete_left,
                            delete_right,
                            moves,
                            &mut pos,
                            last_pos,
                            hchar,
                            &mut bchar,
                            scanner,
                            eqtb,
                            logger,
                        ) {
                            continue;
                        }
                    }
                    LigKernCommand::Kern(wd) => {
                        self.kerning = wd;
                    }
                }
            }

            let left_node = std::mem::replace(&mut self.cur_l, LeftNode::None);
            let left_boundary = if let LeftNode::None = left_node {
                true
            } else {
                false
            };
            self.move_left_node_to_cut_prefix(
                self.right_hit,
                left_node,
                &eqtb.fonts[self.font_index as usize],
            );

            if let Some(lig_item) = self.lig_stack.pop() {
                let mut left_lig_item = LeftLigItem {
                    character: lig_item.character,
                    consumed_chars: Vec::new(),
                    left_boundary,
                };
                self.pop_lig_stack(
                    lig_item,
                    &mut left_lig_item,
                    &mut pos,
                    last_pos,
                    hchar,
                    bchar,
                );
                self.cur_l = LeftNode::Lig(left_lig_item);
            } else {
                return (pos, std::mem::take(&mut self.cut_prefix), hyphen_passed);
            }
        }
    }

    /// See 908.
    fn set_up_data_structures_with_cursor(
        &mut self,
        pos: &mut usize,
        last_pos: usize,
        hchar: Option<u8>,
        bchar: Option<u8>,
    ) {
        if *pos == 0 {
            if let Some(initial_left_node) = self.initial_left_node.take() {
                self.cur_l = initial_left_node;
            } else {
                *pos += 1;
                self.cur_l = match self.letters[*pos] {
                    NON_CHAR => LeftNode::None,
                    character => LeftNode::Char(character as u8),
                };
            }
        } else {
            self.cur_l = match self.letters[*pos] {
                NON_CHAR => LeftNode::None,
                character => LeftNode::Char(character as u8),
            };
        }
        self.set_cur_r(*pos, last_pos, hchar, bchar);
    }

    /// See 909.
    fn lookup_lig_kern_command(
        &mut self,
        pos: usize,
        hchar: &mut Option<u8>,
        hyphen_passed: &mut usize,
        font: &FontInfo,
    ) -> Option<LigKernCommand> {
        let lig_kern_program = match self.cur_l {
            LeftNode::None => match font.bchar_label {
                None => return None,
                Some(lig_index) => &font.lig_kerns[lig_index],
            },
            LeftNode::Char(character) | LeftNode::Lig(LeftLigItem { character, .. }) => {
                if let CharTag::Lig(lig_index) = font.tag(character) {
                    &font.lig_kerns[lig_index]
                } else {
                    return None;
                }
            }
        };

        // Potentially update hyphen_passed information.
        if let Some(c) = self.cur_rh {
            // If there exists a lig_kern command for a hyphen here.
            if lig_kern_program.get(&c).is_some() {
                *hyphen_passed = pos;
                *hchar = None;
            }
            self.cur_rh = None;
        }

        let Some(test_char) = self.cur_r else {
            return None;
        };
        match lig_kern_program.get(&test_char) {
            Some(lig_kern_command) => {
                // Potentially update hyphen_passed information.
                if hchar.is_some() && self.hyphens[pos] {
                    *hyphen_passed = pos;
                    *hchar = None;
                }
                Some(*lig_kern_command)
            }
            None => None,
        }
    }

    /// Returns false for goto continue or true for goto done.
    /// See 911.
    fn carry_out_ligature_replacement(
        &mut self,
        ligature: u8,
        delete_left: bool,
        delete_right: bool,
        moves: u8,
        pos: &mut usize,
        last_pos: usize,
        hchar: Option<u8>,
        bchar: &mut Option<u8>,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) -> bool {
        if *pos == last_pos && self.lig_stack.is_empty() {
            self.right_hit = true;
        }
        logger.check_interrupt(scanner, eqtb);
        let font = &eqtb.fonts[self.font_index as usize];
        match (delete_left, delete_right, moves) {
            //  =:|,  =:|>
            (true, false, _) => {
                let cur_l = std::mem::replace(&mut self.cur_l, LeftNode::None);
                let left_lig_item = LeftLigItem {
                    character: ligature,
                    left_boundary: match &cur_l {
                        LeftNode::None => true,
                        LeftNode::Char(_) => false,
                        LeftNode::Lig(lig_item) => lig_item.left_boundary,
                    },
                    consumed_chars: match cur_l {
                        LeftNode::None => Vec::new(),
                        LeftNode::Char(character) => vec![character],
                        LeftNode::Lig(LeftLigItem { consumed_chars, .. }) => consumed_chars,
                    },
                };
                self.cur_l = LeftNode::Lig(left_lig_item);
            }
            //  |=:,  |=:>
            (false, true, _) => {
                self.cur_r = Some(ligature);
                if let Some(lig_item) = self.lig_stack.last_mut() {
                    lig_item.character = ligature;
                } else {
                    let consumed_char = if *pos == last_pos {
                        *bchar = None;
                        None
                    } else {
                        Some(self.letters[*pos + 1] as u8)
                    };
                    self.lig_stack.push(RightLigItem {
                        character: ligature,
                        consumed_char,
                    });
                }
            }
            // |=:|
            (false, false, 0) => {
                self.cur_r = Some(ligature);
                self.lig_stack.push(RightLigItem {
                    character: ligature,
                    consumed_char: None,
                });
            }
            // |=:|>, |=:|
            (false, false, _) => {
                let left_node = std::mem::replace(&mut self.cur_l, LeftNode::None);
                let left_boundary = if let LeftNode::None = left_node {
                    true
                } else {
                    false
                };
                self.move_left_node_to_cut_prefix(false, left_node, font);
                self.cur_l = LeftNode::Lig(LeftLigItem {
                    character: ligature,
                    consumed_chars: Vec::new(),
                    left_boundary,
                });
            }
            // =:
            (true, true, _) => {
                let cur_l = std::mem::replace(&mut self.cur_l, LeftNode::None);
                let mut left_lig_item = LeftLigItem {
                    character: ligature,
                    left_boundary: match &cur_l {
                        LeftNode::None => true,
                        LeftNode::Char(_) => false,
                        LeftNode::Lig(lig_item) => lig_item.left_boundary,
                    },
                    consumed_chars: match cur_l {
                        LeftNode::None => Vec::new(),
                        LeftNode::Char(character) => vec![character],
                        LeftNode::Lig(LeftLigItem { consumed_chars, .. }) => consumed_chars,
                    },
                };
                if let Some(lig_item) = self.lig_stack.pop() {
                    self.pop_lig_stack(lig_item, &mut left_lig_item, pos, last_pos, hchar, *bchar);
                    self.cur_l = LeftNode::Lig(left_lig_item)
                } else if *pos == last_pos {
                    self.cur_l = LeftNode::Lig(left_lig_item);
                    return true;
                } else {
                    left_lig_item.consumed_chars.push(self.cur_r.unwrap());
                    self.cur_l = LeftNode::Lig(left_lig_item);
                    *pos += 1;
                    self.set_cur_r(*pos, last_pos, hchar, *bchar);
                }
            }
        }
        if moves > 0 && (moves == 2 || delete_left || delete_right) {
            return true;
        }
        false
    }

    /// See 910.
    fn move_left_node_to_cut_prefix(&mut self, b: bool, left_node: LeftNode, font: &FontInfo) {
        match left_node {
            LeftNode::Lig(lig_item) => {
                let right_boundary = if b && self.lig_stack.is_empty() {
                    self.right_hit = false;
                    true
                } else {
                    false
                };
                self.cut_prefix.push(Node::Ligature(LigatureNode {
                    font_index: self.font_index,
                    character: lig_item.character,
                    width: font.width(lig_item.character),
                    height: font.height(lig_item.character),
                    depth: font.depth(lig_item.character),
                    italic: font.italic(lig_item.character),
                    lig: lig_item.consumed_chars,
                    left_boundary: lig_item.left_boundary,
                    right_boundary,
                }));
            }
            LeftNode::Char(character) => {
                self.cut_prefix.push(Node::Char(CharNode {
                    font_index: self.font_index,
                    character,
                    width: font.width(character),
                    height: font.height(character),
                    depth: font.depth(character),
                    italic: font.italic(character),
                }));
            }
            LeftNode::None => {}
        }
        // Append a KernNode if the lig-kern program indicated it.
        if self.kerning != 0 {
            self.cut_prefix
                .push(Node::Kern(KernNode::new(self.kerning)));
            self.kerning = 0;
        }
    }

    /// See 910.
    fn pop_lig_stack(
        &mut self,
        lig_item: RightLigItem,
        left_lig_item: &mut LeftLigItem,
        pos: &mut usize,
        last_pos: usize,
        hchar: Option<u8>,
        bchar: Option<u8>,
    ) {
        if let Some(character) = lig_item.consumed_char {
            left_lig_item.consumed_chars.push(character);
            *pos += 1;
        }
        if let Some(lig_item) = self.lig_stack.last() {
            self.cur_r = Some(lig_item.character);
        } else {
            self.set_cur_r(*pos, last_pos, hchar, bchar);
        }
    }

    /// See 908.
    fn set_cur_r(&mut self, pos: usize, last_pos: usize, hchar: Option<u8>, bchar: Option<u8>) {
        if pos < last_pos {
            self.cur_r = match self.letters[pos + 1] {
                NON_CHAR => None,
                c => Some(c as u8),
            };
        } else {
            self.cur_r = bchar;
        }
        self.cur_rh = if self.hyphens[pos] { hchar } else { None }
    }
}

/// See 918.
fn move_point_to_end_of_current_list(
    s: &mut Vec<Node>,
    mut major_tail: Vec<Node>,
    pre_break: Vec<Node>,
    post_break: Vec<Node>,
) {
    if major_tail.len() > 127 {
        s.append(&mut major_tail);
    } else {
        let disc_node = DiscNode {
            pre_break,
            post_break,
            no_break: major_tail,
        };
        s.push(Node::Disc(disc_node));
    }
}
