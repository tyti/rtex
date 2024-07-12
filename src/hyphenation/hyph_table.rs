use crate::command::UnexpandableCommand;
use crate::eqtb::{Eqtb, IntegerVariable};
use crate::format::{Dumpable, FormatError};
use crate::input::expansion::get_x_token;
use crate::input::Scanner;
use crate::logger::Logger;
use crate::print::Printer;
use crate::token::Token;

use std::collections::HashMap;
use std::io::Write;

/// Stores all the hyphenation related information.
pub struct Hyphenator {
    /// Stores for each language the hyphenation exceptions.
    /// The information is given as a list of the hyphen positions.
    /// See 926.
    pub exceptions: [HashMap<Vec<u8>, Vec<usize>>; 256],

    pre_trie: PreTrie,

    /// See 950.
    pub trie: Option<Trie>,

    pub cur_lang: usize,
    /// Minimum number of characters before the first hyphen when hyphenating.
    pub l_hyf: usize,
    /// Minimum number of characters after the last hyphen when hyphenating.
    pub r_hyf: usize,

    /// The values of cur_lang, l_hyf, and r_hyf at the start of a paragraph.
    pub init_cur_lang: usize,
    pub init_l_hyf: usize,
    pub init_r_hyf: usize,
}

impl Hyphenator {
    pub fn new() -> Self {
        Self {
            exceptions: std::array::from_fn(|_| HashMap::new()),
            pre_trie: PreTrie::new(),
            trie: None,
            cur_lang: 0,
            l_hyf: 0,
            r_hyf: 0,
            init_cur_lang: 0,
            init_l_hyf: 0,
            init_r_hyf: 0,
        }
    }

    /// See 934.
    pub fn set_cur_lang(&mut self, eqtb: &Eqtb) {
        let language = eqtb.integer(IntegerVariable::Language);
        self.cur_lang = if language <= 0 || language > 255 {
            0
        } else {
            language as usize
        };
    }

    /// Returns false for Return, else true.
    /// See 923.
    pub fn find_hyphen_locations(&self, word: &[u8], pattern: &mut Vec<u8>) -> bool {
        pattern.clear();
        for _ in 0..word.len() - 1 {
            pattern.push(0);
        }
        // First look for the pattern in the exception table.
        if !self.find_pattern_in_exceptions(&word[1..word.len() - 1], pattern) {
            // Alternatively look for the pattern in the trie.
            if !self
                .trie
                .as_ref()
                .unwrap()
                .determine_hyph_pattern(word, self.cur_lang, pattern)
            {
                // Pattern was not found in the trie either.
                return false;
            }
        }

        // found:
        for j in 0..self.l_hyf {
            pattern[j] = 0;
        }
        for j in 0..self.r_hyf {
            pattern[word.len() - 2 - j] = 0;
        }
        true
    }

    /// If the word is in the exception table, store the corresponding pattern in `pattern` and
    /// return true, else return false.
    /// See 930., 931. and 932.
    fn find_pattern_in_exceptions(&self, word: &[u8], pattern: &mut Vec<u8>) -> bool {
        if let Some(hyphen_positions) = self.exceptions[self.cur_lang].get(word) {
            for &pos in hyphen_positions {
                pattern[pos] = 1;
            }
            true
        } else {
            false
        }
    }

    /// See 939., 940. and 941.
    fn enter_hyphenation_exception(&mut self, word: Vec<u8>, hyphen_positions: Vec<usize>) {
        self.exceptions[self.cur_lang].insert(word, hyphen_positions);
    }
}

/// See 920. and 921.
pub struct Trie {
    pub nodes: Vec<TrieNode>,
    hyf_ops: [Vec<HyfOp>; 256],
    op_code_hash: [HashMap<HyfOp, usize>; 256],
}

/// See 920. and 921.
#[derive(Clone, Copy)]
pub struct TrieNode {
    link: Option<usize>,
    chr: Option<u8>,
    op: Option<usize>,
}

impl Trie {
    /// See 923.
    fn determine_hyph_pattern(&self, word: &[u8], lang: usize, pattern: &mut Vec<u8>) -> bool {
        match self.nodes[lang + 1].chr {
            None => return false,
            Some(chr) => {
                if chr as usize != lang {
                    return false;
                }
            }
        }

        for j in 0..word.len() {
            let Some(mut base) = self.nodes[lang + 1].link else {
                continue;
            };
            for l in j..word.len() {
                let node = self.nodes[base + word[l] as usize];

                if node.chr != Some(word[l]) {
                    break;
                }

                if let Some(op) = node.op {
                    self.store_maximum_values_in_pattern(op, l, pattern, lang);
                }
                base = match node.link {
                    Some(base) => base,
                    None => break,
                }
            }
        }
        true
    }

    /// See 924.
    fn store_maximum_values_in_pattern(
        &self,
        op: usize,
        l: usize,
        pattern: &mut Vec<u8>,
        lang: usize,
    ) {
        let mut v = op;
        loop {
            let hyf_op = self.hyf_ops[lang][v];
            let i = l - hyf_op.distance;
            if hyf_op.num > pattern[i] {
                pattern[i] = hyf_op.num;
            }
            v = match hyf_op.next {
                Some(index) => index,
                None => break,
            }
        }
    }
}

/// See 920. and 921.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct HyfOp {
    distance: usize,
    num: u8,
    next: Option<usize>,
}

impl Hyphenator {
    /// See 934.
    pub fn new_hyph_exceptions(
        &mut self,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        scanner.scan_left_brace(eqtb, logger);
        self.set_cur_lang(eqtb);
        self.enter_as_many_hyphenations_exception_as_are_listed(scanner, eqtb, logger);
    }

    /// See 935.
    fn enter_as_many_hyphenations_exception_as_are_listed(
        &mut self,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        let mut word = Vec::new();
        let mut hyphen_positions = Vec::new();
        loop {
            let (unexpandable_command, _) = get_x_token(scanner, eqtb, logger);
            match unexpandable_command {
                UnexpandableCommand::Letter(c)
                | UnexpandableCommand::Other(c)
                | UnexpandableCommand::CharGiven(c) => append_new_letter_or_hyphen(
                    c,
                    &mut word,
                    &mut hyphen_positions,
                    scanner,
                    eqtb,
                    logger,
                ),
                UnexpandableCommand::CharNum => {
                    let c = scanner.scan_char_num(eqtb, logger);
                    append_new_letter_or_hyphen(
                        c,
                        &mut word,
                        &mut hyphen_positions,
                        scanner,
                        eqtb,
                        logger,
                    );
                }
                UnexpandableCommand::RightBrace(_) | UnexpandableCommand::Spacer => {
                    if word.len() > 1 {
                        self.enter_hyphenation_exception(word, hyphen_positions);
                    }
                    if let UnexpandableCommand::RightBrace(_) = unexpandable_command {
                        return;
                    }
                    word = Vec::new();
                    hyphen_positions = Vec::new();
                }
                _ => give_improper_hyphenation_error(scanner, eqtb, logger),
            }
        }
    }
}

/// See 936.
fn give_improper_hyphenation_error(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) {
    logger.print_err("Improper ");
    logger.print_esc_str(b"hyphenation");
    logger.print_str(" will be flushed");
    let help = &[
        "Hyphenation exceptions must contain only letters",
        "and hyphens. But continue; I'll forgive and forget.",
    ];
    logger.error(help, scanner, eqtb)
}

/// See 937. and 938.
fn append_new_letter_or_hyphen(
    chr: u8,
    word: &mut Vec<u8>,
    hyphen_positions: &mut Vec<usize>,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    if chr == b'-' {
        if word.len() < 63 {
            hyphen_positions.push(word.len());
        }
    } else if eqtb.lc_code(chr as usize) == 0 {
        logger.print_err("Not a letter");
        let help = &[
            "Letters in \\hyphenation words must have \\lccode>0.",
            "Proceed; I'll ignore the character I just read.",
        ];
        logger.error(help, scanner, eqtb);
    } else if word.len() < 63 {
        word.push(eqtb.lc_code(chr as usize) as u8);
    }
}

/// See 947.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct PreTrieNode {
    c: u8,
    op: Option<usize>,
    first_child: Option<usize>,
    next_sibling: Option<usize>,
}

/// A Trie that can accept new entries.
/// Keys are sequences of bytes, values are `usize`.
/// The very first node corresponds to the root (corresponding to an empty key).
/// See 947.
#[derive(Debug)]
struct PreTrie {
    nodes: Vec<PreTrieNode>,
    subtrie_hash: HashMap<PreTrieNode, usize>,
    hyf_ops: [Vec<HyfOp>; 256],
    op_code_hash: [HashMap<HyfOp, usize>; 256],
}

impl PreTrie {
    fn new() -> Self {
        Self {
            // We insert the root node corresponding to an empty key.
            nodes: vec![PreTrieNode {
                c: 0,
                op: None,
                first_child: None,
                next_sibling: None,
            }],
            subtrie_hash: HashMap::new(),
            hyf_ops: std::array::from_fn(|_| Vec::new()),
            op_code_hash: std::array::from_fn(|_| HashMap::new()),
        }
    }

    /// See 963.
    fn insert(&mut self, word: &[u8], op: Option<usize>) -> Result<(), ()> {
        // We start with the parent being the root.
        let mut parent = 0;
        for &c in word {
            let mut prev_child = None;
            let mut child = self.nodes[parent].first_child;
            while let Some(index) = child {
                if self.nodes[index].c >= c {
                    break;
                }
                prev_child = Some(index);
                child = self.nodes[index].next_sibling;
            }
            let pos = match child {
                None => self.push_new_trie_node(c, None),
                Some(index) => {
                    if self.nodes[index].c > c {
                        self.push_new_trie_node(c, Some(index))
                    } else {
                        index
                    }
                }
            };
            let prev_link = match prev_child {
                None => &mut self.nodes[parent].first_child,
                Some(prev_index) => &mut self.nodes[prev_index].next_sibling,
            };
            *prev_link = Some(pos);
            parent = pos;
        }
        if self.nodes[parent].op.is_some() {
            self.nodes[parent].op = op;
            Err(())
        } else {
            self.nodes[parent].op = op;
            Ok(())
        }
    }

    /// See 963.
    fn insert_new_pattern_into_linked_trie(
        &mut self,
        mut word: Vec<u8>,
        pattern: Vec<u8>,
        lang: usize,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        let v = self.compute_trie_op_code(&word, pattern, lang);
        // Use the language as the letter at index zero.
        word.insert(0, lang as u8);
        match self.insert(&word, v) {
            Ok(()) => {}
            Err(()) => {
                logger.print_err("Duplicate pattern");
                let help = &["(See Appendix H.)"];
                logger.error(help, scanner, eqtb);
            }
        }
    }

    /// Creates a new TrieNode and sets the given next sibling. Returns the
    /// corresponding index in the node list.
    /// See 964.
    fn push_new_trie_node(&mut self, c: u8, next_sibling: Option<usize>) -> usize {
        self.nodes.push(PreTrieNode {
            c,
            op: None,
            first_child: None,
            next_sibling,
        });
        self.nodes.len() - 1
    }

    /// See 965.
    fn compute_trie_op_code(
        &mut self,
        word: &[u8],
        mut pattern: Vec<u8>,
        lang: usize,
    ) -> Option<usize> {
        // We don't allow hyphenation before or after the virtual beginning-of-word and
        // ending-of-word marks.
        if let Some(0) = word.first() {
            pattern[0] = 0;
        }
        if let Some(0) = word.last() {
            pattern[word.len()] = 0;
        }
        let mut l = word.len();
        let mut v = None;
        loop {
            if pattern[l] != 0 {
                let hyf_op = HyfOp {
                    distance: word.len() - l,
                    num: pattern[l],
                    next: v,
                };
                v = Some(self.new_trie_op(hyf_op, lang));
            }
            if l > 0 {
                l -= 1;
            } else {
                return v;
            }
        }
    }

    /// See 944.
    fn new_trie_op(&mut self, hyf_op: HyfOp, lang: usize) -> usize {
        match self.op_code_hash[lang].get(&hyf_op) {
            None => {
                let u = self.hyf_ops[lang].len();
                // Add the opcode.
                self.hyf_ops[lang].push(hyf_op);
                // Associate this index with the hash entry.
                self.op_code_hash[lang].insert(hyf_op, u);
                u
            }
            Some(&u) => u,
        }
    }

    /// Removes reduncancy by using only one representative of each class of
    /// equivalent subtries.
    /// See 952.
    fn reduce(&mut self) {
        self.subtrie_hash = HashMap::new();

        // Deduplicate the node trie.
        self.nodes[0].first_child = self.reduce_subtrie(self.nodes[0].first_child);
    }

    /// See 949.
    fn reduce_subtrie(&mut self, subtrie_root: Option<usize>) -> Option<usize> {
        match subtrie_root {
            None => None,
            Some(index) => {
                let next_sibling = self.nodes[index].next_sibling;
                let first_child = self.nodes[index].first_child;
                self.nodes[index].next_sibling = self.reduce_subtrie(next_sibling);
                self.nodes[index].first_child = self.reduce_subtrie(first_child);
                Some(self.subtrie_representative(index))
            }
        }
    }

    /// Return the unique representative of equal subtries.
    /// See 948.
    fn subtrie_representative(&mut self, node_index: usize) -> usize {
        *self
            .subtrie_hash
            .entry(self.nodes[node_index])
            .or_insert(node_index)
    }

    /// See 947.
    fn trie_root(&self) -> Option<usize> {
        self.nodes[0].first_child
    }

    /// See 966.
    pub fn to_trie_mut(&mut self) -> Trie {
        // Remove redundancy in trie.
        self.reduce();

        // This stores for each smallest child, the corresponding base index in the compressed trie.
        let mut trie_ref = HashMap::new();
        // Is used to determine the layout of how to fit the trie into the compressed array.
        // The first node remains always available.
        let mut allocations = vec![AllocationCell {
            is_base: false,
            is_taken: false,
            prev: 0,
            next: 1,
        }];
        if let Some(index) = self.trie_root() {
            self.first_fit(index, &mut allocations, &mut trie_ref);
            self.trie_pack(index, &mut allocations, &mut trie_ref);
        }
        self.move_data_into_trie(allocations.len(), trie_ref)
    }

    /// See 953.
    fn first_fit(
        &self,
        p: usize,
        allocations: &mut Vec<AllocationCell>,
        trie_ref: &mut HashMap<usize, usize>,
    ) {
        let c = self.nodes[p].c;
        let mut z = 0;
        // Ensure that the base will be at index 1 or higher.
        while z <= c as usize {
            z = allocations[z].next;
        }
        let mut h;
        loop {
            h = z - c as usize;
            Self::enforce_trie_max_larger_than_trie(h, allocations);
            if allocations[h].is_base {
                // not_found:
                z = allocations[z].next;
                continue;
            }
            if self.all_characters_of_familiy_fit(p, h, allocations) {
                break;
            }

            // not_found:
            z = allocations[z].next;
        }
        // found:
        self.pack_family_into_trie(h, p, allocations, trie_ref);
    }

    /// See 954.
    fn enforce_trie_max_larger_than_trie(h: usize, allocations: &mut Vec<AllocationCell>) {
        if allocations.len() - h < 257 {
            loop {
                let pos = allocations.len();
                allocations.push(AllocationCell {
                    is_base: false,
                    is_taken: false,
                    next: pos + 1,
                    prev: pos - 1,
                });
                if allocations.len() - h == 257 {
                    break;
                }
            }
        }
    }

    /// Returns false for goto not_found and true for goto found.
    /// See 955.
    fn all_characters_of_familiy_fit(
        &self,
        p: usize,
        h: usize,
        allocations: &[AllocationCell],
    ) -> bool {
        let mut q = self.nodes[p].next_sibling;
        while let Some(index) = q {
            // If this spot is already in use, we need to find another hole.
            if allocations[h + self.nodes[index].c as usize].is_taken {
                return false;
            }
            q = self.nodes[index].next_sibling;
        }
        true
    }

    /// See 956.
    fn pack_family_into_trie(
        &self,
        h: usize,
        p: usize,
        allocations: &mut Vec<AllocationCell>,
        trie_ref: &mut HashMap<usize, usize>,
    ) {
        allocations[h].is_base = true;
        trie_ref.insert(p, h);
        let mut q = p;
        loop {
            let z = h + self.nodes[q].c as usize;

            // Reconnect the double linked list of unused spots.
            let l = allocations[z].prev;
            let r = allocations[z].next;
            allocations[r].prev = l;
            allocations[l].next = r;

            // Mark this spot as used.
            allocations[z].is_taken = true;

            match self.nodes[q].next_sibling {
                None => break,
                Some(index) => {
                    q = index;
                }
            }
        }
    }

    /// Recursively store the trie with root node p in the table trie.
    /// See 957.
    fn trie_pack(
        &self,
        mut p: usize,
        allocations: &mut Vec<AllocationCell>,
        trie_ref: &mut HashMap<usize, usize>,
    ) {
        loop {
            // Get first child of p.
            let q = self.nodes[p].first_child;
            // If this trie exists and if it has not yet been added, find
            // a fit and store it.
            if let Some(index) = q {
                if !trie_ref.contains_key(&index) {
                    self.first_fit(index, allocations, trie_ref);
                    self.trie_pack(index, allocations, trie_ref);
                }
            }
            // Move on to next sibling node.
            match self.nodes[p].next_sibling {
                None => break,
                Some(index) => {
                    p = index;
                }
            }
        }
    }

    /// See 958.
    fn move_data_into_trie(&self, len: usize, trie_ref: HashMap<usize, usize>) -> Trie {
        // We use this to "zero out" the unused spots.
        let unused_node = TrieNode {
            link: None,
            chr: None,
            op: None,
        };
        // If the trie is empty, zero out the first 257 entries.
        let mut nodes = match self.trie_root() {
            None => {
                vec![unused_node; 256 + 1]
            }
            Some(index) => {
                let mut nodes = vec![unused_node; len];
                // Write the actual trie data into the table trie.
                self.trie_fix(index, &mut nodes, &trie_ref);
                nodes
            }
        };
        nodes[0].chr = None;
        Trie {
            nodes,
            hyf_ops: self.hyf_ops.clone(),
            op_code_hash: self.op_code_hash.clone(),
        }
    }

    /// Write the data from the node trie to the table trie.
    /// See 959.
    fn trie_fix(&self, mut p: usize, nodes: &mut Vec<TrieNode>, trie_ref: &HashMap<usize, usize>) {
        // Get the reference index for p and its siblings.
        let z = trie_ref[&p];
        // Iterate through the siblings.
        loop {
            // The first child of p.
            let q = self.nodes[p].first_child;
            // The character of p.
            let c = self.nodes[p].c;
            // Set the reference index for p's child nodes.
            nodes[z + c as usize].link = q.map(|index| trie_ref[&index]);
            // Store the character and opcode.
            nodes[z + c as usize].chr = Some(c);
            nodes[z + c as usize].op = self.nodes[p].op;
            // If p has child nodes, recursively add their data.
            if let Some(index) = q {
                self.trie_fix(index, nodes, trie_ref);
            }
            // Move on to the next sibling.
            match self.nodes[p].next_sibling {
                None => break,
                Some(index) => {
                    p = index;
                }
            }
        }
    }
}

/// See 950.
struct AllocationCell {
    is_base: bool,
    is_taken: bool,
    next: usize,
    prev: usize,
}

impl Hyphenator {
    /// See 960.
    pub fn new_patterns(
        &mut self,
        token: Token,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        if self.trie.is_none() {
            self.set_cur_lang(eqtb);
            scanner.scan_left_brace(eqtb, logger);
            self.enter_all_patterns_into_linked_trie(scanner, eqtb, logger)
        } else {
            logger.print_err("Too late for ");
            logger.print_esc_str(b"patterns");
            let help = &["All patterns must be given before typesetting begins."];
            logger.error(help, scanner, eqtb);
            // Scan and ignore.
            let Token::CSToken { cs } = token else {
                panic!("Impossible")
            };
            scanner.scan_toks(cs, false, eqtb, logger);
        }
    }

    /// See 961.
    fn enter_all_patterns_into_linked_trie(
        &mut self,
        scanner: &mut Scanner,
        eqtb: &mut Eqtb,
        logger: &mut Logger,
    ) {
        let mut word = Vec::new();
        let mut pattern = vec![0];
        let mut digit_sensed = false;
        loop {
            let (unexpandable_command, _) = get_x_token(scanner, eqtb, logger);
            match unexpandable_command {
                UnexpandableCommand::Letter(c) | UnexpandableCommand::Other(c) => {
                    append_new_letter_or_hyphen_level(
                        c,
                        &mut digit_sensed,
                        &mut word,
                        &mut pattern,
                        scanner,
                        eqtb,
                        logger,
                    )
                }
                UnexpandableCommand::RightBrace(_) | UnexpandableCommand::Spacer => {
                    // If there is at least one character in current pattern
                    if !word.is_empty() {
                        self.pre_trie.insert_new_pattern_into_linked_trie(
                            word,
                            pattern,
                            self.cur_lang,
                            scanner,
                            eqtb,
                            logger,
                        );
                    }
                    if let UnexpandableCommand::RightBrace(_) = unexpandable_command {
                        return;
                    }
                    // Reset for next pattern.
                    word = Vec::new();
                    pattern = vec![0];
                    digit_sensed = false;
                }
                _ => {
                    logger.print_err("Bad ");
                    logger.print_esc_str(b"patterns");
                    let help = &["(See Appendix H.)"];
                    logger.error(help, scanner, eqtb);
                }
            }
        }
    }
}

/// See 962.
fn append_new_letter_or_hyphen_level(
    mut chr: u8,
    digit_sensed: &mut bool,
    word: &mut Vec<u8>,
    pattern: &mut Vec<u8>,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    if *digit_sensed || chr < b'0' || chr > b'9' {
        if chr == b'.' {
            chr = 0;
        } else {
            chr = eqtb.lc_code(chr as usize) as u8;
            if chr == 0 {
                logger.print_err("Nonletter");
                let help = &["(See Appendix H.)"];
                logger.error(help, scanner, eqtb);
            }
        }
        if word.len() < 63 {
            word.push(chr);
            pattern.push(0);
            *digit_sensed = false;
        }
    } else if word.len() < 63 {
        pattern[word.len()] = chr - b'0';
        *digit_sensed = true;
    }
}

impl Hyphenator {
    /// See 966.
    pub fn init_trie(&mut self) {
        self.trie = Some(self.pre_trie.to_trie_mut());
    }
}

impl Dumpable for Hyphenator {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        for map in &self.exceptions {
            map.dump(target)?;
        }

        self.pre_trie.dump(target)?;
        self.trie.dump(target)?;

        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let mut hyphenator = Hyphenator::new();

        for i in 0..hyphenator.exceptions.len() {
            let map = HashMap::undump(lines)?;
            hyphenator.exceptions[i] = map;
        }

        hyphenator.pre_trie = PreTrie::undump(lines)?;
        hyphenator.trie = Option::undump(lines)?;

        Ok(hyphenator)
    }
}

impl Dumpable for HyfOp {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.distance.dump(target)?;
        self.num.dump(target)?;
        self.next.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let distance = usize::undump(lines)?;
        let num = u8::undump(lines)?;
        let next = Option::undump(lines)?;
        Ok(Self {
            distance,
            num,
            next,
        })
    }
}

impl Dumpable for PreTrie {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.nodes.dump(target)?;
        self.subtrie_hash.dump(target)?;
        for ops in &self.hyf_ops {
            ops.dump(target)?;
        }
        for map in &self.op_code_hash {
            map.dump(target)?;
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let mut pre_trie = PreTrie::new();
        pre_trie.nodes = Vec::undump(lines)?;
        pre_trie.subtrie_hash = HashMap::undump(lines)?;
        for i in 0..pre_trie.hyf_ops.len() {
            let ops = Vec::undump(lines)?;
            pre_trie.hyf_ops[i] = ops;
        }
        for i in 0..pre_trie.op_code_hash.len() {
            let map = HashMap::undump(lines)?;
            pre_trie.op_code_hash[i] = map;
        }
        Ok(pre_trie)
    }
}

impl Dumpable for PreTrieNode {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.c.dump(target)?;
        self.op.dump(target)?;
        self.first_child.dump(target)?;
        self.next_sibling.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let c = u8::undump(lines)?;
        let op = Option::undump(lines)?;
        let first_child = Option::undump(lines)?;
        let next_sibling = Option::undump(lines)?;
        Ok(Self {
            c,
            op,
            first_child,
            next_sibling,
        })
    }
}

impl Dumpable for Trie {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.nodes.dump(target)?;
        for ops in &self.hyf_ops {
            ops.dump(target)?;
        }
        for map in &self.op_code_hash {
            map.dump(target)?;
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let nodes = Vec::undump(lines)?;
        let mut hyf_ops: [_; 256] = std::array::from_fn(|_| Vec::new());
        for i in 0..hyf_ops.len() {
            let ops = Vec::undump(lines)?;
            hyf_ops[i] = ops;
        }
        let mut op_code_hash: [_; 256] = std::array::from_fn(|_| HashMap::new());
        for i in 0..op_code_hash.len() {
            let map = HashMap::undump(lines)?;
            op_code_hash[i] = map;
        }
        Ok(Self {
            nodes,
            hyf_ops,
            op_code_hash,
        })
    }
}

impl Dumpable for TrieNode {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.link.dump(target)?;
        self.chr.dump(target)?;
        self.op.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let link = Option::undump(lines)?;
        let chr = Option::undump(lines)?;
        let op = Option::undump(lines)?;
        Ok(Self { link, chr, op })
    }
}
