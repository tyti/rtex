//! Here resides the structures that contain the font information that TeX needs as well
//! as methods to load such font information from a file.

use crate::command::{PrefixableCommand, UnexpandableCommand};
use crate::dimension::Dimension;
use crate::eqtb::{ControlSequence, Eqtb, FontIndex, FontVariable, IntegerVariable, NULL_FONT};
use crate::format::{Dumpable, FormatError};
use crate::input::Scanner;
use crate::integer::{Integer, IntegerExt};
use crate::logger::Logger;
use crate::nodes::{CharNode, GlueSpec};
use crate::print::Printer;
use crate::scaled::{xn_over_d, Scaled, UNITY};

use std::collections::HashMap;
use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;
use std::rc::Rc;

const SLANT_CODE: usize = 1;
pub const SPACE_CODE: usize = 2;
const SPACE_STRETCH_CODE: usize = 3;
pub const SPACE_SHRINK_CODE: usize = 4;
const X_HEIGHT_CODE: usize = 5;
const QUAD_CODE: usize = 6;
const EXTRA_SPACE_CODE: usize = 7;

// Originally b"TeXfonts:"
const TEX_FONT_AREA: &str = if cfg!(feature = "trip") {
    "./"
} else {
    "fonts/"
};

/// Contains all the information needed from a font.
/// See 541.
#[derive(Debug)]
pub struct FontInfo {
    pub check: u32,
    pub size: Dimension,
    pub dsize: Dimension,
    pub name: Vec<u8>,
    pub area: Vec<u8>,
    pub bc: u8,
    pub ec: u8,
    pub glue: Option<Rc<GlueSpec>>,
    /// Indicates whether the font has already been used/declared in the DVI file.
    pub used: bool,
    pub hyphen_char: i32,
    pub skew_char: i32,
    pub bchar_label: Option<usize>,
    pub bchar: Option<u8>,
    pub false_bchar: Option<u8>,

    char_infos: Vec<CharInfo>,
    widths: Vec<Dimension>,
    heights: Vec<Dimension>,
    depths: Vec<Dimension>,
    italics: Vec<Dimension>,
    pub lig_kerns: Vec<HashMap<u8, LigKernCommand>>,
    pub extens: Vec<ExtensibleRecipe>,
    /// NOTE: These are 0-indexed, not 1-indexed like the actual parameter numbers.
    pub params: Vec<Scaled>,
}

impl FontInfo {
    /// If is negative, it corresponds to a scaling factor. If positive, it corresponds
    /// to the intended size in points (when viewed as Scaled). In the latter case it
    /// may at most be 2048pt.
    /// `s` is either in -32768..=-1 or 1..2**27.
    /// See 560.
    pub fn from_file(
        path: &Path,
        s: SizeIndicator,
        hyphen_char: i32,
        skew_char: i32,
    ) -> Result<Self, TfmError> {
        let mut file = Self::open_tfm_file(path)?;
        let tfm_header = Self::read_tfm_size_fields(&mut file)?;
        let (font_check, dsize, size) = Self::read_tfm_header_table(&mut file, &tfm_header, s)?;
        let char_data = Self::read_character_data(&mut file, &tfm_header)?;
        let (widths, heights, depths, italics) =
            Self::read_box_dimensions(&mut file, &tfm_header, size)?;
        let (bch_label, bchar, lig_kerns) =
            Self::read_ligature_kern_program(&mut file, &tfm_header, &char_data, size)?;
        let extens = Self::read_extensible_character_recipes(&mut file, &tfm_header, &char_data)?;
        let params = Self::read_font_parameters(&mut file, tfm_header.np, size)?;

        // From 576.

        // If bchar exists as real character in the font, set false_bchar to None, otherwise set it
        // to bchar.
        let false_bchar = match bchar {
            Some(c)
                if c >= tfm_header.bc
                    && c <= tfm_header.ec
                    && char_data[(c - tfm_header.bc) as usize].width_index > 0 =>
            {
                None
            }
            _ => bchar,
        };

        let name = path
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .as_bytes()
            .to_vec();
        let area = path
            .parent()
            .unwrap_or(Path::new(""))
            .to_str()
            .unwrap()
            .as_bytes()
            .to_vec();

        Ok(Self {
            check: font_check,
            size,
            dsize,
            name,
            area,
            bc: tfm_header.bc,
            ec: tfm_header.ec,
            glue: None,
            used: false,
            hyphen_char,
            skew_char,
            bchar_label: bch_label,
            bchar,
            false_bchar,

            char_infos: char_data,
            widths,
            heights,
            depths,
            italics,
            lig_kerns,
            extens,
            params,
        })
    }

    /// See 563.
    fn open_tfm_file(path: &Path) -> Result<File, TfmError> {
        let mut path = match path.parent() {
            Some(p) if !p.as_os_str().is_empty() => path.to_path_buf(),
            _ => Path::new(TEX_FONT_AREA).join(path),
        };
        path.set_extension("tfm");
        match File::open(path) {
            Ok(file) => Ok(file),
            Err(_) => Err(TfmError::FileNotFound),
        }
    }

    /// See 565.
    fn read_tfm_size_fields(file: &mut File) -> Result<TfmHeader, TfmError> {
        let lf = file.read_sixteen()? as usize;
        let lh = file.read_sixteen()? as usize;
        let mut bc = file.read_sixteen()? as usize;
        let mut ec = file.read_sixteen()? as usize;
        if bc > ec + 1 || ec > 255 {
            return Err(TfmError::BadFormat);
        }
        // If the font contains no characters, normalize the range.
        if bc > 255 {
            bc = 1;
            ec = 0;
        }
        let nw = file.read_sixteen()? as usize;
        let nh = file.read_sixteen()? as usize;
        let nd = file.read_sixteen()? as usize;
        let ni = file.read_sixteen()? as usize;
        let nl = file.read_sixteen()? as usize;
        let nk = file.read_sixteen()? as usize;
        let ne = file.read_sixteen()? as usize;
        let np = file.read_sixteen()? as usize;
        if lf != 6 + lh + (ec + 1 - bc) + nw + nh + nd + ni + nl + nk + ne + np {
            return Err(TfmError::BadFormat);
        }
        if nw == 0 || nh == 0 || nd == 0 || ni == 0 {
            return Err(TfmError::BadFormat);
        }
        Ok(TfmHeader {
            lh,
            bc: bc as u8,
            ec: ec as u8,
            nw,
            nh,
            nd,
            ni,
            nl,
            nk,
            ne,
            np,
        })
    }

    /// See 568.
    fn read_tfm_header_table(
        file: &mut File,
        tfm_header: &TfmHeader,
        s: SizeIndicator,
    ) -> Result<(u32, Scaled, Scaled), TfmError> {
        if tfm_header.lh < 2 {
            return Err(TfmError::BadFormat);
        }
        // Scan check sum.
        let fq = file.get_four_quarters()?;
        let font_check = u32::from_be_bytes(fq);

        // Scan the design size.
        // We need to ensure that the top bit is 0, i.e., that it is non-negative.
        let z1 = file.read_sixteen()?;
        let z2 = file.read_u16()?;
        // We need to translate TFM 12-bit fixed point to TEX 16-bit fixed point.
        let z = ((z1 as usize) << 12 ^ (z2 as usize) >> 4) as i32;
        // The design size must be at least 1.0.
        if z < UNITY {
            return Err(TfmError::BadFormat);
        }

        // Skip the remainder of the header table.
        let mut lh = tfm_header.lh;
        while lh > 2 {
            file.get_four_quarters()?;
            lh -= 1;
        }
        let dsize = z;
        let size = match s {
            SizeIndicator::AtSize(size) => size,
            SizeIndicator::Factor(n) => xn_over_d(z, n as i32, 1000)
                .expect("An already large font has been scaled too large"),
        };
        Ok((font_check, dsize, size))
    }

    /// See 569.
    fn read_character_data(
        file: &mut File,
        tfm_header: &TfmHeader,
    ) -> Result<Vec<CharInfo>, TfmError> {
        let mut char_data = Vec::new();
        for cur_char in tfm_header.bc..=tfm_header.ec {
            let [a, b, c, d] = file.get_four_quarters()?;
            let char_info = CharInfo {
                width_index: a as usize,
                height_index: b as usize / 16,
                depth_index: b as usize % 16,
                italic_index: c as usize / 4,
                tag: match c % 4 {
                    0 => CharTag::None,
                    1 => CharTag::Lig(d as usize),
                    2 => CharTag::List(d),
                    3 => CharTag::Ext(d as usize),
                    _ => panic!("Impossible"),
                },
            };
            if char_info.width_index >= tfm_header.nw
                || char_info.height_index >= tfm_header.nh
                || char_info.depth_index >= tfm_header.nd
                || char_info.italic_index >= tfm_header.ni
            {
                return Err(TfmError::BadFormat);
            }
            match char_info.tag {
                CharTag::Lig(lig_index) => {
                    if lig_index >= tfm_header.nl {
                        return Err(TfmError::BadFormat);
                    }
                }
                CharTag::Ext(ext_index) => {
                    if ext_index >= tfm_header.ne {
                        return Err(TfmError::BadFormat);
                    }
                }
                CharTag::List(next_char) => {
                    Self::check_charlist_cycle(cur_char, &char_data, next_char, tfm_header)?
                }
                _ => {}
            }
            char_data.push(char_info);
        }
        Ok(char_data)
    }

    /// See 570.
    fn check_byte_range(c: u8, tfm_header: &TfmHeader) -> Result<(), TfmError> {
        if c < tfm_header.bc || c > tfm_header.ec {
            Err(TfmError::BadFormat)
        } else {
            Ok(())
        }
    }

    /// See 570.
    fn check_charlist_cycle(
        cur_char: u8,
        char_data: &[CharInfo],
        mut next_char: u8,
        tfm_header: &TfmHeader,
    ) -> Result<(), TfmError> {
        Self::check_byte_range(next_char, tfm_header)?;
        while next_char < cur_char {
            match char_data[(next_char - tfm_header.bc) as usize].tag {
                CharTag::List(c) => {
                    next_char = c;
                }
                _ => return Ok(()),
            }
        }
        if next_char == cur_char {
            return Err(TfmError::BadFormat);
        }
        Ok(())
    }

    /// See 571.
    fn get_scaled(file: &mut File, z: i32, alpha: i32, beta: i32) -> Result<Scaled, TfmError> {
        let fq = file.get_four_quarters()?;
        let a = fq[0] as i32;
        let b = fq[1] as i32;
        let c = fq[2] as i32;
        let d = fq[3] as i32;
        let sw = (((d * z / 0o400 + c * z) / 0o400) + b * z) / beta;
        if a == 0 {
            Ok(sw)
        } else if a == 255 {
            Ok(sw - alpha)
        } else {
            Err(TfmError::BadFormat)
        }
    }

    /// See 571.
    fn read_box_dimensions(
        file: &mut File,
        tfm_header: &TfmHeader,
        size: Scaled,
    ) -> Result<(Vec<Scaled>, Vec<Scaled>, Vec<Scaled>, Vec<Scaled>), TfmError> {
        let mut z = size;
        let (alpha, beta) = Self::replace_z_by_z_prime_and_compute_alpha_and_beta(&mut z);
        let mut widths = Vec::new();
        for _ in 0..tfm_header.nw {
            widths.push(Self::get_scaled(file, z, alpha, beta)?);
        }
        let mut heights = Vec::new();
        for _ in 0..tfm_header.nh {
            heights.push(Self::get_scaled(file, z, alpha, beta)?);
        }
        let mut depths = Vec::new();
        for _ in 0..tfm_header.nd {
            depths.push(Self::get_scaled(file, z, alpha, beta)?);
        }
        let mut italics = Vec::new();
        for _ in 0..tfm_header.ni {
            italics.push(Self::get_scaled(file, z, alpha, beta)?);
        }
        if widths[0] != 0 || heights[0] != 0 || depths[0] != 0 || italics[0] != 0 {
            return Err(TfmError::BadFormat);
        }
        Ok((widths, heights, depths, italics))
    }

    /// Find e such that z*2^{-e} < 2^23. Set z to z*2^{-e} and beta to 16*2^{-e}.
    /// Set alpha to 16*size.
    /// See 572.
    fn replace_z_by_z_prime_and_compute_alpha_and_beta(z: &mut Scaled) -> (Scaled, Scaled) {
        let mut alpha = 16;
        while *z >= 0o40000000 {
            *z /= 2;
            alpha *= 2;
        }
        let beta = 256 / alpha;
        alpha *= *z;
        (alpha, beta)
    }

    /// See 573.
    fn check_existence(
        char_data: &[CharInfo],
        c: u8,
        tfm_header: &TfmHeader,
    ) -> Result<(), TfmError> {
        Self::check_byte_range(c, tfm_header)?;
        // char_exists (width > 0)
        if char_data[(c - tfm_header.bc) as usize].width_index > 0 {
            Ok(())
        } else {
            Err(TfmError::BadFormat)
        }
    }

    /// See 573.
    fn read_ligature_kern_program(
        file: &mut File,
        tfm_header: &TfmHeader,
        char_data: &[CharInfo],
        size: Scaled,
    ) -> Result<(Option<usize>, Option<u8>, Vec<HashMap<u8, LigKernCommand>>), TfmError> {
        let mut bch_label = None;
        let mut bchar = None;

        // We store the data in the orginal format and extract the lig/kern information in a second pass.
        // We only do the validity checks in this pass as well as extracting the bchar and
        // bchar_label information, so that we do not need to do them again.
        let mut lig_kern_table = Vec::new();
        if tfm_header.nl > 0 {
            for k in 0..tfm_header.nl {
                let [a, b, c, d] = file.get_four_quarters()?;
                lig_kern_table.push((a, b, c, d));

                // If the first byte is larger than 128, it will be interpreted as a long jump
                // in the table if this is the entry point in the table. It is therefore required
                // that such a jump will lead to a valid position.
                if a > 128 {
                    if 256 * c as usize + d as usize >= tfm_header.nl {
                        return Err(TfmError::BadFormat);
                    }
                    // If the first byte in the first position is 255, the second byte denotes the
                    // value for the right boundary character.
                    if k == 0 && a == 255 {
                        bchar = Some(b);
                    }
                    // If the first byte in the last position is 255, the long address denotes the
                    // start of the ligature program for the left boundary.
                    if k == tfm_header.nl - 1 && a == 255 {
                        bch_label = Some(256 * c as usize + d as usize);
                    }
                } else {
                    // Apart from right boundary character, we require all characters to exist in
                    // the font.
                    if Some(b) != bchar {
                        Self::check_existence(char_data, b, tfm_header)?;
                    }
                    // If the third byte is smaller 128, this is a ligature. Otherwise it is a
                    // kern.
                    if c < 128 {
                        // The ligature (the fourth byte) must exist in the font.
                        Self::check_existence(char_data, d, tfm_header)?;
                    // The address in the kern table must be valid.
                    } else if 256 * (c as usize - 128) + d as usize >= tfm_header.nk {
                        return Err(TfmError::BadFormat);
                    }
                    // If the first byte is less than 128, there is a next instruction in the chain
                    // which is reached by skipping over the next a positions in the array. We need
                    // to ensure that these jumps lead to valid adresses.
                    if a < 128 {
                        if k + a as usize + 1 >= tfm_header.nl {
                            return Err(TfmError::BadFormat);
                        }
                    }
                }
            }
        }

        // We scan the kerning table now so that we can use the value directly.
        let mut z = size;
        let (alpha, beta) = Self::replace_z_by_z_prime_and_compute_alpha_and_beta(&mut z);
        let mut kern_table = Vec::new();
        for _ in 0..tfm_header.nk {
            kern_table.push(Self::get_scaled(file, z, alpha, beta)?);
        }

        // Now we need a useful representation of the ligature/kerning programs from the table.
        let mut lig_kerns = Vec::new();
        // We only need to consider ligature/kerning programs starting at the first 256 positions,
        // as those are the only possible starting positions.
        for i in 0..std::cmp::min(256, tfm_header.nl) {
            // We need to potentially make a long jump into the table first before properly
            // starting.
            let pos = if lig_kern_table[i].0 > 128 {
                256 * lig_kern_table[i].2 as usize + lig_kern_table[i].3 as usize
            } else {
                i
            };
            // We can now determine the commands for the program starting in this position and
            // put them into the table.
            let commands = Self::extract_lig_kern_commands(pos, &lig_kern_table, &kern_table);
            lig_kerns.push(commands);
        }

        // We need not forget to read the program for the left boundary if it exists.
        let bchar_label = match bch_label {
            Some(pos) => {
                let left_boundary_commands =
                    Self::extract_lig_kern_commands(pos, &lig_kern_table, &kern_table);
                lig_kerns.push(left_boundary_commands);
                Some(lig_kerns.len() - 1)
            }
            None => None,
        };

        Ok((bchar_label, bchar, lig_kerns))
    }

    /// Extracts a representation of the ligature/kerning program starting in position `pos` of the
    /// lig/kern table.
    /// See 573.
    fn extract_lig_kern_commands(
        mut pos: usize,
        lig_kern_table: &[(u8, u8, u8, u8)],
        kern_table: &[Scaled],
    ) -> HashMap<u8, LigKernCommand> {
        let mut commands = HashMap::new();
        loop {
            let (a, b, c, d) = lig_kern_table[pos];
            // If the first byte is larger than 128, we are done and do not look at the
            // instructions here.
            if a > 128 {
                break;
            }
            // We don't want to overwrite an already existing command for a letter.
            if !commands.contains_key(&b) {
                let key = b;
                // A value of at least 128 in the third byte indicates a kerning instruction.
                // The size of the kerning can be found in the kerning table at a position
                // indicated by the third and fourth byte.
                let lig_kern_command = if c >= 128 {
                    LigKernCommand::Kern(kern_table[256 * (c as usize - 128) + d as usize])
                // In the case of a ligature, the fourth byte is the character code of the ligature
                // while the third byte details how the ligature is formed:
                //  * A zero in the lowest bit position (bit 0) means that the next character is deleted.
                //  * A zero in the next lowest bit position (bit 1) means that the current character is deleted.
                //  * Bits 2 and 3 indicate the number of characters that we are passing over.
                } else {
                    let delete_left = (c / 2) % 2 == 0;
                    let delete_right = c % 2 == 0;
                    let moves = c / 4;
                    let allowed_moves = match (delete_left, delete_right) {
                        (false, false) => 2,
                        (true, false) | (false, true) => 1,
                        (true, true) => 0,
                    };
                    if moves > allowed_moves {
                        panic!("Corrupted TFM file");
                    }
                    LigKernCommand::Lig {
                        ligature: d,
                        delete_left,
                        delete_right,
                        moves: c / 4,
                    }
                };
                commands.insert(key, lig_kern_command);
            }
            // A value of 128 in the first byte means that this is the final step in the list
            // of commands.
            if a == 128 {
                break;
            }
            // We skip to the next position in the lig/kern table.
            pos += 1 + a as usize;
        }
        commands
    }

    /// See 574.
    fn read_extensible_character_recipes(
        file: &mut File,
        tfm_header: &TfmHeader,
        char_data: &[CharInfo],
    ) -> Result<Vec<ExtensibleRecipe>, TfmError> {
        let mut extens = Vec::new();
        for _ in 0..tfm_header.ne {
            let [a, b, c, d] = file.get_four_quarters()?;
            let recipe = ExtensibleRecipe {
                top: if a != 0 { Some(a) } else { None },
                mid: if b != 0 { Some(b) } else { None },
                bot: if c != 0 { Some(c) } else { None },
                rep: d,
            };

            // Check that all used characters exist in the font.
            if let Some(c) = recipe.top {
                Self::check_existence(char_data, c, tfm_header)?;
            }
            if let Some(c) = recipe.mid {
                Self::check_existence(char_data, c, tfm_header)?;
            }
            if let Some(c) = recipe.bot {
                Self::check_existence(char_data, c, tfm_header)?;
            }
            Self::check_existence(char_data, recipe.rep, tfm_header)?;

            extens.push(recipe);
        }
        Ok(extens)
    }

    /// See 575.
    fn read_font_parameters(
        file: &mut File,
        param_num: usize,
        size: Scaled,
    ) -> Result<Vec<Scaled>, TfmError> {
        let mut z = size;
        let (alpha, beta) = Self::replace_z_by_z_prime_and_compute_alpha_and_beta(&mut z);
        let mut params = Vec::new();
        for k in 0..param_num {
            if k == 0 {
                let fq = file.get_four_quarters()?;
                let sw = Scaled::from_be_bytes(fq);
                params.push(sw / 0x10);
            } else {
                params.push(Self::get_scaled(file, z, alpha, beta)?);
            }
        }
        // Ensure there are at least 7 parameters
        if param_num < 7 {
            params.resize(7, 0);
        }
        Ok(params)
    }

    pub fn null_font() -> Self {
        Self {
            check: 0,
            size: 0,
            dsize: 0,
            name: b"nullfont".to_vec(),
            area: b"".to_vec(),
            bc: 1,
            ec: 0,
            glue: None,
            used: false,
            hyphen_char: b'-' as i32,
            skew_char: -1,
            bchar_label: None,
            bchar: None,
            false_bchar: None,

            char_infos: Vec::new(),
            widths: Vec::new(),
            heights: Vec::new(),
            depths: Vec::new(),
            italics: Vec::new(),
            lig_kerns: Vec::new(),
            extens: Vec::new(),
            params: vec![0; 7],
        }
    }

    fn char_info(&self, c: u8) -> &CharInfo {
        &self.char_infos[(c - self.bc) as usize]
    }

    pub fn width(&self, c: u8) -> Dimension {
        self.widths[self.char_info(c).width_index]
    }

    pub fn height(&self, c: u8) -> Dimension {
        self.heights[self.char_info(c).height_index]
    }

    pub fn depth(&self, c: u8) -> Dimension {
        self.depths[self.char_info(c).depth_index]
    }

    pub fn italic(&self, c: u8) -> Dimension {
        self.italics[self.char_info(c).italic_index]
    }

    pub fn tag(&self, c: u8) -> CharTag {
        self.char_info(c).tag
    }

    pub fn char_exists(&self, c: u8) -> bool {
        self.char_info(c).width_index > 0
    }

    pub fn slant(&self) -> Scaled {
        self.params[SLANT_CODE - 1]
    }

    pub fn space(&self) -> Scaled {
        self.params[SPACE_CODE - 1]
    }

    pub fn space_stretch(&self) -> Scaled {
        self.params[SPACE_STRETCH_CODE - 1]
    }

    pub fn space_shrink(&self) -> Scaled {
        self.params[SPACE_SHRINK_CODE - 1]
    }

    pub fn x_height(&self) -> Scaled {
        self.params[X_HEIGHT_CODE - 1]
    }

    pub fn quad(&self) -> Scaled {
        self.params[QUAD_CODE - 1]
    }

    pub fn extra_space(&self) -> Scaled {
        self.params[EXTRA_SPACE_CODE - 1]
    }
}

pub enum SizeIndicator {
    /// `s` is either in -32768..=-1 or 1..2^27.
    /// Indicates that the font should be scaled by this factor where the factor
    /// will be divided by 1000, i.e., 1000 corresponds to the default case.
    /// The value has to be at least 1 and at most 32768.
    Factor(u32),
    /// Indicates the intended size of the font. Has to be less than 2048.0 pt.
    /// The value has to be positive and less than 2^27 (when viewed as integer).
    AtSize(Scaled),
}

pub enum TfmError {
    FileNotFound,
    BadFormat,
}

/// This struct encapsulates the information in the TFM Header (not the header data).
/// All lengths are in 32-bit words.
/// See 540.
struct TfmHeader {
    /// The length of the header data in 32-bit words.
    lh: usize,
    /// The smallest character code in the font.
    bc: u8,
    /// The largest character code in the font.
    ec: u8,
    /// The number of 32-bit words in the width table.
    nw: usize,
    /// The number of 32-bit words in the height table.
    nh: usize,
    /// The number of 32-bit words in the depth table.
    nd: usize,
    /// The number of 32-bit words in the italic correction table.
    ni: usize,
    /// The number of 32-bit words in the lig/kern table.
    nl: usize,
    /// The number of 32-bit words in the kern table.
    nk: usize,
    /// The number of 32-bit words in the extensible character table.
    ne: usize,
    /// The number of 32-bit words in the font parameter table.
    np: usize,
}

#[derive(Debug)]
pub struct CharInfo {
    width_index: usize,
    height_index: usize,
    depth_index: usize,
    italic_index: usize,
    pub tag: CharTag,
}

/// See 544.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CharTag {
    None,
    Lig(usize),
    List(u8),
    Ext(usize),
}

/// See 545.
#[derive(Debug, Clone, Copy)]
pub enum LigKernCommand {
    Kern(Dimension),
    Lig {
        ligature: u8,
        delete_left: bool,
        delete_right: bool,
        moves: u8,
    },
}

/// See 546.
#[derive(Debug, Clone, Copy)]
pub struct ExtensibleRecipe {
    pub top: Option<u8>,
    pub mid: Option<u8>,
    pub bot: Option<u8>,
    pub rep: u8,
}

trait TfmRead {
    fn read_sixteen(&mut self) -> Result<u16, TfmError>;
    fn read_u16(&mut self) -> Result<u16, TfmError>;
    fn get_four_quarters(&mut self) -> Result<[u8; 4], TfmError>;
}

impl TfmRead for File {
    /// See 564.
    fn read_sixteen(&mut self) -> Result<u16, TfmError> {
        let mut mini_buf: [u8; 2] = [0, 0];
        if self.read_exact(&mut mini_buf).is_err() {
            return Err(TfmError::BadFormat);
        }
        let num = u16::from_be_bytes(mini_buf);
        if num >= (1 << 15) {
            Err(TfmError::BadFormat)
        } else {
            Ok(num)
        }
    }

    /// This does the same as `read_sixteen` without checking that the leading bit is zero.
    fn read_u16(&mut self) -> Result<u16, TfmError> {
        let mut mini_buf: [u8; 2] = [0, 0];
        if self.read_exact(&mut mini_buf).is_err() {
            return Err(TfmError::BadFormat);
        }
        let num = u16::from_be_bytes(mini_buf);
        Ok(num)
    }

    /// See 564.
    fn get_four_quarters(&mut self) -> Result<[u8; 4], TfmError> {
        let mut mini_buf: [u8; 4] = [0, 0, 0, 0];
        match self.read_exact(&mut mini_buf) {
            Err(_) => Err(TfmError::BadFormat),
            Ok(_) => Ok(mini_buf),
        }
    }
}

impl Dumpable for FontInfo {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.check.dump(target)?;
        self.size.dump(target)?;
        self.dsize.dump(target)?;
        self.name.dump(target)?;
        self.area.dump(target)?;
        self.bc.dump(target)?;
        self.ec.dump(target)?;
        self.glue.dump(target)?;
        self.used.dump(target)?;
        self.hyphen_char.dump(target)?;
        self.skew_char.dump(target)?;
        self.bchar_label.dump(target)?;
        self.bchar.dump(target)?;
        self.false_bchar.dump(target)?;
        self.char_infos.dump(target)?;
        self.widths.dump(target)?;
        self.heights.dump(target)?;
        self.depths.dump(target)?;
        self.italics.dump(target)?;
        self.lig_kerns.dump(target)?;
        self.extens.dump(target)?;
        self.params.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let check = u32::undump(lines)?;
        let size = Dimension::undump(lines)?;
        let dsize = Dimension::undump(lines)?;
        let name = Vec::undump(lines)?;
        let area = Vec::undump(lines)?;
        let bc = u8::undump(lines)?;
        let ec = u8::undump(lines)?;
        let glue = Option::undump(lines)?;
        let used = bool::undump(lines)?;
        let hyphen_char = i32::undump(lines)?;
        let skew_char = i32::undump(lines)?;
        let bchar_label = Option::undump(lines)?;
        let bchar = Option::undump(lines)?;
        let false_bchar = Option::undump(lines)?;
        let char_infos = Vec::undump(lines)?;
        let widths = Vec::undump(lines)?;
        let heights = Vec::undump(lines)?;
        let depths = Vec::undump(lines)?;
        let italics = Vec::undump(lines)?;
        let lig_kerns = Vec::undump(lines)?;
        let extens = Vec::undump(lines)?;
        let params = Vec::undump(lines)?;
        Ok(Self {
            check,
            size,
            dsize,
            name,
            area,
            bc,
            ec,
            glue,
            used,
            hyphen_char,
            skew_char,
            bchar_label,
            bchar,
            false_bchar,
            char_infos,
            widths,
            heights,
            depths,
            italics,
            lig_kerns,
            extens,
            params,
        })
    }
}

/// See 577.
pub fn scan_font_ident(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) -> FontIndex {
    let (unexpandable_command, token) = scanner.get_next_non_blank_non_call_token(eqtb, logger);
    match unexpandable_command {
        UnexpandableCommand::Prefixable(PrefixableCommand::DefFont) => eqtb.cur_font(),
        UnexpandableCommand::Prefixable(PrefixableCommand::SetFont(font_index)) => font_index,
        UnexpandableCommand::Prefixable(PrefixableCommand::DefFamily(size)) => {
            let number = Integer::scan_four_bit_int(scanner, eqtb, logger) as usize;
            *eqtb
                .font_params
                .get(FontVariable::MathFont { size, number })
        }
        _ => {
            logger.print_err("Missing font identifier");
            let help = &[
                "I was looking for a control sequence whose",
                "current meaning has been defined by \\font.",
            ];
            scanner.back_error(token, help, eqtb, logger);
            NULL_FONT
        }
    }
}

/// Scans a parameter number and a font number.
/// See 578.
pub fn find_font_dimen(
    writing: bool,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> Option<(FontIndex, usize)> {
    // NOTE: parameters are 1-indexed. This needs to be accounted for.
    let param_number = Integer::scan_int(scanner, eqtb, logger);
    let font_index = scan_font_ident(scanner, eqtb, logger);
    if param_number <= 0 {
        issue_error_message_for_invalid_param_number(font_index, scanner, eqtb, logger);
        None
    } else {
        let font = &mut eqtb.fonts[font_index as usize];
        if writing
            && param_number <= SPACE_SHRINK_CODE as i32
            && param_number >= SPACE_CODE as i32
            && font.glue.is_some()
        {
            font.glue = None;
        }
        // Here we correct the 1-indexing.
        let param_index = (param_number - 1) as usize;
        if param_index >= font.params.len() {
            if (font_index as usize) < eqtb.fonts.len() - 1 {
                issue_error_message_for_invalid_param_number(font_index, scanner, eqtb, logger);
                None
            } else {
                increase_number_of_parameter_in_last_font(param_index, font_index, eqtb);
                Some((font_index, param_index))
            }
        } else {
            Some((font_index, param_index))
        }
    }
}

/// See 579.
fn issue_error_message_for_invalid_param_number(
    font_index: FontIndex,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    logger.print_err("Font ");
    let cs = ControlSequence::FontId(font_index);
    let text = eqtb.control_sequences.text(cs);
    logger.print_esc_str(text);
    logger.print_str(" has only ");
    logger.print_int(eqtb.fonts[font_index as usize].params.len() as i32);
    logger.print_str(" fontdimen parameters");
    let help = &[
        "To increase the number of font parameters, you must",
        "use \\fontdimen immediately after the \\font is loaded.",
    ];
    logger.error(help, scanner, eqtb)
}

/// See 580.
fn increase_number_of_parameter_in_last_font(n: usize, font_index: FontIndex, eqtb: &mut Eqtb) {
    let params = &mut eqtb.fonts[font_index as usize].params;
    for _ in params.len()..(n + 1) {
        params.push(0);
    }
}

/// See 581
pub fn char_warning(font: &FontInfo, c: u8, eqtb: &Eqtb, logger: &mut Logger) {
    if eqtb.integer(IntegerVariable::TracingLostChars) > 0 {
        logger.begin_diagnostic(eqtb.tracing_online());
        logger.print_nl_str("Missing character: There is no ");
        logger.print(c);
        logger.print_str(" in font ");
        logger.slow_print_str(&font.name);
        logger.print_char(b'!');
        logger.end_diagnostic(false);
    }
}

/// See 582.
pub fn new_character(
    font_index: FontIndex,
    c: u8,
    eqtb: &Eqtb,
    logger: &mut Logger,
) -> Option<CharNode> {
    let font = &eqtb.fonts[font_index as usize];
    if font.bc <= c && font.ec >= c && font.char_exists(c) {
        Some(CharNode {
            font_index,
            character: c,
            width: font.width(c),
            height: font.height(c),
            depth: font.depth(c),
            italic: font.italic(c),
        })
    } else {
        char_warning(font, c, eqtb, logger);
        None
    }
}

impl Dumpable for CharInfo {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.width_index.dump(target)?;
        self.height_index.dump(target)?;
        self.depth_index.dump(target)?;
        self.italic_index.dump(target)?;
        self.tag.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let width_index = usize::undump(lines)?;
        let height_index = usize::undump(lines)?;
        let depth_index = usize::undump(lines)?;
        let italic_index = usize::undump(lines)?;
        let tag = CharTag::undump(lines)?;
        Ok(Self {
            width_index,
            height_index,
            depth_index,
            italic_index,
            tag,
        })
    }
}

impl Dumpable for CharTag {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::None => writeln!(target, "None")?,
            Self::Lig(n) => {
                writeln!(target, "Lig")?;
                n.dump(target)?;
            }
            Self::List(n) => {
                writeln!(target, "List")?;
                n.dump(target)?;
            }
            Self::Ext(n) => {
                writeln!(target, "Ext")?;
                n.dump(target)?;
            }
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "None" => Ok(Self::None),
            "Lig" => {
                let n = usize::undump(lines)?;
                Ok(Self::Lig(n))
            }
            "List" => {
                let n = u8::undump(lines)?;
                Ok(Self::List(n))
            }
            "Ext" => {
                let n = usize::undump(lines)?;
                Ok(Self::Ext(n))
            }
            _ => Err(FormatError::ParseError),
        }
    }
}

impl Dumpable for LigKernCommand {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::Kern(w) => {
                writeln!(target, "Kern")?;
                w.dump(target)?;
            }
            Self::Lig {
                ligature,
                delete_left,
                delete_right,
                moves,
            } => {
                writeln!(target, "Lig")?;
                ligature.dump(target)?;
                delete_left.dump(target)?;
                delete_right.dump(target)?;
                moves.dump(target)?;
            }
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "Kern" => {
                let w = Dimension::undump(lines)?;
                Ok(Self::Kern(w))
            }
            "Lig" => {
                let ligature = u8::undump(lines)?;
                let delete_left = bool::undump(lines)?;
                let delete_right = bool::undump(lines)?;
                let moves = u8::undump(lines)?;
                Ok(Self::Lig {
                    ligature,
                    delete_left,
                    delete_right,
                    moves,
                })
            }
            _ => Err(FormatError::ParseError),
        }
    }
}

impl Dumpable for ExtensibleRecipe {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.top.dump(target)?;
        self.mid.dump(target)?;
        self.bot.dump(target)?;
        self.rep.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let top = Option::undump(lines)?;
        let mid = Option::undump(lines)?;
        let bot = Option::undump(lines)?;
        let rep = u8::undump(lines)?;
        Ok(Self { top, mid, bot, rep })
    }
}
