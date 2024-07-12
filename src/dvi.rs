//! Everything needed to create a DVI file.

use crate::scaled::Scaled;

use std::collections::HashMap;
use std::io::Write;

/// Size of the output buffer; must be a multiple of 8
/// Original: 800
/// See 11.
const DVI_BUF_SIZE: usize = if cfg!(feature = "trip") { 800 } else { 16384 };
const _: () = assert!(DVI_BUF_SIZE % 8 == 0, "DVI_BUF_SIZE not multiple of 8");

type DviWriterResult = Result<(), DviWriterError>;

pub struct DviWriter<T: Write> {
    target: T,
    total_pages: usize,
    max_v: Scaled,
    max_h: Scaled,
    max_push: usize,
    push_depth: usize,
    push_position_stack: Vec<usize>,
    last_bop: Option<usize>,

    // Variables needed for the buffer.
    /// The size of the buffer.
    buf_size: usize,
    /// The buffer itself.
    buffer: Vec<u8>,
    /// The lowest index of the second half of the buffer.
    half_buf: usize,
    /// The index just after the highest index of the currently used buffer half.
    dvi_limit: usize,
    /// The buffer index where the next byte should be written to.
    dvi_ptr: usize,
    /// The byte position that the first byte of the buffer will have in the DVI file.
    dvi_offset: usize,
    /// The Number of bytes that have left the buffer.
    dvi_gone: usize,

    // For w, x, y, z optimization
    right_stack: Vec<RightDownEntry>,
    down_stack: Vec<RightDownEntry>,

    mag: Scaled,

    /// We store all defined fonts in this table.
    /// We can reuse this in the postamble.
    fonts: HashMap<u32, DviFontInfo>,
}

#[derive(Clone)]
struct DviFontInfo {
    checksum: u32,
    at_size: Scaled,
    design_size: Scaled,
    area: Vec<u8>,
    name: Vec<u8>,
}

impl<T: Write> DviWriter<T> {
    /// Creates a new DviWriter.
    pub fn new(target: T, mag: Scaled, comment: &[u8]) -> Result<Self, DviWriterError> {
        let mut dvi_writer = Self::new_with_bufsize(target, DVI_BUF_SIZE);
        dvi_writer.write_preamble(mag, comment)?;
        Ok(dvi_writer)
    }

    /// Creates a new DviWriter with a given buffer size.
    /// Used only for testing.
    fn new_with_bufsize(target: T, buf_size: usize) -> Self {
        DviWriter {
            target,
            total_pages: 0,
            max_v: 0,
            max_h: 0,
            max_push: 0,
            push_depth: 0,
            push_position_stack: Vec::new(),
            last_bop: None,
            buf_size,

            // See 596.
            buffer: vec![0; buf_size],
            half_buf: buf_size / 2,
            dvi_limit: buf_size,
            dvi_ptr: 0,
            dvi_offset: 0,
            dvi_gone: 0,

            right_stack: vec![],
            down_stack: vec![],
            mag: 0,
            fonts: HashMap::new(),
        }
    }

    /// Output all buffer bytes with indices from a to b inclusive.
    /// From 597.
    fn write_dvi(&mut self, a: usize, b: usize) -> DviWriterResult {
        self.target
            .write_all(&self.buffer[a..=b])
            .map_err(|_| DviWriterError::IOError)
    }

    /// Write a single byte to the buffer.
    /// From 598.
    fn dvi_out(&mut self, byte: u8) -> DviWriterResult {
        self.buffer[self.dvi_ptr] = byte;
        self.dvi_ptr += 1;
        if self.dvi_ptr == self.dvi_limit {
            self.dvi_swap()?;
        }
        Ok(())
    }

    /// Write to the file that half of the buffer that we are about to start writing into.
    /// From 598.
    fn dvi_swap(&mut self) -> DviWriterResult {
        if self.dvi_limit == self.buf_size {
            self.write_dvi(0, self.half_buf - 1)?;
            self.dvi_limit = self.half_buf;
            self.dvi_offset += self.buf_size;
            self.dvi_ptr = 0;
            self.dvi_gone += self.half_buf;
        } else {
            self.write_dvi(self.half_buf, self.buf_size - 1)?;
            self.dvi_limit = self.buf_size;
            self.dvi_gone += self.buf_size - self.half_buf;
        }
        Ok(())
    }

    /// Write entire buffer to target.
    /// From 599.
    fn flush(&mut self) -> DviWriterResult {
        if self.dvi_limit == self.half_buf {
            self.write_dvi(self.half_buf, self.buf_size - 1)?;
            self.dvi_gone += self.buf_size - self.half_buf;
        }
        if self.dvi_ptr > 0 {
            self.write_dvi(0, self.dvi_ptr - 1)?;
            self.dvi_gone += self.dvi_ptr;
        }
        self.dvi_offset = self.dvi_gone;
        self.dvi_ptr = 0;
        self.dvi_limit = self.buf_size;
        Ok(())
    }

    /// Write the 4-bytes number out in big endian.
    /// From 600.
    fn dvi_four(&mut self, x: i32) -> DviWriterResult {
        for &byte in &x.to_be_bytes() {
            self.dvi_out(byte)?;
        }
        Ok(())
    }

    /// Write a pop operation. `l` should refer to the address after the last push operation.
    /// From 601.
    pub fn dvi_pop(&mut self) -> DviWriterResult {
        let Some(pos) = self.push_position_stack.pop() else {
            return Err(DviWriterError::TooManyPopCalls);
        };
        self.push_depth -= 1;
        self.prune_movements(pos);

        // If we just pushed and the push is still in the buffer, remove it instead of adding a
        // pop.
        if pos == self.loc() && self.dvi_ptr > 0 {
            self.dvi_ptr -= 1;
        } else {
            self.dvi_out(POP)?;
        }
        Ok(())
    }

    /// Write a push operation.
    /// See 619. and 629.
    pub fn dvi_push(&mut self) -> DviWriterResult {
        self.push_depth += 1;
        if self.push_depth > self.max_push {
            self.max_push = self.push_depth;
        }
        self.dvi_out(PUSH)?;
        self.push_position_stack.push(self.loc());
        Ok(())
    }

    /// Write a font definition command.
    /// The area and name may not be more than 255 bytes.
    /// From 602. and 603.
    pub fn dvi_font_def(
        &mut self,
        font_number: u32,
        checksum: u32,
        at_size: Scaled,
        design_size: Scaled,
        area: &[u8],
        name: &[u8],
    ) -> DviWriterResult {
        assert!(area.len() < 256);
        assert!(name.len() < 256);
        if self.fonts.contains_key(&font_number) {
            return Err(DviWriterError::RedefinedFontNumber);
        }
        let font_info = DviFontInfo {
            checksum,
            at_size,
            design_size,
            area: area.to_vec(),
            name: name.to_vec(),
        };

        self.write_font_def(font_number, &font_info)?;
        self.fonts.insert(font_number, font_info);

        Ok(())
    }

    /// Write a font definition.
    fn write_font_def(&mut self, number: u32, font_info: &DviFontInfo) -> DviWriterResult {
        if let Ok(n) = u8::try_from(number) {
            self.dvi_out(FNT_DEF1)?;
            self.dvi_out(n)?;
        } else {
            self.dvi_out(FNT_DEF4)?;
            let bytes = number.to_be_bytes();
            for byte in bytes {
                self.dvi_out(byte)?;
            }
        };
        self.dvi_four(font_info.checksum as i32)?;
        self.dvi_four(font_info.at_size)?;
        self.dvi_four(font_info.design_size)?;
        let Ok(area_len) = u8::try_from(font_info.area.len()) else {
            return Err(DviWriterError::FontAreaTooLong);
        };
        self.dvi_out(area_len)?;
        let Ok(name_len) = u8::try_from(font_info.name.len()) else {
            return Err(DviWriterError::FontNameTooLong);
        };
        self.dvi_out(name_len)?;
        for &byte in &font_info.area {
            self.dvi_out(byte)?;
        }
        for &byte in &font_info.name {
            self.dvi_out(byte)?;
        }
        Ok(())
    }

    /// Move right. This is a version of the original `movement` method.
    pub fn right(&mut self, w: Scaled) -> DviWriterResult {
        self.movement(w, RIGHT1)
    }

    /// Move down. This is a version of the original `movement` method.
    pub fn down(&mut self, w: Scaled) -> DviWriterResult {
        self.movement(w, DOWN1)
    }

    /// Note: Only two values are allowed for `o`: DOWN1 or RIGHT1
    /// To understand this method, we refer to 604. to 614.
    /// From 607.
    fn movement(&mut self, w: Scaled, o: u8) -> DviWriterResult {
        use YZOption::*;
        let stack = if o == DOWN1 {
            &mut self.down_stack
        } else {
            &mut self.right_stack
        };
        #[derive(Clone, Copy)]
        enum Seen {
            None,
            Y,
            Z,
        }
        let mut state = Seen::None;
        for (pos, entry) in stack.iter_mut().enumerate().rev() {
            if entry.width == w {
                // 612.
                match (state, entry.option) {
                    (Seen::None, YZOk) | (Seen::None, YOk) | (Seen::Z, YZOk) | (Seen::Z, YOk) => {
                        if entry.location < self.dvi_gone {
                            return self.generate_down_or_right_command(w, o);
                        } else {
                            // 613.
                            let k = if entry.location < self.dvi_offset {
                                entry.location + self.buf_size - self.dvi_offset
                            } else {
                                entry.location - self.dvi_offset
                            };
                            self.buffer[k] += Y1 - DOWN1;
                            entry.option = YHere;
                            return self.generate_w_x_y_or_z_command(w, o, YHere, pos);
                        }
                    }
                    (Seen::None, ZOk) | (Seen::Y, YZOk) | (Seen::Y, ZOk) => {
                        if entry.location < self.dvi_gone {
                            return self.generate_down_or_right_command(w, o);
                        } else {
                            // 614.
                            let k = if entry.location < self.dvi_offset {
                                entry.location + self.buf_size - self.dvi_offset
                            } else {
                                entry.location - self.dvi_offset
                            };
                            self.buffer[k] += Z1 - DOWN1;
                            entry.option = ZHere;
                            return self.generate_w_x_y_or_z_command(w, o, ZHere, pos);
                        }
                    }
                    (Seen::None, YHere) | (Seen::Z, YHere) => {
                        return self.generate_w_x_y_or_z_command(w, o, YHere, pos)
                    }
                    (Seen::None, ZHere) | (Seen::Y, ZHere) => {
                        return self.generate_w_x_y_or_z_command(w, o, ZHere, pos)
                    }
                    _ => {}
                }
            } else {
                match (state, entry.option) {
                    (Seen::None, YHere) => {
                        state = Seen::Y;
                    }
                    (Seen::None, ZHere) => {
                        state = Seen::Z;
                    }
                    (Seen::Y, ZHere) | (Seen::Z, YHere) => {
                        return self.generate_down_or_right_command(w, o)
                    }
                    _ => {}
                }
            }
        }
        self.generate_down_or_right_command(w, o)?;
        Ok(())
    }

    fn generate_down_or_right_command(&mut self, w: Scaled, o: u8) -> DviWriterResult {
        let entry = RightDownEntry {
            width: w,
            location: self.loc(),
            option: YZOption::YZOk,
        };
        if o == DOWN1 {
            self.down_stack.push(entry);
        } else {
            self.right_stack.push(entry);
        }
        if w.abs() >= 0x800000 {
            // RIGHT4 or DOWN4
            self.dvi_out(o + 3)?;
            self.dvi_four(w)?;
        } else if w.abs() >= 0x8000 {
            // RIGHT3 or DOWN3
            self.dvi_out(o + 2)?;
            let bytes = w.to_be_bytes();
            self.dvi_out(bytes[1])?;
            self.dvi_out(bytes[2])?;
            self.dvi_out(bytes[3])?;
        } else if w.abs() >= 0x80 {
            // RIGHT2 or DOWN2
            self.dvi_out(o + 1)?;
            let bytes = w.to_be_bytes();
            self.dvi_out(bytes[2])?;
            self.dvi_out(bytes[3])?;
        } else {
            // RIGHT1 or DOWN1
            self.dvi_out(o)?;
            let bytes = w.to_be_bytes();
            self.dvi_out(bytes[3])?;
        }
        Ok(())
    }

    fn generate_w_x_y_or_z_command(
        &mut self,
        w: Scaled,
        o: u8,
        option: YZOption,
        p: usize,
    ) -> DviWriterResult {
        if let YZOption::YHere = option {
            // Y0 or W0
            self.dvi_out(o - RIGHT1 + W0)?;
            let stack = if o == DOWN1 {
                &mut self.down_stack
            } else {
                &mut self.right_stack
            };
            for entry in &mut stack[(p + 1)..] {
                match entry.option {
                    YZOption::YZOk => {
                        entry.option = YZOption::ZOk;
                    }
                    YZOption::YOk => {
                        entry.option = YZOption::DFixed;
                    }
                    _ => {}
                }
            }
        } else {
            // Z0 or X0
            self.dvi_out(o - RIGHT1 + X0)?;
            let stack = if o == DOWN1 {
                &mut self.down_stack
            } else {
                &mut self.right_stack
            };
            for entry in &mut stack[(p + 1)..] {
                match entry.option {
                    YZOption::YZOk => {
                        entry.option = YZOption::YOk;
                    }
                    YZOption::ZOk => {
                        entry.option = YZOption::DFixed;
                    }
                    _ => {}
                }
            }
        }
        let entry = RightDownEntry {
            width: w,
            location: self.loc(),
            option,
        };
        let stack = if o == DOWN1 {
            &mut self.down_stack
        } else {
            &mut self.right_stack
        };
        stack.push(entry);
        Ok(())
    }

    /// Remove all RightDownEntries from the stacks with location larger or equal to `pos`.
    /// From 615.
    fn prune_movements(&mut self, pos: usize) {
        for stack in &mut [&mut self.down_stack, &mut self.right_stack] {
            let mut i = stack.len();
            while i != 0 {
                if stack[i - 1].location < pos {
                    break;
                }
                i -= 1;
            }
            stack.truncate(i);
        }
    }

    /// From 617.
    fn write_preamble(&mut self, mag: Scaled, comment: &[u8]) -> DviWriterResult {
        if comment.len() > u8::MAX as usize {
            return Err(DviWriterError::CommentTooLong);
        }
        self.mag = mag;
        self.dvi_out(PRE)?;
        self.dvi_out(ID_BYTE)?;
        self.dvi_four(25400000)?;
        self.dvi_four(473628672)?;
        self.dvi_four(mag)?;
        self.dvi_out(comment.len() as u8)?;
        for &byte in comment {
            self.dvi_out(byte)?;
        }
        Ok(())
    }

    /// Finish the DVI file and return the size of the file.
    /// From 642.
    pub fn write_postamble(mut self) -> Result<usize, DviWriterError> {
        let post_location = self.loc();
        self.dvi_out(POST)?;
        let last_bop = match self.last_bop {
            None => -1,
            Some(b) => b as i32,
        };
        self.dvi_four(last_bop)?;
        self.dvi_four(25400000)?;
        self.dvi_four(473628672)?;
        self.dvi_four(self.mag)?;
        self.dvi_four(self.max_v)?;
        self.dvi_four(self.max_h)?;
        self.dvi_out((self.max_push / 256) as u8)?;
        self.dvi_out((self.max_push % 256) as u8)?;
        self.dvi_out((self.total_pages / 256) as u8)?;
        self.dvi_out((self.total_pages % 256) as u8)?;
        // Create a list of the used fonts in decreasing order by font number.
        let mut fonts: Vec<(u32, DviFontInfo)> = self
            .fonts
            .iter()
            .map(|(n, info)| (*n, info.clone()))
            .collect();
        fonts.sort_by(|a, b| b.0.cmp(&a.0));
        for (number, font_info) in fonts {
            self.write_font_def(number, &font_info)?;
        }
        self.dvi_out(POST_POST)?;
        self.dvi_four(post_location as i32)?;
        self.dvi_out(ID_BYTE)?;
        let k = 4 + ((self.buf_size - self.dvi_ptr) % 4);
        for _ in 0..k {
            self.dvi_out(223)?;
        }
        self.flush()?;

        Ok(self.loc())
    }

    /// The location just after the last byte that has been written.
    /// In other words, the number of bytes written so far.
    fn loc(&self) -> usize {
        self.dvi_offset + self.dvi_ptr
    }

    /// Sets a char.
    /// We are using the fact that for a char c < 128, c == SET_CHAR_c.
    /// Taken from 620.
    pub fn set_char(&mut self, c: u8) -> DviWriterResult {
        if c >= 128 {
            self.dvi_out(SET1)?;
        }
        self.dvi_out(c)
    }

    /// Sets a font.
    /// Taken from 621.
    pub fn set_font(&mut self, font_number: u32) -> DviWriterResult {
        if let Ok(f) = u8::try_from(font_number) {
            if f < 64 {
                self.dvi_out(FNT_NUM_0 + f)
            } else {
                self.dvi_out(FNT1)?;
                self.dvi_out(f)
            }
        } else {
            self.dvi_out(FNT4)?;
            let bytes = font_number.to_be_bytes();
            for byte in bytes {
                self.dvi_out(byte)?;
            }
            Ok(())
        }
    }

    /// Sets a rule and moves right.
    /// Taken from 624.
    pub fn set_rule(&mut self, height: Scaled, width: Scaled) -> DviWriterResult {
        self.dvi_out(SET_RULE)?;
        self.dvi_four(height)?;
        self.dvi_four(width)
    }

    /// Puts a rule and does not move right.
    /// Taken from 633.
    pub fn put_rule(&mut self, height: Scaled, width: Scaled) -> DviWriterResult {
        self.dvi_out(PUT_RULE)?;
        self.dvi_four(height)?;
        self.dvi_four(width)
    }

    /// Start a new page.
    /// Taken from 640. and 641.
    pub fn start_page(
        &mut self,
        counts: &[i32; 10],
        page_height: Scaled,
        page_width: Scaled,
    ) -> DviWriterResult {
        let this_bop = self.loc();
        self.dvi_out(BOP)?;
        for &count in counts {
            self.dvi_four(count)?;
        }
        let last_bop = match self.last_bop {
            None => -1,
            Some(b) => b as i32,
        };
        self.dvi_four(last_bop)?;
        self.last_bop = Some(this_bop);

        if page_height > self.max_v {
            self.max_v = page_height;
        }
        if page_width > self.max_h {
            self.max_h = page_width;
        }

        Ok(())
    }

    /// End page.
    /// Taken from 640.
    pub fn end_page(&mut self) -> DviWriterResult {
        assert!(self.push_depth == 0);

        self.right_stack.clear();
        self.down_stack.clear();
        self.dvi_out(EOP)?;
        self.total_pages += 1;

        Ok(())
    }

    /// Get number of finished pages.
    pub fn get_total_pages(&self) -> usize {
        self.total_pages
    }

    /// Write special byte string.
    pub fn write_special(&mut self, bytes: &[u8]) -> DviWriterResult {
        if bytes.len() < 256 {
            self.dvi_out(XXX1)?;
            self.dvi_out(bytes.len() as u8)?;
        } else {
            self.dvi_out(XXX4)?;
            self.dvi_four(bytes.len() as i32)?;
        }
        for &byte in bytes {
            self.dvi_out(byte)?;
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum YZOption {
    YHere,
    ZHere,
    YZOk,
    YOk,
    ZOk,
    DFixed,
}

#[derive(Clone, Copy, Debug)]
struct RightDownEntry {
    option: YZOption,
    width: Scaled,
    location: usize,
}

// From 587.
const ID_BYTE: u8 = 2;

// From 586.
const SET1: u8 = 128;
const SET_RULE: u8 = 132;
const PUT_RULE: u8 = 137;
const BOP: u8 = 139;
const EOP: u8 = 140;
const PUSH: u8 = 141;
const POP: u8 = 142;
const RIGHT1: u8 = 143;
const W0: u8 = 147;
const X0: u8 = 152;
const DOWN1: u8 = 157;
const Y1: u8 = 162;
const Z1: u8 = 167;
const FNT_NUM_0: u8 = 171;
const FNT1: u8 = 235;
const FNT4: u8 = 238;
const XXX1: u8 = 239;
const XXX4: u8 = 242;
const FNT_DEF1: u8 = 243;
const FNT_DEF4: u8 = 246;
const PRE: u8 = 247;
const POST: u8 = 248;
const POST_POST: u8 = 249;

#[cfg(test)]
mod test {
    use super::DviWriter;
    use super::YZOption;
    use super::DOWN1;

    #[test]
    fn write_big_endian_number() {
        let mut target = vec![];
        let mut dw = DviWriter::new_with_bufsize(&mut target, 8);
        dw.dvi_four(0x01020304).unwrap();
        dw.flush().unwrap();
        assert_eq!(target, [1, 2, 3, 4]);
    }

    #[test]
    fn write_100_ones() {
        let mut target = vec![];
        let mut dw = DviWriter::new_with_bufsize(&mut target, 8);
        for _ in 0..100 {
            dw.dvi_out(1).unwrap();
        }
        dw.flush().unwrap();
        assert_eq!(target, [1; 100]);
    }

    #[test]
    /// See 604. for an explanation.
    fn create_sequence_of_down_movements() {
        use YZOption::*;
        let mut target = vec![];
        let mut dw = DviWriter::new_with_bufsize(&mut target, 32);
        let values = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9];
        for &value in &values {
            dw.movement(value, DOWN1).unwrap();
        }
        let expected = [
            ZHere, YHere, DFixed, YHere, YHere, DFixed, DFixed, DFixed, YHere, ZHere, YHere, YZOk,
            YZOk,
        ];
        let found: Vec<YZOption> = dw.down_stack.iter().map(|x| x.option).collect();
        assert_eq!(found, expected);
    }
}

#[derive(Debug)]
pub enum DviWriterError {
    CommentTooLong,
    IOError,
    TooManyPopCalls,
    RedefinedFontNumber,
    FontNameTooLong,
    FontAreaTooLong,
}
