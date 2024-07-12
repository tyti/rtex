mod dump_command;
mod dump_noads;
mod dump_nodes;

use crate::dimension::Dimension;
use crate::eqtb::{ControlSequence, Eqtb, FontIndex, IntegerVariable};
use crate::error::dump_in_group_error;
use crate::hyphenation::Hyphenator;
use crate::input::InputStack;
use crate::logger::{InteractionMode, Logger};
use crate::nodes::GlueRatio;
use crate::print::Printer;
use crate::{open_in, open_out};

use std::collections::HashMap;
use std::ffi::OsString;
use std::fs::File;
use std::hash::Hash;
use std::io::{BufWriter, Read, Write};
use std::path::PathBuf;
use std::str::FromStr;

pub const FORMAT_EXTENSION: &str = "fmt";
const FORMAT_DEFAULT_AREA: &str = "TeXformats";
const FORMAT_DEFAULT_PLAIN: &str = "plain";

/// See 1332.
pub fn load_hyphenator_and_eqtb_from_specified_format_file(
    format_name: OsString,
) -> Result<(Logger, Hyphenator, Box<Eqtb>), ()> {
    let Some(fmt_file) = open_fmt_file(Some(format_name)) else {
        return Err(());
    };
    let Ok(undumped_objects) = load_fmt_file(fmt_file) else {
        return Err(());
    };
    Ok(undumped_objects)
}

/// See 1332.
pub fn load_hyphenator_and_eqtb_from_default_format_file(
) -> Result<(Logger, Hyphenator, Box<Eqtb>), ()> {
    // Now attempt to load the format file or abort.
    let Some(fmt_file) = open_fmt_file(None) else {
        return Err(());
    };
    let Ok(undumped_objects) = load_fmt_file(fmt_file) else {
        return Err(());
    };
    Ok(undumped_objects)
}

/// Attempts to open a format file.
///
/// If the first character of the line is a `&` character, all non-space
/// characters up to the next space character or the end of the line are
/// considered the file name for the format file.
/// In case that the given format file cannot be found or in case that the
/// first character is not a `&`, the PLAIN format file will be opened.
/// If that fails as well, None is returned.
/// See 524.
fn open_fmt_file(format_name: Option<OsString>) -> Option<File> {
    if let Some(file_name) = format_name {
        // Try to find format file from current working directory.
        let mut path = PathBuf::from(&file_name);
        path.set_extension(FORMAT_EXTENSION);
        if let Ok(file) = open_in(&path) {
            return Some(file);
        }
        // Try to find format file in the default format directory.
        let mut path: PathBuf = PathBuf::from(FORMAT_DEFAULT_AREA);
        path.push(file_name);
        path.set_extension(FORMAT_EXTENSION);
        if let Ok(file) = open_in(&path) {
            return Some(file);
        }
        println!("Sorry, I can't find that format; will try PLAIN.");
    }
    // Look for default PLAIN format file
    let mut path: PathBuf = [FORMAT_DEFAULT_AREA, FORMAT_DEFAULT_PLAIN].iter().collect();
    path.set_extension(FORMAT_EXTENSION);
    match open_in(&path) {
        Ok(file) => Some(file),
        Err(_) => {
            println!("I can't find the PLAIN format file!");
            None
        }
    }
}

/// NOTE Does currently not check that all undumped value are in a valid range.
/// See 1303.
fn load_fmt_file(mut fmt_file: File) -> Result<(Logger, Hyphenator, Box<Eqtb>), FormatError> {
    let mut format_string = String::new();
    if fmt_file.read_to_string(&mut format_string).is_err() {
        println!("Format file is not valid UTF-8");
        return Err(FormatError::NoUtf8);
    }
    let mut lines = CountedLines::from(format_string.lines());
    let eqtb = undump_table_of_equivalents(&mut lines)?;
    let hyphenator = undump_hyphenation_tables(&mut lines)?;
    let logger = undump_a_couple_more(&mut lines, &eqtb)?;
    Ok((logger, hyphenator, eqtb))
}

/// See 1314.
fn undump_table_of_equivalents(lines: &mut CountedLines) -> Result<Box<Eqtb>, FormatError> {
    match Box::<Eqtb>::undump(lines) {
        Ok(eqtb) => Ok(eqtb),
        Err(format_error) => {
            println!(
                "Format error on line {} while parsing Eqtb",
                lines.line_number()
            );
            Err(format_error)
        }
    }
}

/// See 1325.
fn undump_hyphenation_tables<'a>(
    lines: &mut impl Iterator<Item = &'a str>,
) -> Result<Hyphenator, FormatError> {
    match Hyphenator::undump(lines) {
        Ok(hyphenator) => Ok(hyphenator),
        Err(format_error) => {
            println!("Format error while parsing hyphenation tables");
            Err(format_error)
        }
    }
}

/// See 1327.
fn undump_a_couple_more<'a>(
    lines: &mut impl Iterator<Item = &'a str>,
    eqtb: &Eqtb,
) -> Result<Logger, FormatError> {
    let interaction = match InteractionMode::undump(lines) {
        Ok(interaction) => interaction,
        Err(_) => {
            println!("Format error while parsing interaction");
            return Err(FormatError::ParseError);
        }
    };
    let format_ident = match String::undump(lines) {
        Ok(format_ident) => format_ident,
        Err(_) => {
            println!("Format error while parsing format_ident");
            return Err(FormatError::ParseError);
        }
    };

    let mut logger = Logger::new(format_ident, interaction);

    // We update the Logger's copies of escapechar and newlinechar here.
    logger.escape_char = eqtb.get_current_escape_character();
    logger.newline_char = eqtb.get_current_newline_character();

    match i32::undump(lines) {
        Ok(69069) => Ok(logger),
        _ => {
            println!("Format error: final constant incorrect");
            Err(FormatError::WrongConstant)
        }
    }
}

/// See 1302. and 1304.
pub fn store_fmt_file(
    hyphenator: &Hyphenator,
    input_stack: &InputStack,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    if !eqtb.save_stack.is_empty() {
        dump_in_group_error(input_stack, eqtb, logger);
    }
    let format_file = create_format_ident_and_open_file(input_stack, eqtb, logger);
    let mut format_file = BufWriter::new(format_file);
    if let Some(log_file) = &mut logger.log_file {
        log_file.flush().expect("Error writing to log file");
    }

    dump_table_of_equivalents(&mut format_file, eqtb, logger).unwrap();
    dump_font_information(eqtb, logger).unwrap();
    dump_hyphenation_tables(&mut format_file, hyphenator, logger).unwrap();
    dump_a_couple_more(&mut format_file, logger).unwrap();

    eqtb.integers.set(IntegerVariable::TracingStats, 0);
}

/// See 1328.
fn create_format_ident_and_open_file(
    input_stack: &InputStack,
    eqtb: &Eqtb,
    logger: &mut Logger,
) -> File {
    let mut format_ident = " (preloaded format=".to_string();
    format_ident.push_str(&logger.job_name.as_ref().unwrap().to_string_lossy());
    let date = format!(
        " {}.{}.{})",
        eqtb.integer(IntegerVariable::Year),
        eqtb.integer(IntegerVariable::Month),
        eqtb.integer(IntegerVariable::Day),
    );
    format_ident.push_str(&date);
    logger.terminal_logging = logger.interaction != InteractionMode::Batch;
    let job_name = logger.job_name.as_ref().unwrap();
    let mut path = PathBuf::from(job_name);
    path.set_extension(FORMAT_EXTENSION);

    let file = loop {
        match open_out(&path) {
            Ok(file) => break file,
            Err(_) => {
                path = logger.prompt_format_file_name(&path, input_stack, eqtb);
            }
        }
    };
    logger.print_nl_str("Beginning to dump on file ");
    logger.slow_print_str(path.to_str().unwrap().as_bytes());
    logger.print_nl_str("");
    logger.slow_print_str(format_ident.as_bytes());
    logger.format_ident = format_ident;
    file
}

/// See 1313.
fn dump_table_of_equivalents(
    format_file: &mut BufWriter<File>,
    eqtb: &Eqtb,
    logger: &mut Logger,
) -> Result<(), std::io::Error> {
    logger.print_ln();
    logger.print_int(eqtb.control_sequences.cs_count as i32);
    logger.print_str(" multiletter control sequences");
    eqtb.dump(format_file)
}

/// See 1320.
fn dump_font_information(eqtb: &Eqtb, logger: &mut Logger) -> Result<(), std::io::Error> {
    for (font_index, font) in eqtb.fonts.iter().enumerate() {
        logger.print_nl_str("\\font");
        logger.print_esc_str(
            eqtb.control_sequences
                .text(ControlSequence::FontId(font_index as FontIndex)),
        );
        logger.print_char(b'=');
        let mut file_name = font.area.clone();
        file_name.append(&mut font.name.clone());
        let path = PathBuf::from(std::str::from_utf8(&file_name).unwrap());
        logger.print_file_name(&path);
        if font.size != font.dsize {
            logger.print_str(" at ");
            logger.print_scaled(font.size);
            logger.print_str("pt");
        }
    }
    // Note that fonts get dumped as part of Eqtb.
    Ok(())
}

/// See 1324.
fn dump_hyphenation_tables(
    format_file: &mut BufWriter<File>,
    hyphenator: &Hyphenator,
    logger: &mut Logger,
) -> Result<(), std::io::Error> {
    let mut total_exceptions = 0;
    for exceptions in &hyphenator.exceptions {
        total_exceptions += exceptions.len();
    }
    logger.print_ln();
    logger.print_int(total_exceptions as i32);
    logger.print_str(" hyphenation exception");
    if total_exceptions != 1 {
        logger.print_char(b's');
    }

    hyphenator.dump(format_file)
}

/// See 1326.
fn dump_a_couple_more(
    format_file: &mut BufWriter<File>,
    logger: &Logger,
) -> Result<(), std::io::Error> {
    logger.interaction.dump(format_file)?;
    logger.format_ident.dump(format_file)?;
    69069.dump(format_file)?;
    Ok(())
}

/// An error while parsing a format file.
#[derive(Debug)]
pub enum FormatError {
    NoUtf8,
    IncompleteFile,
    ParseError,
    WrongConstant,
}

/// A wrapper around `Lines` that counts how many lines have been consumed.
pub struct CountedLines<'a> {
    lines: std::str::Lines<'a>,
    count: usize,
}

impl<'a> CountedLines<'a> {
    pub fn from(lines: std::str::Lines<'a>) -> Self {
        Self { lines, count: 0 }
    }

    pub fn line_number(&self) -> usize {
        self.count
    }
}

impl<'a> Iterator for CountedLines<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        self.count += 1;
        self.lines.next()
    }
}

fn parse_next<'a, T: FromStr>(lines: &mut impl Iterator<Item = &'a str>) -> Result<T, FormatError> {
    lines
        .next()
        .ok_or(FormatError::IncompleteFile)?
        .parse()
        .map_err(|_| FormatError::ParseError)
}

pub trait Dumpable {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error>;
    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError>
    where
        Self: Sized;
}

impl Dumpable for u8 {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        writeln!(target, "{}", self)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        parse_next(lines)
    }
}

impl Dumpable for u16 {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        writeln!(target, "{}", self)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        parse_next(lines)
    }
}

impl Dumpable for u32 {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        writeln!(target, "{}", self)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        parse_next(lines)
    }
}

impl Dumpable for usize {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        writeln!(target, "{}", self)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        parse_next(lines)
    }
}

impl Dumpable for bool {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        writeln!(target, "{}", self)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        parse_next(lines)
    }
}

impl Dumpable for String {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        writeln!(target, "{}", self)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        parse_next(lines)
    }
}

impl<T: Dumpable + Default, const N: usize> Dumpable for [T; N] {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        for t in self {
            t.dump(target)?;
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let mut a = std::array::from_fn(|_| T::default());
        for i in 0..N {
            a[i] = T::undump(lines)?;
        }
        Ok(a)
    }
}

impl<T, U> Dumpable for (T, U)
where
    T: Dumpable,
    U: Dumpable,
{
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.0.dump(target)?;
        self.1.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let t0 = T::undump(lines)?;
        let t1 = U::undump(lines)?;
        Ok((t0, t1))
    }
}

impl<T, U, V> Dumpable for (T, U, V)
where
    T: Dumpable,
    U: Dumpable,
    V: Dumpable,
{
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.0.dump(target)?;
        self.1.dump(target)?;
        self.2.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let t0 = T::undump(lines)?;
        let t1 = U::undump(lines)?;
        let t2 = V::undump(lines)?;
        Ok((t0, t1, t2))
    }
}

impl<T: Dumpable> Dumpable for Option<T> {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            None => writeln!(target, "None")?,
            Some(x) => {
                writeln!(target, "Some")?;
                x.dump(target)?;
            }
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "None" => Ok(None),
            "Some" => {
                let x = T::undump(lines)?;
                Ok(Some(x))
            }
            _ => Err(FormatError::ParseError),
        }
    }
}

impl<T: Dumpable> Dumpable for Box<T> {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        (**self).dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let x = T::undump(lines)?;
        Ok(Box::new(x))
    }
}

impl<T: Dumpable> Dumpable for Vec<T> {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        writeln!(target, "{}", self.len())?;
        for x in self {
            x.dump(target)?;
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let n = parse_next(lines)?;
        let mut vec = Vec::new();
        for _ in 0..n {
            let x = T::undump(lines)?;
            vec.push(x);
        }
        Ok(vec)
    }
}

impl<T: Dumpable + Eq + Hash, U: Dumpable> Dumpable for HashMap<T, U> {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        writeln!(target, "{}", self.len())?;
        for (key, val) in self {
            key.dump(target)?;
            val.dump(target)?;
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let n = parse_next(lines)?;
        let mut map = HashMap::new();
        for _ in 0..n {
            let key = T::undump(lines)?;
            let val = U::undump(lines)?;
            map.insert(key, val);
        }
        Ok(map)
    }
}

impl<T: Dumpable> Dumpable for std::rc::Rc<T> {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        (**self).dump(target)
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let t = T::undump(lines)?;
        Ok(std::rc::Rc::new(t))
    }
}

impl Dumpable for Dimension {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        writeln!(target, "{}", self)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        parse_next(lines)
    }
}

impl Dumpable for GlueRatio {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        writeln!(target, "{}", self)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        parse_next(lines)
    }
}
