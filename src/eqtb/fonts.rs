use crate::format::{Dumpable, FormatError};

use std::io::Write;

pub const NULL_FONT: FontIndex = 0;

pub type FontIndex = u8;

/// See 230.
pub struct FontParameters {
    cur_font: FontIndex,
    text_fonts: [FontIndex; 16],
    script_fonts: [FontIndex; 16],
    script_script_fonts: [FontIndex; 16],
}

impl FontParameters {
    /// See 232.
    pub fn new() -> Self {
        Self {
            cur_font: NULL_FONT,
            text_fonts: [NULL_FONT; 16],
            script_fonts: [NULL_FONT; 16],
            script_script_fonts: [NULL_FONT; 16],
        }
    }

    fn index(&self, index: FontVariable) -> &FontIndex {
        match index {
            FontVariable::CurFont => &self.cur_font,
            FontVariable::MathFont {
                size: MathFontSize::Text,
                number,
            } => &self.text_fonts[number],
            FontVariable::MathFont {
                size: MathFontSize::Script,
                number,
            } => &self.script_fonts[number],
            FontVariable::MathFont {
                size: MathFontSize::ScriptScript,
                number,
            } => &self.script_script_fonts[number],
        }
    }

    fn index_mut(&mut self, index: FontVariable) -> &mut FontIndex {
        match index {
            FontVariable::CurFont => &mut self.cur_font,
            FontVariable::MathFont {
                size: MathFontSize::Text,
                number,
            } => &mut self.text_fonts[number],
            FontVariable::MathFont {
                size: MathFontSize::Script,
                number,
            } => &mut self.script_fonts[number],
            FontVariable::MathFont {
                size: MathFontSize::ScriptScript,
                number,
            } => &mut self.script_script_fonts[number],
        }
    }

    pub fn get(&self, font_var: FontVariable) -> &FontIndex {
        self.index(font_var)
    }

    pub fn set(&mut self, font_var: FontVariable, new_value: FontIndex) -> FontIndex {
        let prev_value = *self.index_mut(font_var);
        *self.index_mut(font_var) = new_value;
        prev_value
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MathFontSize {
    Text,
    Script,
    ScriptScript,
}

impl MathFontSize {
    pub fn as_str(&self) -> &str {
        match self {
            Self::Text => "textfont",
            Self::Script => "scriptfont",
            Self::ScriptScript => "scriptscriptfont",
        }
    }
}

impl Dumpable for MathFontSize {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::Text => writeln!(target, "Text")?,
            Self::Script => writeln!(target, "Script")?,
            Self::ScriptScript => writeln!(target, "ScriptScript")?,
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "Text" => Ok(Self::Text),
            "Script" => Ok(Self::Script),
            "ScriptScript" => Ok(Self::ScriptScript),
            _ => Err(FormatError::ParseError),
        }
    }
}

/// Specifies a font variable in the Eqtb.
/// See 230.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FontVariable {
    CurFont,
    MathFont { size: MathFontSize, number: usize },
}

impl Dumpable for FontVariable {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::CurFont => writeln!(target, "CurFont")?,
            Self::MathFont { size, number } => {
                writeln!(target, "MathFont")?;
                size.dump(target)?;
                number.dump(target)?;
            }
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "CurFont" => Ok(Self::CurFont),
            "MathFont" => {
                let size = MathFontSize::undump(lines)?;
                let number = usize::undump(lines)?;
                Ok(Self::MathFont { size, number })
            }
            _ => Err(FormatError::ParseError),
        }
    }
}

impl Dumpable for FontParameters {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.cur_font.dump(target)?;
        self.text_fonts.dump(target)?;
        self.script_fonts.dump(target)?;
        self.script_script_fonts.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let cur_font = Dumpable::undump(lines)?;
        let text_fonts = Dumpable::undump(lines)?;
        let script_fonts = Dumpable::undump(lines)?;
        let script_script_fonts = Dumpable::undump(lines)?;

        Ok(Self {
            cur_font,
            text_fonts,
            script_fonts,
            script_script_fonts,
        })
    }
}
