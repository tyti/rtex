use super::{print_indentation, show_list_node, show_node_list, ListNode, Node};
use crate::command::LimitType;
use crate::dimension::Dimension;
use crate::eqtb::Eqtb;
use crate::format::{Dumpable, FormatError};
use crate::logger::Logger;
use crate::math::MathStyle;
use crate::print::Printer;

use std::io::Write;

pub const DEFAULT_CODE: i32 = 0o10000000000;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Noad {
    Normal(NormalNoad),
    Radical(RadicalNoad),
    Fraction(FractionNoad),
    Under(UnderNoad),
    Over(OverNoad),
    Accent(AccentNoad),
    Vcenter(VcenterNoad),
    Left(LeftNoad),
    Right(RightNoad),
}

// NOTE This is only needed so that we can implement Clone for Node but it should be
// impossible that cloning is every invoked on this type of Node.
impl Clone for Noad {
    fn clone(&self) -> Self {
        panic!("It should not happen that these get copied in copy_node_list")
    }
}

impl Noad {
    pub fn display(
        &self,
        indent_str: &[u8],
        depth_max: i32,
        breadth_max: usize,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        match self {
            Self::Normal(normal_noad) => {
                normal_noad.display(indent_str, depth_max, breadth_max, eqtb, logger)
            }
            Self::Radical(radical_noad) => {
                radical_noad.display(indent_str, depth_max, breadth_max, eqtb, logger)
            }
            Self::Fraction(fraction_noad) => {
                fraction_noad.display(indent_str, depth_max, breadth_max, eqtb, logger)
            }
            Self::Under(under_noad) => {
                under_noad.display(indent_str, depth_max, breadth_max, eqtb, logger)
            }
            Self::Over(over_noad) => {
                over_noad.display(indent_str, depth_max, breadth_max, eqtb, logger)
            }
            Self::Accent(accent_noad) => {
                accent_noad.display(indent_str, depth_max, breadth_max, eqtb, logger)
            }
            Self::Vcenter(vcenter_noad) => {
                vcenter_noad.display(indent_str, depth_max, breadth_max, eqtb, logger)
            }
            Self::Left(left_noad) => left_noad.display(logger),
            Self::Right(right_noad) => right_noad.display(logger),
        }
    }
}

/// See 681.
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum NoadField {
    MathChar { fam: u8, character: u8 },
    MathTextChar { fam: u8, character: u8 },
    SubBox { list_node: ListNode },
    SubMlist { list: Vec<Node> },
}

impl NoadField {
    /// See 691. and 692.
    fn display(
        &self,
        c: u8,
        indent_str: &[u8],
        depth_max: i32,
        breadth_max: usize,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        if indent_str.len() as i32 >= depth_max {
            logger.print_str(" []");
        } else {
            let mut indent_str = indent_str.to_vec();
            indent_str.push(c);
            match self {
                &Self::MathChar { fam, character } => {
                    logger.print_ln();
                    print_indentation(&indent_str, logger);
                    print_fam_and_char(fam, character, logger);
                }
                Self::SubBox { list_node } => {
                    show_list_node(list_node, &indent_str, depth_max, breadth_max, eqtb, logger)
                }
                Self::SubMlist { list } => {
                    if list.is_empty() {
                        logger.print_ln();
                        print_indentation(&indent_str, logger);
                        logger.print_str("{}");
                    } else {
                        show_info(list, &indent_str, depth_max, breadth_max, eqtb, logger);
                    }
                }
                _ => {}
            }
        }
    }
}

/// See 683.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DelimiterField {
    pub small_fam: u8,
    pub small_char: u8,
    pub large_fam: u8,
    pub large_char: u8,
}

impl DelimiterField {
    pub fn null_delimiter() -> Self {
        Self {
            small_fam: 0,
            small_char: 0,
            large_fam: 0,
            large_char: 0,
        }
    }

    /// See 691.
    fn display(&self, logger: &mut Logger) {
        let mut a = self.small_fam as i32 * 256 + self.small_char as i32;
        a *= 0x1000;
        a += self.large_fam as i32 * 256 + self.large_char as i32;
        if a < 0 {
            logger.print_int(a);
        } else {
            logger.print_hex(a);
        }
    }
}

/// See 682.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NoadType {
    Ord,
    Op { limit_type: LimitType },
    Bin,
    Rel,
    Open,
    Close,
    Punct,
    Inner,
}

impl From<u8> for NoadType {
    fn from(n: u8) -> Self {
        match n {
            0 => Self::Ord,
            1 => Self::Op {
                limit_type: LimitType::DisplayLimits,
            },
            2 => Self::Bin,
            3 => Self::Rel,
            4 => Self::Open,
            5 => Self::Close,
            6 => Self::Punct,
            7 => Self::Inner,
            _ => panic!("Should not happen"),
        }
    }
}

impl Dumpable for NoadType {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            NoadType::Ord => writeln!(target, "Ord")?,
            NoadType::Op { limit_type } => {
                writeln!(target, "Op")?;
                limit_type.dump(target)?;
            }
            NoadType::Bin => writeln!(target, "Bin")?,
            NoadType::Rel => writeln!(target, "Rel")?,
            NoadType::Open => writeln!(target, "Open")?,
            NoadType::Close => writeln!(target, "Close")?,
            NoadType::Punct => writeln!(target, "Punct")?,
            NoadType::Inner => writeln!(target, "Inner")?,
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "Ord" => Ok(NoadType::Ord),
            "Op" => {
                let limit_type = LimitType::undump(lines)?;
                Ok(NoadType::Op { limit_type })
            }
            "Bin" => Ok(NoadType::Bin),
            "Rel" => Ok(NoadType::Rel),
            "Open" => Ok(NoadType::Open),
            "Close" => Ok(NoadType::Close),
            "Punct" => Ok(NoadType::Punct),
            "Inner" => Ok(NoadType::Inner),
            _ => Err(FormatError::ParseError),
        }
    }
}

/// See 681., 682. and 683.
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct NormalNoad {
    pub noad_type: NoadType,
    pub nucleus: Option<Box<NoadField>>,
    pub subscript: Option<Box<NoadField>>,
    pub superscript: Option<Box<NoadField>>,
}

impl NormalNoad {
    /// See 686.
    pub fn new() -> Self {
        Self {
            noad_type: NoadType::Ord,
            nucleus: None,
            subscript: None,
            superscript: None,
        }
    }

    /// See 696.
    fn display(
        &self,
        indent_str: &[u8],
        depth_max: i32,
        breadth_max: usize,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        let s: &[u8] = match self.noad_type {
            NoadType::Ord => b"mathord",
            NoadType::Op { .. } => b"mathop",
            NoadType::Bin => b"mathbin",
            NoadType::Rel => b"mathrel",
            NoadType::Open => b"mathopen",
            NoadType::Close => b"mathclose",
            NoadType::Punct => b"mathpunct",
            NoadType::Inner => b"mathinner",
        };
        logger.print_esc_str(s);
        if let NoadType::Op { limit_type } = self.noad_type {
            match limit_type {
                LimitType::DisplayLimits => {}
                LimitType::Limits | LimitType::NoLimits => limit_type.display(logger),
            }
        }
        if let Some(nucleus) = &self.nucleus {
            nucleus.display(b'.', indent_str, depth_max, breadth_max, eqtb, logger);
        }
        if let Some(superscript) = &self.superscript {
            superscript.display(b'^', indent_str, depth_max, breadth_max, eqtb, logger);
        }
        if let Some(subscript) = &self.subscript {
            subscript.display(b'_', indent_str, depth_max, breadth_max, eqtb, logger);
        }
    }
}

/// See 683.
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct RadicalNoad {
    pub left_delimiter: DelimiterField,
    pub nucleus: Option<Box<NoadField>>,
    pub subscript: Option<Box<NoadField>>,
    pub superscript: Option<Box<NoadField>>,
}

impl RadicalNoad {
    /// See 696.
    fn display(
        &self,
        indent_str: &[u8],
        depth_max: i32,
        breadth_max: usize,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        logger.print_esc_str(b"radical");
        self.left_delimiter.display(logger);
        if let Some(nucleus) = &self.nucleus {
            nucleus.display(b'.', indent_str, depth_max, breadth_max, eqtb, logger);
        }
        if let Some(superscript) = &self.superscript {
            superscript.display(b'^', indent_str, depth_max, breadth_max, eqtb, logger);
        }
        if let Some(subscript) = &self.subscript {
            subscript.display(b'_', indent_str, depth_max, breadth_max, eqtb, logger);
        }
    }
}

/// See 683.
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct FractionNoad {
    pub left_delimiter: DelimiterField,
    pub right_delimiter: DelimiterField,
    pub thickness: Dimension,
    pub numerator: Vec<Node>,
    pub denominator: Vec<Node>,
}

impl FractionNoad {
    /// See 692.
    fn print_mlist(
        list: &[Node],
        c: u8,
        indent_str: &[u8],
        depth_max: i32,
        breadth_max: usize,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        let mut indent_str = indent_str.to_vec();
        indent_str.push(c);
        if list.is_empty() {
            logger.print_ln();
            print_indentation(&indent_str, logger);
            logger.print_str("{}");
        } else {
            show_info(list, &indent_str, depth_max, breadth_max, eqtb, logger);
        }
    }

    /// This is a modified version of the normal display where we never print denominator lists.
    /// This is to avoid having to allow the denominator to be a NoadField::Empty just to print
    /// correctly as an incompleat_noad.
    /// See 697.
    pub fn display_as_compleat_noad(
        &self,
        depth_max: i32,
        breadth_max: usize,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        logger.print_esc_str(b"fraction, thickness ");
        if self.thickness == DEFAULT_CODE {
            logger.print_str("= default");
        } else {
            logger.print_scaled(self.thickness);
        }
        if self.left_delimiter.small_fam != 0
            || self.left_delimiter.small_char != 0
            || self.left_delimiter.large_fam != 0
            || self.left_delimiter.large_char != 0
        {
            logger.print_str(", left-delimiter ");
            self.left_delimiter.display(logger);
        }
        if self.right_delimiter.small_fam != 0
            || self.right_delimiter.small_char != 0
            || self.right_delimiter.large_fam != 0
            || self.right_delimiter.large_char != 0
        {
            logger.print_str(", right-delimiter ");
            self.right_delimiter.display(logger);
        }
        Self::print_mlist(
            &self.numerator,
            b'\\',
            b"",
            depth_max,
            breadth_max,
            eqtb,
            logger,
        );
    }

    /// See 697.
    fn display(
        &self,
        indent_str: &[u8],
        depth_max: i32,
        breadth_max: usize,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        logger.print_esc_str(b"fraction, thickness ");
        if self.thickness == DEFAULT_CODE {
            logger.print_str("= default");
        } else {
            logger.print_scaled(self.thickness);
        }
        if self.left_delimiter.small_fam != 0
            || self.left_delimiter.small_char != 0
            || self.left_delimiter.large_fam != 0
            || self.left_delimiter.large_char != 0
        {
            logger.print_str(", left-delimiter ");
            self.left_delimiter.display(logger);
        }
        if self.right_delimiter.small_fam != 0
            || self.right_delimiter.small_char != 0
            || self.right_delimiter.large_fam != 0
            || self.right_delimiter.large_char != 0
        {
            logger.print_str(", right-delimiter ");
            self.right_delimiter.display(logger);
        }
        Self::print_mlist(
            &self.numerator,
            b'\\',
            indent_str,
            depth_max,
            breadth_max,
            eqtb,
            logger,
        );
        Self::print_mlist(
            &self.denominator,
            b'/',
            indent_str,
            depth_max,
            breadth_max,
            eqtb,
            logger,
        );
    }
}

/// See 687.
#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct UnderNoad {
    pub nucleus: Option<Box<NoadField>>,
    pub subscript: Option<Box<NoadField>>,
    pub superscript: Option<Box<NoadField>>,
}

impl UnderNoad {
    /// See 696.
    fn display(
        &self,
        indent_str: &[u8],
        depth_max: i32,
        breadth_max: usize,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        logger.print_esc_str(b"underline");
        if let Some(nucleus) = &self.nucleus {
            nucleus.display(b'.', indent_str, depth_max, breadth_max, eqtb, logger);
        }
        if let Some(superscript) = &self.superscript {
            superscript.display(b'^', indent_str, depth_max, breadth_max, eqtb, logger);
        }
        if let Some(subscript) = &self.subscript {
            subscript.display(b'_', indent_str, depth_max, breadth_max, eqtb, logger);
        }
    }
}

/// See 687.
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct OverNoad {
    pub nucleus: Option<Box<NoadField>>,
    pub subscript: Option<Box<NoadField>>,
    pub superscript: Option<Box<NoadField>>,
}

impl OverNoad {
    /// See 696.
    fn display(
        &self,
        indent_str: &[u8],
        depth_max: i32,
        breadth_max: usize,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        logger.print_esc_str(b"overline");
        if let Some(nucleus) = &self.nucleus {
            nucleus.display(b'.', indent_str, depth_max, breadth_max, eqtb, logger);
        }
        if let Some(superscript) = &self.superscript {
            superscript.display(b'^', indent_str, depth_max, breadth_max, eqtb, logger);
        }
        if let Some(subscript) = &self.subscript {
            subscript.display(b'_', indent_str, depth_max, breadth_max, eqtb, logger);
        }
    }
}

/// See 687.
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct AccentNoad {
    pub accent_fam: u8,
    pub accent_char: u8,
    pub nucleus: Option<Box<NoadField>>,
    pub subscript: Option<Box<NoadField>>,
    pub superscript: Option<Box<NoadField>>,
}

impl AccentNoad {
    /// See 696.
    fn display(
        &self,
        indent_str: &[u8],
        depth_max: i32,
        breadth_max: usize,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        logger.print_esc_str(b"accent");
        print_fam_and_char(self.accent_fam, self.accent_char, logger);
        if let Some(nucleus) = &self.nucleus {
            nucleus.display(b'.', indent_str, depth_max, breadth_max, eqtb, logger);
        }
        if let Some(superscript) = &self.superscript {
            superscript.display(b'^', indent_str, depth_max, breadth_max, eqtb, logger);
        }
        if let Some(subscript) = &self.subscript {
            subscript.display(b'_', indent_str, depth_max, breadth_max, eqtb, logger);
        }
    }
}

/// See 687.
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct VcenterNoad {
    pub nucleus: ListNode,
    pub subscript: Option<Box<NoadField>>,
    pub superscript: Option<Box<NoadField>>,
}

impl VcenterNoad {
    /// See 696.
    fn display(
        &self,
        indent_str: &[u8],
        depth_max: i32,
        breadth_max: usize,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        logger.print_esc_str(b"vcenter");
        if indent_str.len() as i32 >= depth_max {
            logger.print_str(" []");
        } else {
            let mut indent_str = indent_str.to_vec();
            indent_str.push(b'.');
            show_list_node(
                &self.nucleus,
                &indent_str,
                depth_max,
                breadth_max,
                eqtb,
                logger,
            );
        }
        if let Some(superscript) = &self.superscript {
            superscript.display(b'^', indent_str, depth_max, breadth_max, eqtb, logger);
        }
        if let Some(subscript) = &self.subscript {
            subscript.display(b'_', indent_str, depth_max, breadth_max, eqtb, logger);
        }
    }
}

/// See 687.
#[derive(Debug, PartialEq, Eq)]
pub struct LeftNoad {
    pub delimiter: DelimiterField,
}

impl LeftNoad {
    /// See 696.
    fn display(&self, logger: &mut Logger) {
        logger.print_esc_str(b"left");
        self.delimiter.display(logger);
    }
}

/// See 687.
#[derive(Debug, PartialEq, Eq)]
pub struct RightNoad {
    pub delimiter: DelimiterField,
}

impl RightNoad {
    /// See 696.
    fn display(&self, logger: &mut Logger) {
        logger.print_esc_str(b"right");
        self.delimiter.display(logger);
    }
}

/// See 688.
#[derive(Debug, PartialEq, Eq)]
pub struct StyleNode {
    pub style: MathStyle,
}

impl StyleNode {
    /// See 688.
    pub fn new(style: MathStyle) -> Self {
        Self { style }
    }

    /// See 690.
    pub fn display(&self, logger: &mut Logger) {
        self.style.display(logger);
    }
}

// NOTE This is only needed so that we can implement Clone for Node but it should be
// impossible that cloning is every invoked on this type of Node.
impl Clone for StyleNode {
    fn clone(&self) -> Self {
        panic!("It should not happen that these get copied in copy_node_list")
    }
}

/// See 689.
// NOTE We wrap the Vec's in Boxes to reduce the size of the Node. The performance impact should be
// limited as long as these are not used too often.
#[cfg_attr(test, derive(PartialEq))]
#[allow(clippy::box_collection)]
#[derive(Debug)]
pub struct ChoiceNode {
    pub display_mlist: Box<Vec<Node>>,
    pub text_mlist: Box<Vec<Node>>,
    pub script_mlist: Box<Vec<Node>>,
    pub script_script_mlist: Box<Vec<Node>>,
}

// NOTE This is only needed so that we can implement Clone for Node but it should be
// impossible that cloning is every invoked on this type of Node.
impl Clone for ChoiceNode {
    fn clone(&self) -> Self {
        panic!("It should not happen that these get copied in copy_node_list")
    }
}

impl ChoiceNode {
    /// See 695.
    pub fn display(
        &self,
        indent_str: &[u8],
        depth_max: i32,
        breadth_max: usize,
        eqtb: &Eqtb,
        logger: &mut Logger,
    ) {
        logger.print_esc_str(b"mathchoice");
        let mut display_indent_str = indent_str.to_vec();
        display_indent_str.push(b'D');
        show_node_list(
            &self.display_mlist,
            &display_indent_str,
            depth_max,
            breadth_max,
            eqtb,
            logger,
        );
        let mut text_indent_str = indent_str.to_vec();
        text_indent_str.push(b'T');
        show_node_list(
            &self.text_mlist,
            &text_indent_str,
            depth_max,
            breadth_max,
            eqtb,
            logger,
        );
        let mut script_indent_str = indent_str.to_vec();
        script_indent_str.push(b'S');
        show_node_list(
            &self.script_mlist,
            &script_indent_str,
            depth_max,
            breadth_max,
            eqtb,
            logger,
        );
        let mut script_script_indent_str = indent_str.to_vec();
        script_script_indent_str.push(b's');
        show_node_list(
            &self.script_script_mlist,
            &script_script_indent_str,
            depth_max,
            breadth_max,
            eqtb,
            logger,
        );
    }
}

/// See 691.
fn print_fam_and_char(fam: u8, character: u8, logger: &mut Logger) {
    logger.print_esc_str(b"fam");
    logger.print_int(fam as i32);
    logger.print_char(b' ');
    logger.print(character);
}

/// See 693.
fn show_info(
    node_list: &[Node],
    indent_str: &[u8],
    depth_max: i32,
    breadth_max: usize,
    eqtb: &Eqtb,
    logger: &mut Logger,
) {
    show_node_list(node_list, indent_str, depth_max, breadth_max, eqtb, logger);
}
