use super::{Dumpable, FormatError};
use crate::dimension::Dimension;
use crate::eqtb::{FontIndex, RegisterIndex, SkipVariable};
use crate::nodes::noads::{ChoiceNode, Noad, StyleNode};
use crate::nodes::{
    AdjustNode, CharNode, CloseNode, DimensionOrder, DiscNode, GlueNode, GlueRatio, GlueSign,
    GlueSpec, GlueType, HigherOrderDimension, HlistOrVlist, InsNode, KernNode, KernSubtype,
    LanguageNode, LeaderKind, LigatureNode, ListNode, MarkNode, MathNode, MathNodeKind, Node,
    OpenNode, PenaltyNode, RuleNode, SpecialNode, UnsetNode, WhatsitNode, WriteNode,
};

use std::io::Write;
use std::path::PathBuf;

impl Dumpable for Node {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::Char(char_node) => {
                writeln!(target, "Char")?;
                char_node.dump(target)?;
            }
            Self::List(list_node) => {
                writeln!(target, "List")?;
                list_node.dump(target)?;
            }
            Self::Rule(rule_node) => {
                writeln!(target, "Rule")?;
                rule_node.dump(target)?;
            }
            Self::Ins(ins_node) => {
                writeln!(target, "Ins")?;
                ins_node.dump(target)?;
            }
            Self::Mark(mark_node) => {
                writeln!(target, "Mark")?;
                mark_node.dump(target)?;
            }
            Self::Adjust(adjust_node) => {
                writeln!(target, "Adjust")?;
                adjust_node.dump(target)?;
            }
            Self::Ligature(ligature_node) => {
                writeln!(target, "Ligature")?;
                ligature_node.dump(target)?;
            }
            Self::Disc(disc_node) => {
                writeln!(target, "Disc")?;
                disc_node.dump(target)?;
            }
            Self::Whatsit(whatsit_node) => {
                writeln!(target, "Whatsit")?;
                whatsit_node.dump(target)?;
            }
            Self::Math(math_node) => {
                writeln!(target, "Math")?;
                math_node.dump(target)?;
            }
            Self::Glue(glue_node) => {
                writeln!(target, "Glue")?;
                glue_node.dump(target)?;
            }
            Self::Kern(kern_node) => {
                writeln!(target, "Kern")?;
                kern_node.dump(target)?;
            }
            Self::Penalty(penalty_node) => {
                writeln!(target, "Penalty")?;
                penalty_node.dump(target)?;
            }
            Self::Unset(unset_node) => {
                writeln!(target, "Unset")?;
                unset_node.dump(target)?;
            }
            Self::Style(style_node) => {
                writeln!(target, "Style")?;
                style_node.dump(target)?;
            }
            Self::Choice(choice_node) => {
                writeln!(target, "Choice")?;
                choice_node.dump(target)?;
            }
            Self::Noad(noad) => {
                writeln!(target, "Noad")?;
                noad.dump(target)?;
            }
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "Char" => {
                let char_node = CharNode::undump(lines)?;
                Ok(Self::Char(char_node))
            }
            "List" => {
                let list_node = ListNode::undump(lines)?;
                Ok(Self::List(list_node))
            }
            "Rule" => {
                let rule_node = RuleNode::undump(lines)?;
                Ok(Self::Rule(rule_node))
            }
            "Ins" => {
                let ins_node = InsNode::undump(lines)?;
                Ok(Self::Ins(ins_node))
            }
            "Mark" => {
                let mark_node = MarkNode::undump(lines)?;
                Ok(Self::Mark(mark_node))
            }
            "Adjust" => {
                let adjust_node = AdjustNode::undump(lines)?;
                Ok(Self::Adjust(adjust_node))
            }
            "Ligature" => {
                let ligature_node = LigatureNode::undump(lines)?;
                Ok(Self::Ligature(ligature_node))
            }
            "Disc" => {
                let disc_node = DiscNode::undump(lines)?;
                Ok(Self::Disc(disc_node))
            }
            "Whatsit" => {
                let whatsit_node = WhatsitNode::undump(lines)?;
                Ok(Self::Whatsit(whatsit_node))
            }
            "Math" => {
                let math_node = MathNode::undump(lines)?;
                Ok(Self::Math(math_node))
            }
            "Glue" => {
                let glue_node = GlueNode::undump(lines)?;
                Ok(Self::Glue(glue_node))
            }
            "Kern" => {
                let kern_node = KernNode::undump(lines)?;
                Ok(Self::Kern(kern_node))
            }
            "Penalty" => {
                let penalty_node = PenaltyNode::undump(lines)?;
                Ok(Self::Penalty(penalty_node))
            }
            "Unset" => {
                let unset_node = UnsetNode::undump(lines)?;
                Ok(Self::Unset(unset_node))
            }
            "Style" => {
                let style_node = StyleNode::undump(lines)?;
                Ok(Self::Style(style_node))
            }
            "Choice" => {
                let choice_node = ChoiceNode::undump(lines)?;
                Ok(Self::Choice(choice_node))
            }
            "Noad" => {
                let noad = Noad::undump(lines)?;
                Ok(Self::Noad(noad))
            }
            _ => Err(FormatError::ParseError),
        }
    }
}

impl Dumpable for CharNode {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.font_index.dump(target)?;
        self.character.dump(target)?;
        self.width.dump(target)?;
        self.height.dump(target)?;
        self.depth.dump(target)?;
        self.italic.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let font_index = FontIndex::undump(lines)?;
        let character = u8::undump(lines)?;
        let width = Dimension::undump(lines)?;
        let height = Dimension::undump(lines)?;
        let depth = Dimension::undump(lines)?;
        let italic = Dimension::undump(lines)?;
        Ok(Self {
            font_index,
            character,
            width,
            height,
            depth,
            italic,
        })
    }
}

impl Dumpable for ListNode {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.width.dump(target)?;
        self.height.dump(target)?;
        self.depth.dump(target)?;
        self.shift_amount.dump(target)?;
        match &self.list {
            HlistOrVlist::Hlist(hlist) => {
                writeln!(target, "Hlist")?;
                hlist.dump(target)?;
            }
            HlistOrVlist::Vlist(vlist) => {
                writeln!(target, "Vlist")?;
                vlist.dump(target)?;
            }
        }
        self.glue_set.dump(target)?;
        self.glue_sign.dump(target)?;
        self.glue_order.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let width = Dimension::undump(lines)?;
        let height = Dimension::undump(lines)?;
        let depth = Dimension::undump(lines)?;
        let shift_amount = Dimension::undump(lines)?;
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        let list = match variant {
            "Hlist" => {
                let hlist = Vec::undump(lines)?;
                HlistOrVlist::Hlist(hlist)
            }
            "Vlist" => {
                let vlist = Vec::undump(lines)?;
                HlistOrVlist::Vlist(vlist)
            }
            _ => return Err(FormatError::ParseError),
        };
        let glue_set = GlueRatio::undump(lines)?;
        let glue_sign = GlueSign::undump(lines)?;
        let glue_order = DimensionOrder::undump(lines)?;
        Ok(Self {
            width,
            height,
            depth,
            shift_amount,
            list,
            glue_set,
            glue_sign,
            glue_order,
        })
    }
}

impl Dumpable for RuleNode {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.width.dump(target)?;
        self.height.dump(target)?;
        self.depth.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let width = Dimension::undump(lines)?;
        let height = Dimension::undump(lines)?;
        let depth = Dimension::undump(lines)?;
        Ok(Self {
            width,
            height,
            depth,
        })
    }
}

impl Dumpable for InsNode {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.number.dump(target)?;
        self.size.dump(target)?;
        self.split_max_depth.dump(target)?;
        self.split_top_skip.dump(target)?;
        self.insertion.dump(target)?;
        self.float_cost.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let number = RegisterIndex::undump(lines)?;
        let height = Dimension::undump(lines)?;
        let depth = Dimension::undump(lines)?;
        let split_top_skip = std::rc::Rc::undump(lines)?;
        let insertion = Vec::undump(lines)?;
        let float_cost = i32::undump(lines)?;
        Ok(Self {
            number,
            size: height,
            split_max_depth: depth,
            split_top_skip,
            insertion,
            float_cost,
        })
    }
}

impl Dumpable for MarkNode {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.mark.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let mark = std::rc::Rc::undump(lines)?;
        Ok(Self { mark })
    }
}

impl Dumpable for AdjustNode {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.adjustment.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let adjustment = Vec::undump(lines)?;
        Ok(Self { adjustment })
    }
}

impl Dumpable for LigatureNode {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.font_index.dump(target)?;
        self.character.dump(target)?;
        self.width.dump(target)?;
        self.height.dump(target)?;
        self.depth.dump(target)?;
        self.italic.dump(target)?;
        self.left_boundary.dump(target)?;
        self.right_boundary.dump(target)?;
        self.lig.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let font_index = FontIndex::undump(lines)?;
        let character = u8::undump(lines)?;
        let width = Dimension::undump(lines)?;
        let height = Dimension::undump(lines)?;
        let depth = Dimension::undump(lines)?;
        let italic = Dimension::undump(lines)?;
        let left_boundary = bool::undump(lines)?;
        let right_boundary = bool::undump(lines)?;
        let lig = Vec::undump(lines)?;
        Ok(Self {
            font_index,
            character,
            width,
            height,
            depth,
            italic,
            left_boundary,
            right_boundary,
            lig,
        })
    }
}

impl Dumpable for DiscNode {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.pre_break.dump(target)?;
        self.post_break.dump(target)?;
        self.no_break.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let pre_break = Vec::undump(lines)?;
        let post_break = Vec::undump(lines)?;
        let no_break = Vec::undump(lines)?;
        Ok(Self {
            pre_break,
            post_break,
            no_break,
        })
    }
}

impl Dumpable for WhatsitNode {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::Open(open_node) => {
                writeln!(target, "Open")?;
                open_node.dump(target)?;
            }
            Self::Write(write_node) => {
                writeln!(target, "Write")?;
                write_node.dump(target)?;
            }
            Self::Close(close_node) => {
                writeln!(target, "Close")?;
                close_node.dump(target)?;
            }
            Self::Special(special_node) => {
                writeln!(target, "Special")?;
                special_node.dump(target)?;
            }
            Self::Language(language_node) => {
                writeln!(target, "Language")?;
                language_node.dump(target)?;
            }
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "Open" => {
                let open_node = OpenNode::undump(lines)?;
                Ok(Self::Open(open_node))
            }
            "Write" => {
                let write_node = WriteNode::undump(lines)?;
                Ok(Self::Write(write_node))
            }
            "Close" => {
                let close_node = CloseNode::undump(lines)?;
                Ok(Self::Close(close_node))
            }
            "Special" => {
                let special_node = SpecialNode::undump(lines)?;
                Ok(Self::Special(special_node))
            }
            "Language" => {
                let language_node = LanguageNode::undump(lines)?;
                Ok(Self::Language(language_node))
            }
            _ => Err(FormatError::ParseError),
        }
    }
}

impl Dumpable for MathNode {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.kind.dump(target)?;
        self.width.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let kind = MathNodeKind::undump(lines)?;
        let width = Dimension::undump(lines)?;
        Ok(Self { kind, width })
    }
}

impl Dumpable for MathNodeKind {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::Before => {
                writeln!(target, "Before")?;
            }
            Self::After => {
                writeln!(target, "After")?;
            }
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "Before" => Ok(Self::Before),
            "After" => Ok(Self::After),
            _ => Err(FormatError::ParseError),
        }
    }
}

impl Dumpable for GlueNode {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.subtype.dump(target)?;
        self.glue_spec.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let subtype = GlueType::undump(lines)?;
        let glue_spec = std::rc::Rc::undump(lines)?;
        Ok(Self { subtype, glue_spec })
    }
}

impl Dumpable for GlueType {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::Normal => writeln!(target, "Normal")?,
            Self::Skip(skip_var) => {
                writeln!(target, "Skip")?;
                skip_var.dump(target)?;
            }
            Self::NonScript => writeln!(target, "NonScript")?,
            Self::MuGlue => writeln!(target, "MuGlue")?,
            Self::Leaders {
                leader_kind,
                leader_node,
            } => {
                writeln!(target, "Leaders")?;
                leader_kind.dump(target)?;
                leader_node.dump(target)?;
            }
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        let glue_type = match variant {
            "Normal" => Self::Normal,
            "Skip" => {
                let skip_var = SkipVariable::undump(lines)?;
                Self::Skip(skip_var)
            }
            "NonScript" => Self::NonScript,
            "MuGlue" => Self::MuGlue,
            "Leaders" => {
                let leader_kind = LeaderKind::undump(lines)?;
                let leader_node = Box::new(Node::undump(lines)?);
                Self::Leaders {
                    leader_kind,
                    leader_node,
                }
            }
            _ => return Err(FormatError::ParseError),
        };
        Ok(glue_type)
    }
}

impl Dumpable for LeaderKind {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::ALeaders => writeln!(target, "ALeaders")?,
            Self::CLeaders => writeln!(target, "CLeaders")?,
            Self::XLeaders => writeln!(target, "XLeaders")?,
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "ALeaders" => Ok(Self::ALeaders),
            "CLeaders" => Ok(Self::CLeaders),
            "XLeaders" => Ok(Self::XLeaders),
            _ => Err(FormatError::ParseError),
        }
    }
}

impl Dumpable for KernNode {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self.subtype {
            KernSubtype::Normal => writeln!(target, "Normal")?,
            KernSubtype::Explicit => writeln!(target, "Explicit")?,
            KernSubtype::AccKern => writeln!(target, "AccKern")?,
            KernSubtype::MuGlue => writeln!(target, "MuGlue")?,
        }
        self.width.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        let subtype = match variant {
            "Normal" => KernSubtype::Normal,
            "Explicit" => KernSubtype::Explicit,
            "AccKern" => KernSubtype::AccKern,
            "MuGlue" => KernSubtype::MuGlue,
            _ => return Err(FormatError::ParseError),
        };
        let width = Dimension::undump(lines)?;
        Ok(Self { subtype, width })
    }
}

impl Dumpable for PenaltyNode {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.penalty.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let penalty = i32::undump(lines)?;
        Ok(Self { penalty })
    }
}

impl Dumpable for UnsetNode {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.width.dump(target)?;
        self.height.dump(target)?;
        self.depth.dump(target)?;
        self.list.dump(target)?;
        self.stretch.dump(target)?;
        self.shrink.dump(target)?;
        self.span_count.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let width = Dimension::undump(lines)?;
        let height = Dimension::undump(lines)?;
        let depth = Dimension::undump(lines)?;
        let list = Vec::undump(lines)?;
        let stretch = HigherOrderDimension::undump(lines)?;
        let shrink = HigherOrderDimension::undump(lines)?;
        let span_count = usize::undump(lines)?;
        Ok(Self {
            width,
            height,
            depth,
            list,
            stretch,
            shrink,
            span_count,
        })
    }
}

impl Dumpable for OpenNode {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        use std::os::unix::ffi::OsStrExt;

        self.write_stream.dump(target)?;
        let path_bytes = self.path.as_os_str().as_bytes().to_vec();
        path_bytes.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        use std::os::unix::ffi::OsStrExt;
        let write_stream = usize::undump(lines)?;
        let path_bytes: Vec<u8> = Vec::undump(lines)?;
        let path_str = std::ffi::OsStr::from_bytes(&path_bytes);
        let path = PathBuf::from(path_str);
        Ok(Self { write_stream, path })
    }
}

impl Dumpable for WriteNode {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.write_stream.dump(target)?;
        self.tokens.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let write_stream = usize::undump(lines)?;
        let tokens = std::rc::Rc::undump(lines)?;
        Ok(Self {
            write_stream,
            tokens,
        })
    }
}

impl Dumpable for CloseNode {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.write_stream.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let write_stream = usize::undump(lines)?;
        Ok(Self { write_stream })
    }
}

impl Dumpable for SpecialNode {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.tokens.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let tokens = std::rc::Rc::undump(lines)?;
        Ok(Self { tokens })
    }
}

impl Dumpable for LanguageNode {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.language.dump(target)?;
        self.left_hyphen_min.dump(target)?;
        self.right_hyphen_min.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let language = usize::undump(lines)?;
        let left_hyphen_min = usize::undump(lines)?;
        let right_hyphen_min = usize::undump(lines)?;
        Ok(Self {
            language,
            left_hyphen_min,
            right_hyphen_min,
        })
    }
}

impl Dumpable for GlueSpec {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.width.dump(target)?;
        self.stretch.dump(target)?;
        self.shrink.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let width = Dimension::undump(lines)?;
        let stretch = HigherOrderDimension::undump(lines)?;
        let shrink = HigherOrderDimension::undump(lines)?;
        Ok(Self {
            width,
            stretch,
            shrink,
        })
    }
}

impl Dumpable for HigherOrderDimension {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.order.dump(target)?;
        self.value.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let order = DimensionOrder::undump(lines)?;
        let value = i32::undump(lines)?;
        Ok(Self { order, value })
    }
}

impl Dumpable for DimensionOrder {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            DimensionOrder::Normal => {
                writeln!(target, "Normal")?;
            }
            DimensionOrder::Fil => {
                writeln!(target, "Fil")?;
            }
            DimensionOrder::Fill => {
                writeln!(target, "Fill")?;
            }
            DimensionOrder::Filll => {
                writeln!(target, "Filll")?;
            }
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "Normal" => Ok(DimensionOrder::Normal),
            "Fil" => Ok(DimensionOrder::Fil),
            "Fill" => Ok(DimensionOrder::Fill),
            "Filll" => Ok(DimensionOrder::Filll),
            _ => Err(FormatError::ParseError),
        }
    }
}

impl Dumpable for GlueSign {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::Normal => {
                writeln!(target, "Normal")?;
            }
            Self::Stretching => {
                writeln!(target, "Stretching")?;
            }
            Self::Shrinking => {
                writeln!(target, "Shrinking")?;
            }
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "Normal" => Ok(GlueSign::Normal),
            "Stretching" => Ok(GlueSign::Stretching),
            "Shrinking" => Ok(GlueSign::Shrinking),
            _ => Err(FormatError::ParseError),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eqtb::ControlSequence;
    use crate::math::MathStyle;
    use crate::nodes::noads::OverNoad;
    use crate::token::Token;

    #[test]
    fn dump_node() {
        let chr = Node::Char(CharNode {
            font_index: 1,
            character: 0,
            width: 1,
            height: 2,
            depth: -3,
            italic: 0,
        });
        let list = Node::List(ListNode {
            width: 1,
            height: 2,
            depth: 3,
            shift_amount: 4,
            list: HlistOrVlist::Hlist(vec![Node::Char(CharNode {
                font_index: 5,
                character: 6,
                width: 1,
                height: 2,
                depth: -3,
                italic: 0,
            })]),
            glue_set: 0.999999,
            glue_sign: GlueSign::Shrinking,
            glue_order: DimensionOrder::Fill,
        });
        let rule = Node::Rule(RuleNode {
            width: 5,
            height: 6,
            depth: -1,
        });
        let ins = Node::Ins(InsNode {
            number: 1,
            size: 2,
            split_max_depth: 3,
            split_top_skip: std::rc::Rc::new(GlueSpec::FIL_GLUE),
            insertion: vec![Node::Rule(RuleNode {
                width: 5,
                height: 6,
                depth: -1,
            })],
            float_cost: -1234,
        });
        let mark = Node::Mark(MarkNode {
            mark: std::rc::Rc::new(vec![
                Token::LeftBrace(1),
                Token::MacParam(3),
                Token::CSToken {
                    cs: ControlSequence::FrozenFi,
                },
            ]),
        });
        let adjust = Node::Adjust(AdjustNode {
            adjustment: Vec::new(),
        });
        let ligature = Node::Ligature(LigatureNode {
            font_index: 12,
            character: 43,
            width: 1,
            height: 2,
            depth: -3,
            italic: 0,
            left_boundary: false,
            right_boundary: true,
            lig: vec![45, 89],
        });
        let disc = Node::Disc(DiscNode {
            pre_break: Vec::new(),
            post_break: vec![
                Node::Char(CharNode {
                    font_index: 23,
                    character: 45,
                    width: 1,
                    height: 2,
                    depth: -3,
                    italic: 0,
                }),
                Node::Char(CharNode {
                    font_index: 67,
                    character: 89,
                    width: 1,
                    height: 2,
                    depth: -3,
                    italic: 0,
                }),
            ],
            no_break: Vec::new(),
        });
        let whatsit = Node::Whatsit(WhatsitNode::Open(OpenNode {
            write_stream: 12,
            path: PathBuf::from("../.\x0012asdf/..asd.t"),
        }));
        let math = Node::Math(MathNode {
            kind: MathNodeKind::Before,
            width: 12345,
        });
        let glue = Node::Glue(GlueNode {
            subtype: GlueType::Skip(SkipVariable::TabSkip),
            glue_spec: std::rc::Rc::new(GlueSpec::FIL_NEG_GLUE),
        });
        let kern = Node::Kern(KernNode {
            subtype: KernSubtype::Normal,
            width: -1,
        });
        let penalty = Node::Penalty(PenaltyNode { penalty: -123 });
        let unset = Node::Unset(UnsetNode {
            width: 1,
            height: 2,
            depth: 3,
            list: vec![
                Node::Char(CharNode {
                    font_index: 23,
                    character: 45,
                    width: 1,
                    height: 2,
                    depth: -3,
                    italic: 0,
                }),
                Node::Char(CharNode {
                    font_index: 67,
                    character: 89,
                    width: 1,
                    height: 2,
                    depth: -3,
                    italic: 0,
                }),
            ],
            stretch: HigherOrderDimension {
                order: DimensionOrder::Normal,
                value: 23,
            },
            shrink: HigherOrderDimension {
                order: DimensionOrder::Filll,
                value: 34,
            },
            span_count: 12,
        });
        let style = Node::Style(StyleNode {
            style: MathStyle::Display { cramped: false },
        });
        let choice = Node::Choice(ChoiceNode {
            display_mlist: Box::new(Vec::new()),
            text_mlist: Box::new(Vec::new()),
            script_mlist: Box::new(Vec::new()),
            script_script_mlist: Box::new(Vec::new()),
        });
        let noad = Node::Noad(Noad::Over(OverNoad {
            nucleus: None,
            subscript: None,
            superscript: None,
        }));

        let mut file = Vec::new();
        chr.dump(&mut file).unwrap();
        list.dump(&mut file).unwrap();
        rule.dump(&mut file).unwrap();
        ins.dump(&mut file).unwrap();
        mark.dump(&mut file).unwrap();
        adjust.dump(&mut file).unwrap();
        ligature.dump(&mut file).unwrap();
        disc.dump(&mut file).unwrap();
        whatsit.dump(&mut file).unwrap();
        math.dump(&mut file).unwrap();
        glue.dump(&mut file).unwrap();
        kern.dump(&mut file).unwrap();
        penalty.dump(&mut file).unwrap();
        unset.dump(&mut file).unwrap();
        style.dump(&mut file).unwrap();
        choice.dump(&mut file).unwrap();
        noad.dump(&mut file).unwrap();
        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let char_undumped = Node::undump(&mut lines).unwrap();
        let list_undumped = Node::undump(&mut lines).unwrap();
        let rule_undumped = Node::undump(&mut lines).unwrap();
        let ins_undumped = Node::undump(&mut lines).unwrap();
        let mark_undumped = Node::undump(&mut lines).unwrap();
        let adjust_undumped = Node::undump(&mut lines).unwrap();
        let ligature_undumped = Node::undump(&mut lines).unwrap();
        let disc_undumped = Node::undump(&mut lines).unwrap();
        let whatsit_undumped = Node::undump(&mut lines).unwrap();
        let math_undumped = Node::undump(&mut lines).unwrap();
        let glue_undumped = Node::undump(&mut lines).unwrap();
        let kern_undumped = Node::undump(&mut lines).unwrap();
        let penalty_undumped = Node::undump(&mut lines).unwrap();
        let unset_undumped = Node::undump(&mut lines).unwrap();
        let style_undumped = Node::undump(&mut lines).unwrap();
        let choice_undumped = Node::undump(&mut lines).unwrap();
        let noad_undumped = Node::undump(&mut lines).unwrap();
        assert_eq!(chr, char_undumped);
        assert_eq!(list, list_undumped);
        assert_eq!(rule, rule_undumped);
        assert_eq!(ins, ins_undumped);
        assert_eq!(mark, mark_undumped);
        assert_eq!(adjust, adjust_undumped);
        assert_eq!(ligature, ligature_undumped);
        assert_eq!(disc, disc_undumped);
        assert_eq!(whatsit, whatsit_undumped);
        assert_eq!(math, math_undumped);
        assert_eq!(glue, glue_undumped);
        assert_eq!(kern, kern_undumped);
        assert_eq!(penalty, penalty_undumped);
        assert_eq!(unset, unset_undumped);
        assert_eq!(style, style_undumped);
        assert_eq!(choice, choice_undumped);
        assert_eq!(noad, noad_undumped);
    }

    #[test]
    fn dump_char_node() {
        let char_node = CharNode {
            font_index: 12,
            character: 34,
            width: 1,
            height: 2,
            depth: -3,
            italic: 0,
        };
        let mut file = Vec::new();
        char_node.dump(&mut file).unwrap();
        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let char_node_undumped = CharNode::undump(&mut lines).unwrap();
        assert_eq!(char_node, char_node_undumped);
    }

    #[test]
    fn dump_list_node() {
        let hlist_node = ListNode {
            width: 1,
            height: 2,
            depth: 3,
            shift_amount: 4,
            list: HlistOrVlist::Hlist(vec![Node::Char(CharNode {
                font_index: 5,
                character: 6,
                width: 1,
                height: 2,
                depth: -3,
                italic: 0,
            })]),
            glue_set: 0.999999,
            glue_sign: GlueSign::Stretching,
            glue_order: DimensionOrder::Normal,
        };
        let vlist_node = ListNode {
            width: 1,
            height: 2,
            depth: 3,
            shift_amount: 4,
            list: HlistOrVlist::Vlist(vec![Node::Rule(RuleNode {
                width: 5,
                height: 6,
                depth: -1,
            })]),
            glue_set: 0.123456,
            glue_sign: GlueSign::Normal,
            glue_order: DimensionOrder::Fil,
        };
        let mut file = Vec::new();
        hlist_node.dump(&mut file).unwrap();
        vlist_node.dump(&mut file).unwrap();
        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let hlist_node_undumped = ListNode::undump(&mut lines).unwrap();
        let vlist_node_undumped = ListNode::undump(&mut lines).unwrap();
        assert_eq!(hlist_node, hlist_node_undumped);
        assert_eq!(vlist_node, vlist_node_undumped);
    }

    #[test]
    fn dump_rule_node() {
        let rule_node = RuleNode {
            width: -1,
            height: 12,
            depth: 0,
        };
        let mut file = Vec::new();
        rule_node.dump(&mut file).unwrap();
        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let rule_node_undumped = RuleNode::undump(&mut lines).unwrap();
        assert_eq!(rule_node, rule_node_undumped);
    }

    #[test]
    fn dump_ins_node() {
        let ins_node = InsNode {
            number: 1,
            size: 2,
            split_max_depth: 3,
            split_top_skip: std::rc::Rc::new(GlueSpec::FIL_GLUE),
            insertion: vec![Node::Rule(RuleNode {
                width: 5,
                height: 6,
                depth: -1,
            })],
            float_cost: -1234,
        };
        let mut file = Vec::new();
        ins_node.dump(&mut file).unwrap();
        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let ins_node_undumped = InsNode::undump(&mut lines).unwrap();
        assert_eq!(ins_node, ins_node_undumped);
    }

    #[test]
    fn dump_mark_node() {
        let mark_node = MarkNode {
            mark: std::rc::Rc::new(vec![
                Token::RightBrace(12),
                Token::OtherChar(1),
                Token::CSToken {
                    cs: ControlSequence::FrozenFi,
                },
            ]),
        };
        let mut file = Vec::new();
        mark_node.dump(&mut file).unwrap();
        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let mark_node_undumped = MarkNode::undump(&mut lines).unwrap();
        assert_eq!(mark_node, mark_node_undumped);
    }

    #[test]
    fn dump_adjustment_node() {
        let adjustment_node = AdjustNode {
            adjustment: Vec::new(),
        };
        let mut file = Vec::new();
        adjustment_node.dump(&mut file).unwrap();
        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let adjustment_node_undumped = AdjustNode::undump(&mut lines).unwrap();
        assert_eq!(adjustment_node, adjustment_node_undumped);
    }

    #[test]
    fn dump_ligature_node() {
        let ligature_node = LigatureNode {
            font_index: 12,
            character: 43,
            width: 1,
            height: 2,
            depth: -3,
            italic: 0,
            left_boundary: false,
            right_boundary: true,
            lig: vec![45, 89],
        };

        let mut file = Vec::new();
        ligature_node.dump(&mut file).unwrap();
        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let ligature_node_undumped = LigatureNode::undump(&mut lines).unwrap();
        assert_eq!(ligature_node, ligature_node_undumped);
    }

    #[test]
    fn dump_disc_node() {
        let disc_node = DiscNode {
            pre_break: Vec::new(),
            post_break: vec![
                Node::Char(CharNode {
                    font_index: 23,
                    character: 45,
                    width: 1,
                    height: 2,
                    depth: -3,
                    italic: 0,
                }),
                Node::Char(CharNode {
                    font_index: 67,
                    character: 89,
                    width: 1,
                    height: 2,
                    depth: -3,
                    italic: 0,
                }),
            ],
            no_break: Vec::new(),
        };
        let mut file = Vec::new();
        disc_node.dump(&mut file).unwrap();
        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let disc_node_undumped = DiscNode::undump(&mut lines).unwrap();
        assert_eq!(disc_node, disc_node_undumped);
    }

    #[test]
    fn dump_whatsit_node() {
        let open = WhatsitNode::Open(OpenNode {
            write_stream: 12,
            path: PathBuf::from("../.\x0012asdf/..asd.t"),
        });
        let write = WhatsitNode::Write(WriteNode {
            write_stream: 12,
            tokens: std::rc::Rc::new(Vec::new()),
        });
        let close = WhatsitNode::Close(CloseNode { write_stream: 12 });
        let special = WhatsitNode::Special(SpecialNode {
            tokens: std::rc::Rc::new(vec![Token::TabMark(1)]),
        });
        let language = WhatsitNode::Language(LanguageNode {
            language: 1,
            left_hyphen_min: 2,
            right_hyphen_min: 3,
        });

        let mut file = Vec::new();
        open.dump(&mut file).unwrap();
        write.dump(&mut file).unwrap();
        close.dump(&mut file).unwrap();
        special.dump(&mut file).unwrap();
        language.dump(&mut file).unwrap();

        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let open_undumped = WhatsitNode::undump(&mut lines).unwrap();
        let write_undumped = WhatsitNode::undump(&mut lines).unwrap();
        let close_undumped = WhatsitNode::undump(&mut lines).unwrap();
        let special_undumped = WhatsitNode::undump(&mut lines).unwrap();
        let language_undumped = WhatsitNode::undump(&mut lines).unwrap();
        assert_eq!(open, open_undumped);
        assert_eq!(write, write_undumped);
        assert_eq!(close, close_undumped);
        assert_eq!(special, special_undumped);
        assert_eq!(language, language_undumped);
    }

    #[test]
    fn dump_math_node() {
        let math_node = MathNode {
            kind: MathNodeKind::After,
            width: 12345,
        };

        let mut file = Vec::new();
        math_node.dump(&mut file).unwrap();
        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let math_node_undumped = MathNode::undump(&mut lines).unwrap();
        assert_eq!(math_node, math_node_undumped);
    }

    #[test]
    fn dump_glue_node() {
        let glue_node = GlueNode {
            subtype: GlueType::NonScript,
            glue_spec: std::rc::Rc::new(GlueSpec::FIL_NEG_GLUE),
        };
        let leader_node = GlueNode {
            subtype: GlueType::Leaders {
                leader_kind: LeaderKind::XLeaders,
                leader_node: Box::new(Node::Char(CharNode {
                    font_index: 67,
                    character: 89,
                    width: 1,
                    height: 2,
                    depth: -3,
                    italic: 0,
                })),
            },
            glue_spec: std::rc::Rc::new(GlueSpec::ZERO_GLUE),
        };

        let mut file = Vec::new();
        glue_node.dump(&mut file).unwrap();
        leader_node.dump(&mut file).unwrap();
        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let glue_node_undumped = GlueNode::undump(&mut lines).unwrap();
        let leader_node_undumped = GlueNode::undump(&mut lines).unwrap();
        assert_eq!(glue_node, glue_node_undumped);
        assert_eq!(leader_node, leader_node_undumped);
    }

    #[test]
    fn dump_kern_node() {
        let normal = KernNode {
            subtype: KernSubtype::Normal,
            width: -1,
        };
        let explicit = KernNode {
            subtype: KernSubtype::Explicit,
            width: 0,
        };
        let acc_kern = KernNode {
            subtype: KernSubtype::AccKern,
            width: 1,
        };
        let mu_glue = KernNode {
            subtype: KernSubtype::MuGlue,
            width: 2,
        };

        let mut file = Vec::new();
        normal.dump(&mut file).unwrap();
        explicit.dump(&mut file).unwrap();
        acc_kern.dump(&mut file).unwrap();
        mu_glue.dump(&mut file).unwrap();

        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let normal_undumped = KernNode::undump(&mut lines).unwrap();
        let explicit_undumped = KernNode::undump(&mut lines).unwrap();
        let acc_kern_undumped = KernNode::undump(&mut lines).unwrap();
        let mu_glue_undumped = KernNode::undump(&mut lines).unwrap();
        assert_eq!(normal, normal_undumped);
        assert_eq!(explicit, explicit_undumped);
        assert_eq!(acc_kern, acc_kern_undumped);
        assert_eq!(mu_glue, mu_glue_undumped);
    }

    #[test]
    fn dump_penalty_node() {
        let penalty_node = PenaltyNode { penalty: -123 };

        let mut file = Vec::new();
        penalty_node.dump(&mut file).unwrap();
        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let penalty_node_undumped = PenaltyNode::undump(&mut lines).unwrap();
        assert_eq!(penalty_node, penalty_node_undumped);
    }

    #[test]
    fn dump_unset_node() {
        let unset_node = UnsetNode {
            width: 1,
            height: 2,
            depth: 3,
            list: vec![
                Node::Char(CharNode {
                    font_index: 23,
                    character: 45,
                    width: 1,
                    height: 2,
                    depth: -3,
                    italic: 0,
                }),
                Node::Char(CharNode {
                    font_index: 67,
                    character: 89,
                    width: 1,
                    height: 2,
                    depth: -3,
                    italic: 0,
                }),
            ],
            stretch: HigherOrderDimension {
                order: DimensionOrder::Normal,
                value: -23,
            },
            shrink: HigherOrderDimension {
                order: DimensionOrder::Filll,
                value: -34,
            },
            span_count: 12,
        };

        let mut file = Vec::new();
        unset_node.dump(&mut file).unwrap();
        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let unset_node_undumped = UnsetNode::undump(&mut lines).unwrap();
        assert_eq!(unset_node, unset_node_undumped);
    }

    #[test]
    fn dump_open_node() {
        let open_node = OpenNode {
            write_stream: 12,
            path: PathBuf::from("../.\x0012asdf/..asd.t"),
        };

        let mut file = Vec::new();
        open_node.dump(&mut file).unwrap();
        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let open_node_undumped = OpenNode::undump(&mut lines).unwrap();
        assert_eq!(open_node, open_node_undumped);
    }

    #[test]
    fn dump_write_node() {
        let write_node = WriteNode {
            write_stream: 12,
            tokens: std::rc::Rc::new(Vec::new()),
        };

        let mut file = Vec::new();
        write_node.dump(&mut file).unwrap();
        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let write_node_undumped = WriteNode::undump(&mut lines).unwrap();
        assert_eq!(write_node, write_node_undumped);
    }

    #[test]
    fn dump_close_node() {
        let close_node = CloseNode { write_stream: 12 };

        let mut file = Vec::new();
        close_node.dump(&mut file).unwrap();
        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let close_node_undumped = CloseNode::undump(&mut lines).unwrap();
        assert_eq!(close_node, close_node_undumped);
    }

    #[test]
    fn dump_special_node() {
        let special_node = SpecialNode {
            tokens: std::rc::Rc::new(vec![Token::Letter(1)]),
        };

        let mut file = Vec::new();
        special_node.dump(&mut file).unwrap();
        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let special_node_undumped = SpecialNode::undump(&mut lines).unwrap();
        assert_eq!(special_node, special_node_undumped);
    }

    #[test]
    fn dump_language_node() {
        let language_node = LanguageNode {
            language: 1,
            left_hyphen_min: 2,
            right_hyphen_min: 3,
        };

        let mut file = Vec::new();
        language_node.dump(&mut file).unwrap();
        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let language_node_undumped = LanguageNode::undump(&mut lines).unwrap();
        assert_eq!(language_node, language_node_undumped);
    }

    #[test]
    fn dump_glue_spec() {
        let glue_spec = GlueSpec {
            width: 12,
            stretch: HigherOrderDimension {
                order: DimensionOrder::Fil,
                value: 11,
            },
            shrink: HigherOrderDimension {
                order: DimensionOrder::Fill,
                value: 10,
            },
        };
        let mut file = Vec::new();
        glue_spec.dump(&mut file).unwrap();
        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let glue_spec_undumped = GlueSpec::undump(&mut lines).unwrap();
        assert_eq!(glue_spec, glue_spec_undumped);
    }

    #[test]
    fn dump_higher_order_dimension() {
        let normal = HigherOrderDimension {
            order: DimensionOrder::Normal,
            value: -1,
        };
        let fil = HigherOrderDimension {
            order: DimensionOrder::Fil,
            value: 0,
        };
        let fill = HigherOrderDimension {
            order: DimensionOrder::Fill,
            value: 1,
        };
        let filll = HigherOrderDimension {
            order: DimensionOrder::Filll,
            value: 2,
        };
        let mut file = Vec::new();
        normal.dump(&mut file).unwrap();
        fil.dump(&mut file).unwrap();
        fill.dump(&mut file).unwrap();
        filll.dump(&mut file).unwrap();

        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let normal_undumped = HigherOrderDimension::undump(&mut lines).unwrap();
        let fil_undumped = HigherOrderDimension::undump(&mut lines).unwrap();
        let fill_undumped = HigherOrderDimension::undump(&mut lines).unwrap();
        let filll_undumped = HigherOrderDimension::undump(&mut lines).unwrap();
        assert_eq!(normal, normal_undumped);
        assert_eq!(fil, fil_undumped);
        assert_eq!(fill, fill_undumped);
        assert_eq!(filll, filll_undumped);
    }
}
