use super::{Dumpable, FormatError};
use crate::dimension::Dimension;
use crate::math::MathStyle;
use crate::nodes::noads::{
    AccentNoad, ChoiceNode, DelimiterField, FractionNoad, LeftNoad, Noad, NoadField, NoadType,
    NormalNoad, OverNoad, RadicalNoad, RightNoad, StyleNode, UnderNoad, VcenterNoad,
};
use crate::nodes::ListNode;

use std::io::Write;

impl Dumpable for Noad {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::Normal(normal_noad) => {
                writeln!(target, "Normal")?;
                normal_noad.dump(target)?;
            }
            Self::Radical(radical_noad) => {
                writeln!(target, "Radical")?;
                radical_noad.dump(target)?;
            }
            Self::Fraction(fraction_noad) => {
                writeln!(target, "Fraction")?;
                fraction_noad.dump(target)?;
            }
            Self::Under(under_noad) => {
                writeln!(target, "Under")?;
                under_noad.dump(target)?;
            }
            Self::Over(over_noad) => {
                writeln!(target, "Over")?;
                over_noad.dump(target)?;
            }
            Self::Accent(accent_noad) => {
                writeln!(target, "Accent")?;
                accent_noad.dump(target)?;
            }
            Self::Vcenter(vcenter_noad) => {
                writeln!(target, "Vcenter")?;
                vcenter_noad.dump(target)?;
            }
            Self::Left(left_noad) => {
                writeln!(target, "Left")?;
                left_noad.dump(target)?;
            }
            Self::Right(right_noad) => {
                writeln!(target, "Right")?;
                right_noad.dump(target)?;
            }
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "Normal" => {
                let normal_noad = NormalNoad::undump(lines)?;
                Ok(Self::Normal(normal_noad))
            }
            "Radical" => {
                let radical_noad = RadicalNoad::undump(lines)?;
                Ok(Self::Radical(radical_noad))
            }
            "Fraction" => {
                let fraction_noad = FractionNoad::undump(lines)?;
                Ok(Self::Fraction(fraction_noad))
            }
            "Under" => {
                let under_noad = UnderNoad::undump(lines)?;
                Ok(Self::Under(under_noad))
            }
            "Over" => {
                let over_noad = OverNoad::undump(lines)?;
                Ok(Self::Over(over_noad))
            }
            "Accent" => {
                let accent_noad = AccentNoad::undump(lines)?;
                Ok(Self::Accent(accent_noad))
            }
            "Vcenter" => {
                let vcenter_noad = VcenterNoad::undump(lines)?;
                Ok(Self::Vcenter(vcenter_noad))
            }
            "Left" => {
                let left_noad = LeftNoad::undump(lines)?;
                Ok(Self::Left(left_noad))
            }
            "Right" => {
                let right_noad = RightNoad::undump(lines)?;
                Ok(Self::Right(right_noad))
            }
            _ => Err(FormatError::ParseError),
        }
    }
}

impl Dumpable for NormalNoad {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.noad_type.dump(target)?;
        self.nucleus.dump(target)?;
        self.subscript.dump(target)?;
        self.superscript.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let noad_type = NoadType::undump(lines)?;
        let nucleus = Option::undump(lines)?;
        let subscript = Option::undump(lines)?;
        let superscript = Option::undump(lines)?;
        Ok(Self {
            noad_type,
            nucleus,
            subscript,
            superscript,
        })
    }
}

impl Dumpable for RadicalNoad {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.left_delimiter.dump(target)?;
        self.nucleus.dump(target)?;
        self.subscript.dump(target)?;
        self.superscript.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let left_delimiter = DelimiterField::undump(lines)?;
        let nucleus = Option::undump(lines)?;
        let subscript = Option::undump(lines)?;
        let superscript = Option::undump(lines)?;
        Ok(Self {
            left_delimiter,
            nucleus,
            subscript,
            superscript,
        })
    }
}

impl Dumpable for FractionNoad {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.left_delimiter.dump(target)?;
        self.right_delimiter.dump(target)?;
        self.thickness.dump(target)?;
        self.numerator.dump(target)?;
        self.denominator.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let left_delimiter = DelimiterField::undump(lines)?;
        let right_delimiter = DelimiterField::undump(lines)?;
        let thickness = Dimension::undump(lines)?;
        let numerator = Vec::undump(lines)?;
        let denominator = Vec::undump(lines)?;
        Ok(Self {
            left_delimiter,
            right_delimiter,
            thickness,
            numerator,
            denominator,
        })
    }
}

impl Dumpable for UnderNoad {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.nucleus.dump(target)?;
        self.subscript.dump(target)?;
        self.superscript.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let nucleus = Option::undump(lines)?;
        let subscript = Option::undump(lines)?;
        let superscript = Option::undump(lines)?;
        Ok(Self {
            nucleus,
            subscript,
            superscript,
        })
    }
}

impl Dumpable for OverNoad {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.nucleus.dump(target)?;
        self.subscript.dump(target)?;
        self.superscript.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let nucleus = Option::undump(lines)?;
        let subscript = Option::undump(lines)?;
        let superscript = Option::undump(lines)?;
        Ok(Self {
            nucleus,
            subscript,
            superscript,
        })
    }
}

impl Dumpable for AccentNoad {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.accent_fam.dump(target)?;
        self.accent_char.dump(target)?;
        self.nucleus.dump(target)?;
        self.subscript.dump(target)?;
        self.superscript.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let accent_fam = u8::undump(lines)?;
        let accent_char = u8::undump(lines)?;
        let nucleus = Option::undump(lines)?;
        let subscript = Option::undump(lines)?;
        let superscript = Option::undump(lines)?;
        Ok(Self {
            accent_fam,
            accent_char,
            nucleus,
            subscript,
            superscript,
        })
    }
}

impl Dumpable for VcenterNoad {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.nucleus.dump(target)?;
        self.subscript.dump(target)?;
        self.superscript.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let nucleus = ListNode::undump(lines)?;
        let subscript = Option::undump(lines)?;
        let superscript = Option::undump(lines)?;
        Ok(Self {
            nucleus,
            subscript,
            superscript,
        })
    }
}

impl Dumpable for LeftNoad {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.delimiter.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let delimiter = DelimiterField::undump(lines)?;
        Ok(Self { delimiter })
    }
}

impl Dumpable for RightNoad {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.delimiter.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let delimiter = DelimiterField::undump(lines)?;
        Ok(Self { delimiter })
    }
}

impl Dumpable for StyleNode {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.style.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let style = MathStyle::undump(lines)?;
        Ok(Self { style })
    }
}

impl Dumpable for ChoiceNode {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.display_mlist.dump(target)?;
        self.text_mlist.dump(target)?;
        self.script_mlist.dump(target)?;
        self.script_script_mlist.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let display_mlist = Box::undump(lines)?;
        let text_mlist = Box::undump(lines)?;
        let script_mlist = Box::undump(lines)?;
        let script_script_mlist = Box::undump(lines)?;
        Ok(Self {
            display_mlist,
            text_mlist,
            script_mlist,
            script_script_mlist,
        })
    }
}

impl Dumpable for NoadField {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::MathChar { fam, character } => {
                writeln!(target, "MathChar")?;
                fam.dump(target)?;
                character.dump(target)?;
            }
            Self::MathTextChar { fam, character } => {
                writeln!(target, "MathTextChar")?;
                fam.dump(target)?;
                character.dump(target)?;
            }
            Self::SubBox { list_node } => {
                writeln!(target, "SubBox")?;
                list_node.dump(target)?;
            }
            Self::SubMlist { list } => {
                writeln!(target, "SubMlist")?;
                list.dump(target)?;
            }
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "MathChar" => {
                let fam = u8::undump(lines)?;
                let character = u8::undump(lines)?;
                Ok(Self::MathChar { fam, character })
            }
            "MathTextChar" => {
                let fam = u8::undump(lines)?;
                let character = u8::undump(lines)?;
                Ok(Self::MathTextChar { fam, character })
            }
            "SubBox" => {
                let list_node = ListNode::undump(lines)?;
                Ok(Self::SubBox { list_node })
            }
            "SubMlist" => {
                let list = Vec::undump(lines)?;
                Ok(Self::SubMlist { list })
            }
            _ => Err(FormatError::ParseError),
        }
    }
}

impl Dumpable for DelimiterField {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        self.small_fam.dump(target)?;
        self.small_char.dump(target)?;
        self.large_fam.dump(target)?;
        self.large_char.dump(target)?;
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let small_fam = u8::undump(lines)?;
        let small_char = u8::undump(lines)?;
        let large_fam = u8::undump(lines)?;
        let large_char = u8::undump(lines)?;
        Ok(Self {
            small_fam,
            small_char,
            large_fam,
            large_char,
        })
    }
}

impl Dumpable for MathStyle {
    fn dump(&self, target: &mut impl Write) -> Result<(), std::io::Error> {
        match self {
            Self::Display { cramped } => {
                writeln!(target, "Display")?;
                cramped.dump(target)?;
            }
            Self::Text { cramped } => {
                writeln!(target, "Text")?;
                cramped.dump(target)?;
            }
            Self::Script { cramped } => {
                writeln!(target, "Script")?;
                cramped.dump(target)?;
            }
            Self::ScriptScript { cramped } => {
                writeln!(target, "ScriptScript")?;
                cramped.dump(target)?;
            }
        }
        Ok(())
    }

    fn undump<'a>(lines: &mut impl Iterator<Item = &'a str>) -> Result<Self, FormatError> {
        let variant = lines.next().ok_or(FormatError::IncompleteFile)?;
        match variant {
            "Display" => {
                let cramped = bool::undump(lines)?;
                Ok(Self::Display { cramped })
            }
            "Text" => {
                let cramped = bool::undump(lines)?;
                Ok(Self::Text { cramped })
            }
            "Script" => {
                let cramped = bool::undump(lines)?;
                Ok(Self::Script { cramped })
            }
            "ScriptScript" => {
                let cramped = bool::undump(lines)?;
                Ok(Self::ScriptScript { cramped })
            }
            _ => Err(FormatError::ParseError),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::command::LimitType;

    #[test]
    fn dump_noad() {
        let normal = Noad::Normal(NormalNoad {
            noad_type: NoadType::Ord,
            nucleus: None,
            subscript: None,
            superscript: None,
        });
        let radical = Noad::Radical(RadicalNoad {
            left_delimiter: DelimiterField {
                small_fam: 1,
                small_char: 2,
                large_fam: 3,
                large_char: 4,
            },
            nucleus: None,
            subscript: None,
            superscript: None,
        });
        let fraction = Noad::Fraction(FractionNoad {
            left_delimiter: DelimiterField {
                small_fam: 1,
                small_char: 2,
                large_fam: 3,
                large_char: 4,
            },
            right_delimiter: DelimiterField {
                small_fam: 1,
                small_char: 2,
                large_fam: 3,
                large_char: 4,
            },
            thickness: 23,
            numerator: Vec::new(),
            denominator: Vec::new(),
        });
        let under = Noad::Under(UnderNoad {
            nucleus: None,
            subscript: None,
            superscript: None,
        });
        let over = Noad::Over(OverNoad {
            nucleus: None,
            subscript: None,
            superscript: None,
        });
        let accent = Noad::Accent(AccentNoad {
            accent_fam: 1,
            accent_char: 2,
            nucleus: None,
            subscript: None,
            superscript: None,
        });
        let vcenter = Noad::Vcenter(VcenterNoad {
            nucleus: ListNode::new_empty_hbox(),
            subscript: None,
            superscript: None,
        });
        let left = Noad::Left(LeftNoad {
            delimiter: DelimiterField {
                small_fam: 1,
                small_char: 2,
                large_fam: 3,
                large_char: 4,
            },
        });
        let right = Noad::Right(RightNoad {
            delimiter: DelimiterField {
                small_fam: 1,
                small_char: 2,
                large_fam: 3,
                large_char: 4,
            },
        });
        let mut file = Vec::new();
        normal.dump(&mut file).unwrap();
        radical.dump(&mut file).unwrap();
        fraction.dump(&mut file).unwrap();
        under.dump(&mut file).unwrap();
        over.dump(&mut file).unwrap();
        accent.dump(&mut file).unwrap();
        vcenter.dump(&mut file).unwrap();
        left.dump(&mut file).unwrap();
        right.dump(&mut file).unwrap();

        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let normal_undumped = Noad::undump(&mut lines).unwrap();
        let radical_undumped = Noad::undump(&mut lines).unwrap();
        let fraction_undumped = Noad::undump(&mut lines).unwrap();
        let under_undumped = Noad::undump(&mut lines).unwrap();
        let over_undumped = Noad::undump(&mut lines).unwrap();
        let accent_undumped = Noad::undump(&mut lines).unwrap();
        let vcenter_undumped = Noad::undump(&mut lines).unwrap();
        let left_undumped = Noad::undump(&mut lines).unwrap();
        let right_undumped = Noad::undump(&mut lines).unwrap();
        assert_eq!(normal, normal_undumped);
        assert_eq!(radical, radical_undumped);
        assert_eq!(fraction, fraction_undumped);
        assert_eq!(under, under_undumped);
        assert_eq!(over, over_undumped);
        assert_eq!(accent, accent_undumped);
        assert_eq!(vcenter, vcenter_undumped);
        assert_eq!(left, left_undumped);
        assert_eq!(right, right_undumped);
    }

    #[test]
    fn dump_normal_noad() {
        let ord = NormalNoad {
            noad_type: NoadType::Ord,
            nucleus: Some(Box::new(NoadField::MathChar {
                fam: 0,
                character: 1,
            })),
            subscript: None,
            superscript: None,
        };
        let op = NormalNoad {
            noad_type: NoadType::Op {
                limit_type: LimitType::DisplayLimits,
            },
            nucleus: None,
            subscript: None,
            superscript: None,
        };
        let bin = NormalNoad {
            noad_type: NoadType::Bin,
            nucleus: None,
            subscript: None,
            superscript: None,
        };
        let rel = NormalNoad {
            noad_type: NoadType::Rel,
            nucleus: None,
            subscript: None,
            superscript: None,
        };
        let open = NormalNoad {
            noad_type: NoadType::Open,
            nucleus: None,
            subscript: None,
            superscript: None,
        };
        let close = NormalNoad {
            noad_type: NoadType::Close,
            nucleus: None,
            subscript: None,
            superscript: None,
        };
        let punct = NormalNoad {
            noad_type: NoadType::Punct,
            nucleus: None,
            subscript: None,
            superscript: None,
        };
        let inner = NormalNoad {
            noad_type: NoadType::Inner,
            nucleus: None,
            subscript: None,
            superscript: None,
        };
        let mut file = Vec::new();
        ord.dump(&mut file).unwrap();
        op.dump(&mut file).unwrap();
        bin.dump(&mut file).unwrap();
        rel.dump(&mut file).unwrap();
        open.dump(&mut file).unwrap();
        close.dump(&mut file).unwrap();
        punct.dump(&mut file).unwrap();
        inner.dump(&mut file).unwrap();

        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let ord_undumped = NormalNoad::undump(&mut lines).unwrap();
        let op_undumped = NormalNoad::undump(&mut lines).unwrap();
        let bin_undumped = NormalNoad::undump(&mut lines).unwrap();
        let rel_undumped = NormalNoad::undump(&mut lines).unwrap();
        let open_undumped = NormalNoad::undump(&mut lines).unwrap();
        let close_undumped = NormalNoad::undump(&mut lines).unwrap();
        let punct_undumped = NormalNoad::undump(&mut lines).unwrap();
        let inner_undumped = NormalNoad::undump(&mut lines).unwrap();
        assert_eq!(ord, ord_undumped);
        assert_eq!(op, op_undumped);
        assert_eq!(bin, bin_undumped);
        assert_eq!(rel, rel_undumped);
        assert_eq!(open, open_undumped);
        assert_eq!(close, close_undumped);
        assert_eq!(punct, punct_undumped);
        assert_eq!(inner, inner_undumped);
    }

    #[test]
    fn dump_radical_noad() {
        let radical_noad = RadicalNoad {
            left_delimiter: DelimiterField {
                small_fam: 1,
                small_char: 2,
                large_fam: 3,
                large_char: 4,
            },
            nucleus: None,
            subscript: None,
            superscript: None,
        };
        let mut file = Vec::new();
        radical_noad.dump(&mut file).unwrap();

        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let radical_noad_undumped = RadicalNoad::undump(&mut lines).unwrap();
        assert_eq!(radical_noad, radical_noad_undumped);
    }

    #[test]
    fn dump_fraction_noad() {
        let fraction_noad = FractionNoad {
            left_delimiter: DelimiterField {
                small_fam: 1,
                small_char: 2,
                large_fam: 3,
                large_char: 4,
            },
            right_delimiter: DelimiterField {
                small_fam: 1,
                small_char: 2,
                large_fam: 3,
                large_char: 4,
            },
            thickness: 1234,
            numerator: Vec::new(),
            denominator: Vec::new(),
        };
        let mut file = Vec::new();
        fraction_noad.dump(&mut file).unwrap();

        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let fraction_noad_undumped = FractionNoad::undump(&mut lines).unwrap();
        assert_eq!(fraction_noad, fraction_noad_undumped);
    }

    #[test]
    fn dump_under_noad() {
        let under_noad = UnderNoad {
            nucleus: None,
            subscript: None,
            superscript: None,
        };
        let mut file = Vec::new();
        under_noad.dump(&mut file).unwrap();

        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let under_noad_undumped = UnderNoad::undump(&mut lines).unwrap();
        assert_eq!(under_noad, under_noad_undumped);
    }

    #[test]
    fn dump_over_noad() {
        let over_noad = OverNoad {
            nucleus: None,
            subscript: None,
            superscript: None,
        };
        let mut file = Vec::new();
        over_noad.dump(&mut file).unwrap();

        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let over_noad_undumped = OverNoad::undump(&mut lines).unwrap();
        assert_eq!(over_noad, over_noad_undumped);
    }

    #[test]
    fn dump_accent_noad() {
        let accent_noad = AccentNoad {
            accent_fam: 2,
            accent_char: 3,
            nucleus: None,
            subscript: None,
            superscript: None,
        };
        let mut file = Vec::new();
        accent_noad.dump(&mut file).unwrap();

        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let accent_noad_undumped = AccentNoad::undump(&mut lines).unwrap();
        assert_eq!(accent_noad, accent_noad_undumped);
    }

    #[test]
    fn dump_vcenter_noad() {
        let vcenter_noad = VcenterNoad {
            nucleus: ListNode::new_empty_hbox(),
            subscript: None,
            superscript: None,
        };
        let mut file = Vec::new();
        vcenter_noad.dump(&mut file).unwrap();

        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let vcenter_noad_undumped = VcenterNoad::undump(&mut lines).unwrap();
        assert_eq!(vcenter_noad, vcenter_noad_undumped);
    }

    #[test]
    fn dump_left_noad() {
        let left_noad = LeftNoad {
            delimiter: DelimiterField {
                small_fam: 1,
                small_char: 2,
                large_fam: 3,
                large_char: 4,
            },
        };
        let mut file = Vec::new();
        left_noad.dump(&mut file).unwrap();

        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let left_noad_undumped = LeftNoad::undump(&mut lines).unwrap();
        assert_eq!(left_noad, left_noad_undumped);
    }

    #[test]
    fn dump_right_noad() {
        let right_noad = RightNoad {
            delimiter: DelimiterField {
                small_fam: 1,
                small_char: 2,
                large_fam: 3,
                large_char: 4,
            },
        };
        let mut file = Vec::new();
        right_noad.dump(&mut file).unwrap();

        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let right_noad_undumped = RightNoad::undump(&mut lines).unwrap();
        assert_eq!(right_noad, right_noad_undumped);
    }

    #[test]
    fn dump_style_node() {
        let style_node = StyleNode {
            style: MathStyle::Display { cramped: false },
        };
        let mut file = Vec::new();
        style_node.dump(&mut file).unwrap();

        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let style_node_undumped = StyleNode::undump(&mut lines).unwrap();
        assert_eq!(style_node, style_node_undumped);
    }

    #[test]
    fn dump_choice_node() {
        let choice_node = ChoiceNode {
            display_mlist: Box::new(Vec::new()),
            text_mlist: Box::new(Vec::new()),
            script_mlist: Box::new(Vec::new()),
            script_script_mlist: Box::new(Vec::new()),
        };
        let mut file = Vec::new();
        choice_node.dump(&mut file).unwrap();

        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let choice_node_undumped = ChoiceNode::undump(&mut lines).unwrap();
        assert_eq!(choice_node, choice_node_undumped);
    }

    #[test]
    fn dump_noad_field() {
        let math_char = NoadField::MathChar {
            fam: 0,
            character: 1,
        };
        let math_text_char = NoadField::MathTextChar {
            fam: 0,
            character: 1,
        };
        let sub_box = NoadField::SubBox {
            list_node: ListNode::new_empty_hbox(),
        };
        let sub_mlist = NoadField::SubMlist { list: Vec::new() };
        let mut file = Vec::new();
        math_char.dump(&mut file).unwrap();
        math_text_char.dump(&mut file).unwrap();
        sub_box.dump(&mut file).unwrap();
        sub_mlist.dump(&mut file).unwrap();

        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let math_char_undumped = NoadField::undump(&mut lines).unwrap();
        let math_text_char_undumped = NoadField::undump(&mut lines).unwrap();
        let sub_box_undumped = NoadField::undump(&mut lines).unwrap();
        let sub_mlist_undumped = NoadField::undump(&mut lines).unwrap();
        assert_eq!(math_char, math_char_undumped);
        assert_eq!(math_text_char, math_text_char_undumped);
        assert_eq!(sub_box, sub_box_undumped);
        assert_eq!(sub_mlist, sub_mlist_undumped);
    }

    #[test]
    fn dump_delimiter_field() {
        let delimiter_field = DelimiterField {
            small_fam: 1,
            small_char: 2,
            large_fam: 3,
            large_char: 4,
        };
        let mut file = Vec::new();
        delimiter_field.dump(&mut file).unwrap();
        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let delimiter_field_undumped = DelimiterField::undump(&mut lines).unwrap();
        assert_eq!(delimiter_field, delimiter_field_undumped);
    }

    #[test]
    fn dump_math_style() {
        let display = MathStyle::Display { cramped: false };
        let text = MathStyle::Text { cramped: true };
        let script = MathStyle::Script { cramped: true };
        let script_script = MathStyle::ScriptScript { cramped: true };
        let mut file = Vec::new();
        display.dump(&mut file).unwrap();
        text.dump(&mut file).unwrap();
        script.dump(&mut file).unwrap();
        script_script.dump(&mut file).unwrap();

        let input = String::from_utf8(file).unwrap();
        let mut lines = input.lines();
        let display_undumped = MathStyle::undump(&mut lines).unwrap();
        let text_undumped = MathStyle::undump(&mut lines).unwrap();
        let script_undumped = MathStyle::undump(&mut lines).unwrap();
        let script_script_undumped = MathStyle::undump(&mut lines).unwrap();
        assert_eq!(display, display_undumped);
        assert_eq!(text, text_undumped);
        assert_eq!(script, script_undumped);
        assert_eq!(script_script, script_script_undumped);
    }
}
