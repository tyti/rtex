use crate::eqtb::Eqtb;
use crate::logger::Logger;
use crate::nodes::noads::{FractionNoad, Noad, RightNoad};
use crate::nodes::Node;
use crate::print::Printer;

#[derive(Debug)]
pub struct MathMode {
    pub list: Vec<Node>,
    pub display: bool,
    pub incompleat_noad: Option<FractionNoad>,
}

impl MathMode {
    /// See 214.
    pub fn append_node(&mut self, node: Node, eqtb: &mut Eqtb) {
        self.list.push(node);
        self.update_last_node_info(eqtb);
    }

    /// See 718.
    pub fn flush_math(&mut self, eqtb: &mut Eqtb) {
        self.list.clear();
        self.update_last_node_info(eqtb);
        self.incompleat_noad = None;
    }

    /// See 1184.
    pub fn fin_mlist(self, right_noad: Option<RightNoad>) -> Vec<Node> {
        let mut cur_list = self.list;
        if let Some(fraction_noad) = self.incompleat_noad {
            cur_list = MathMode::compleat_incompleat_noad(fraction_noad, cur_list, right_noad)
        } else if let Some(right_noad) = right_noad {
            cur_list.push(Node::Noad(Noad::Right(right_noad)));
        };
        cur_list
    }

    /// See 1185.
    fn compleat_incompleat_noad(
        mut fraction_noad: FractionNoad,
        cur_list: Vec<Node>,
        right_noad: Option<RightNoad>,
    ) -> Vec<Node> {
        fraction_noad.denominator = cur_list;
        if let Some(right_noad) = right_noad {
            if fraction_noad.numerator.is_empty() {
                panic!("right");
            }
            let inner_list = fraction_noad.numerator.split_off(1);
            let Node::Noad(Noad::Left(left_noad)) = fraction_noad.numerator.remove(0) else {
                panic!("right");
            };
            fraction_noad.numerator = inner_list;
            vec![
                Node::Noad(Noad::Left(left_noad)),
                Node::Noad(Noad::Fraction(fraction_noad)),
                Node::Noad(Noad::Right(right_noad)),
            ]
        } else {
            vec![Node::Noad(Noad::Fraction(fraction_noad))]
        }
    }

    /// See 219.
    pub fn show_fields(&self, eqtb: &Eqtb, logger: &mut Logger) {
        if let Some(fraction_noad) = &self.incompleat_noad {
            logger.print_str("this will begin denominator of:");
            logger.print_ln();

            let (max_depth, max_breadth) = eqtb.get_max_depth_and_breadth();

            fraction_noad.display_as_compleat_noad(max_depth, max_breadth, eqtb, logger);
            logger.print_ln();
        }
    }

    /// Updates the condensed info about the last node of the current list that we keep in the
    /// Eqtb.
    pub fn update_last_node_info(&mut self, eqtb: &mut Eqtb) {
        eqtb.update_last_node_info(self.list.last());
    }
}
