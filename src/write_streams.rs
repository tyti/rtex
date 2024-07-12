use crate::command::UnexpandableCommand;
use crate::eqtb::Eqtb;
use crate::input::expansion::get_x_token;
use crate::input::Scanner;
use crate::integer::{Integer, IntegerExt};
use crate::logger::Logger;
use crate::nodes::{CloseNode, Node, OpenNode, WhatsitNode, WriteNode};
use crate::output::{open_write_file, write_out, Output};
use crate::semantic_nest::SemanticState;
use crate::token::Token;

use std::path::PathBuf;

/// See 1348.
pub fn do_open_out(
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let node = Node::Whatsit(WhatsitNode::Open(implement_openout(scanner, eqtb, logger)));
    nest.tail_push(node, eqtb);
}

/// See 1348.
pub fn do_write(
    token: Token,
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let node = Node::Whatsit(WhatsitNode::Write(implement_write(
        token, scanner, eqtb, logger,
    )));
    nest.tail_push(node, eqtb);
}

/// See 1348.
pub fn do_close_out(
    nest: &mut SemanticState,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let node = Node::Whatsit(WhatsitNode::Close(implement_closeout(
        scanner, eqtb, logger,
    )));
    nest.tail_push(node, eqtb);
}

/// See 1351.
fn implement_openout(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) -> OpenNode {
    let stream_number = Integer::scan_four_bit_int(scanner, eqtb, logger);

    scanner.scan_optional_equals(eqtb, logger);
    let file_name = scanner.scan_file_name(eqtb, logger);
    let path = PathBuf::from(std::str::from_utf8(&file_name).unwrap());
    OpenNode {
        write_stream: stream_number as usize,
        path,
    }
}

/// See 1352.
fn implement_write(
    token: Token,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) -> WriteNode {
    let Token::CSToken { cs } = token else {
        panic!("Impossible")
    };

    let mut stream_number = Integer::scan_int(scanner, eqtb, logger);
    if stream_number < 0 {
        stream_number = 17;
    } else if stream_number > 15 {
        stream_number = 16;
    }
    let token_list = scanner.scan_toks(cs, false, eqtb, logger);

    WriteNode {
        write_stream: stream_number as usize,
        tokens: std::rc::Rc::new(token_list),
    }
}

/// See 1353.
fn implement_closeout(scanner: &mut Scanner, eqtb: &mut Eqtb, logger: &mut Logger) -> CloseNode {
    let mut stream_number = Integer::scan_int(scanner, eqtb, logger);
    if stream_number < 0 {
        stream_number = 17;
    } else if stream_number > 15 {
        stream_number = 16;
    }
    CloseNode {
        write_stream: stream_number as usize,
    }
}

/// See 1375.
pub fn implement_immediate(
    output: &mut Output,
    scanner: &mut Scanner,
    eqtb: &mut Eqtb,
    logger: &mut Logger,
) {
    let (unexpandable_command, token) = get_x_token(scanner, eqtb, logger);
    match unexpandable_command {
        UnexpandableCommand::OpenOut => {
            let open_node = implement_openout(scanner, eqtb, logger);
            open_write_file(&open_node, output, scanner, eqtb, logger)
        }
        UnexpandableCommand::Write => {
            let write_node = implement_write(token, scanner, eqtb, logger);
            write_out(&write_node, output, scanner, eqtb, logger)
        }
        UnexpandableCommand::CloseOut => {
            let close_node = implement_closeout(scanner, eqtb, logger);
            if close_node.write_stream < 16 {
                output.write_files[close_node.write_stream] = None;
            }
        }
        _ => scanner.back_input(token, eqtb, logger),
    }
}
