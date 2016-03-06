mod value;
mod reader;
mod printer;
mod eval;

extern crate copperline;

use std::rc::Rc;
use copperline::Copperline;
use reader::Parser;
use eval::{Interpreter, analyze};

// Main

fn main () {
    let mut cpl = Copperline::new();
    let mut itp = Interpreter::new();

    loop {
        match cpl.read_line_utf8("ctr> ") {
            Ok(input) => {
                println!("{}",
                         itp.eval(
                             &analyze(
                                 &Rc::new(Parser::new(&input)
                                          .parse_expr().unwrap().0))));
                cpl.add_history(input);
            },
            Err(copperline::Error::EndOfFile) => break,
            Err(err) => println!("; {}", err)
        }
    }
}
