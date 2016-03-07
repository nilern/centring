mod value;
mod reader;
mod printer;
mod expand;
mod eval;

extern crate copperline;

use std::rc::Rc;
use copperline::Copperline;
use reader::Parser;
use eval::Interpreter;

// Main

fn main () {
    let mut cpl = Copperline::new();
    let mut itp = Interpreter::new();

    loop {
        match cpl.read_line_utf8("ctr> ") {
            Ok(input) => {
                let parsed = Rc::new(Parser::new(&input).parse_expr().unwrap().0);
                let expr = itp.expand_all(parsed);
                println!("{}", itp.eval(&expr));
                cpl.add_history(input);
            },
            Err(copperline::Error::EndOfFile) => break,
            Err(err) => println!("; {}", err)
        }
    }
}
