mod value;
mod reader;
mod printer;
mod eval;

use std::rc::Rc;
use reader::Parser;
use eval::{Interpreter, analyze};

// Main

fn main () {
    let mut itp = Interpreter::new();

    println!("{}",
             itp.eval(
                 &analyze(
                     Rc::new(Parser::new(
                         "(centring.lang/do (centring.lang/def foo 5) foo)")
                             .parse_expr().unwrap().0))));
}
