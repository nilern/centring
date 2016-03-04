use std::rc::Rc;
use value::{Value, ValueRef, Interpreter};
use reader::{Parser};

mod value;
mod reader;
mod printer;

// Eval

impl Interpreter {
    fn new() -> Interpreter {
        Interpreter
    }
    
    fn eval(&mut self, expr: Value) -> ValueRef {
        Rc::new(expr)
    }
}

// Main

fn main () {
    println!("{}",
             Parser::new("#(\\a \\ä \\newline)").parse_expr().unwrap().0);
}
