mod value;
mod reader;
mod printer;
mod expand;
mod environment;
mod interpreter;
mod eval;
mod builtins;

extern crate copperline;

use std::rc::Rc;
use copperline::Copperline;
use reader::Parser;
use interpreter::Interpreter;

// Main

fn main () {
    let mut cpl = Copperline::new();
    let mut itp = Interpreter::new();
    
    loop {
        let mod_name = itp.current_mod_name().unwrap_or_else(|| "???".to_string());
        match cpl.read_line_utf8(&format!("ctr@{}> ", mod_name)) {
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
