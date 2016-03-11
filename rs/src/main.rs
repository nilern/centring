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
use value::Value;
use interpreter::Interpreter;

// Main

fn main () {
    let mut cpl = Copperline::new();
    let mut itp = Interpreter::new();

    itp.store_local("+", Rc::new(Value::NativeFn {
        name: "+".to_string(),
        formal_types: vec![],
        code: builtins::add_2i
    }));
    itp.store_local("prepend", Rc::new(Value::NativeFn {
        name: "prepend".to_string(),
        formal_types: vec![],
        code: builtins::prepend_ls
    }));
    itp.store_local("load", Rc::new(Value::NativeFn {
        name: "load".to_string(),
        formal_types: vec![],
        code: builtins::load
    }));
    
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
