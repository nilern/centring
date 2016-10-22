#![feature(alloc, heap_api, ptr_eq)]

extern crate alloc;
extern crate rustyline;

pub mod ops;
pub mod interpreter;
pub mod gc;
pub mod read;
pub mod value;
pub mod refs;
pub mod write;
pub mod primops;
pub mod analyze;

use interpreter::Interpreter;
use write::ContextValue;
use analyze::{analyze, ast_to_sexpr};

use rustyline::error::ReadlineError;
use rustyline::Editor;

fn main() {
    let mut ed = Editor::<()>::new();
    let mut itp = Interpreter::new();
    loop {
        let line = ed.readline("ctr> ");
        match line {
            Ok(line) => {
                let mut st = read::ParseState::new(line);
                match read::read(&mut itp, &mut st) {
                    Ok(Some(v)) => {
                        if let Ok(sexp) = analyze(&mut itp, v.borrow())
                                              .and_then(|ast|
                                                  ast_to_sexpr(&mut itp, ast.borrow())) {
                            println!("{}", ContextValue::new(sexp.borrow(), &itp));
                        } else {
                            unimplemented!();
                        }
                    }
                    Ok(None) => { }
                    Err(e) => println!("ReadError: {:?}", e)
                }
            },
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
                break;
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}
