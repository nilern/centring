#![feature(alloc, heap_api)]

extern crate alloc;
extern crate rustyline;

pub mod ops;
pub mod interpreter;
pub mod gc;
pub mod read;
pub mod value;
pub mod refs;
pub mod write;

use interpreter::Interpreter;
use write::ContextValue;

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
                    Ok(v) =>
                        println!("{}", ContextValue::new(v.borrow(), &itp)),
                    Err(_) =>
                        println!("ReadError!")
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
