#![feature(alloc, heap_api)]

extern crate alloc;
extern crate rustyline;

pub mod gc;
pub mod ops;
pub mod interpreter;
pub mod read;

use rustyline::error::ReadlineError;
use rustyline::Editor;

fn main() {
    let mut ed = Editor::<()>::new();
    loop {
        let line = ed.readline("ctr> ");
        match line {
            Ok(line) => {
                let mut st = read::ParseState::new(line);
                println!("{:?}", read::int(&mut st));
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
