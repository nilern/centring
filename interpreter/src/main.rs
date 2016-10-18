#![feature(alloc, heap_api)]

extern crate alloc;
extern crate rustyline;

pub mod gc;
pub mod ops;
pub mod interpreter;
pub mod read;
pub mod value;

use interpreter::Interpreter;
use value::{Unbox, Bits};

use std::cell::Cell;
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
                unsafe {
                    println!("{:?}", read::int(&mut itp, &mut st).map(|n|
                        (*(n.borrow().as_ptr() as *const Bits<isize>)).unbox()));
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
