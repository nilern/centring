#![feature(alloc, heap_api)]

extern crate alloc;

mod gc;
mod ops;
mod interpreter;

use std::mem::size_of;

use gc::GCState;
use interpreter::Interpreter;

fn main() {
    println!("hello!");
}
