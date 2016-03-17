use std::rc::Rc;

mod value;
mod vm;
mod environment;
mod bytecode;
mod builtins;

use value::{Value, Closure, CodeObject, NativeFn};
use bytecode::{bc, CLOSED, CONSTANT, CALL};
use vm::VMProcess;

fn main() {
    let addition = Value::Fn(Closure {
        codeobj: Rc::new(CodeObject {
            instrs: vec![bc(CLOSED, 0),   // +
                         bc(CLOSED, 1),   // halt
                         bc(CONSTANT, 0), // 3
                         bc(CONSTANT, 1), // 4
                         bc(CALL, 3)],    // (+ halt 3 4)
            consts: vec![Rc::new(Value::Int(3)), Rc::new(Value::Int(4))],
            codeobjs: vec![],
        }),
        clovers: vec![Rc::new(Value::NativeFn(NativeFn {
            code: builtins::add_2i
        })),
        Rc::new(Value::NativeFn(NativeFn {
            code: builtins::halt
        }))]
    });
    VMProcess::new(Rc::new(addition), vec![]).run();
}
