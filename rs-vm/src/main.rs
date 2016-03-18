use std::rc::Rc;

mod value;
mod vm;
mod environment;
mod bytecode;
mod builtins;

use value::{Value, Closure, NativeFn};
use bytecode::Bytecode::{Arg, Const, Closed, Brf, Fn, Call};
use vm::{VM, CodeObject};

fn main() {
    let mull = Rc::new(CodeObject {
        instrs: vec![Closed(0), // *
                     Closed(1), // k
                     Closed(2), // n
                     Arg(0),    // m
                     Call(3)    // (* k n m)
        ],
        consts: vec![],
        codeobjs: vec![]
    });

    let recur = Rc::new(CodeObject {
        instrs: vec![Closed(0), // fact
            
                     Closed(1), // *
                     Closed(2), // k
                     Closed(3), // n
                     Fn(0, 3),  // mull
                     
                     Arg(0),    // m
                     Call(2)    // (fact mull m)
        ],
        consts: vec![],
        codeobjs: Vec![mull]
    });
    
    let branch = Rc::new(CodeObject {
        instrs: vec![Arg(0),    // c
                     Brf(3),    // if c
                     Const(0),  // 1
                     Closed(0), // -

                     Closed(1), // fact
                     Closed(2), // *
                     Closed(3), // k
                     Closed(4), // n
                     Fn(0, 4)   // recur
                     
                     Closed(3), // n
                     Const(0),  // 1
                     Call(3)    // (- mull n 1)
        ],
        consts: vec![Rc::new(Value::Int(1))],
        codeobjs: vec![recur]
    });
    
    let fact = Rc::new(CodeObject {
        instrs: vec![Closed(0), // <
                     
                     Closed(1), // -
                     Closed(2), // fact
                     Closed(3), // *
                     Arg(0),    // k
                     Arg(1),    // n
                     Fn(0, 4),  // branch
                     
                     Arg(1),    // n
                     Const(0),  // 1
                     Call(3)    // (< branch n 1)
        ],
        consts: vec![Rc::new(Value::Int(1))],
        codeobjs: vec![branch]
    });

    let main = Rc::new(Value::Fn(Closure {
        codeobj: Rc::new(CodeObject {
            instrs: vec![Closed(0), // <
                         Closed(1), // -
                         Closed(2), // *
                         Fn(0, 3),  // fact
                         Closed(3), // halt
                         Const(0),  // 5
                         Call(2)    // (fact halt 5)
            ],
            consts: vec![Rc::new(Value::Int(5))],
            codeobjs: vec![fact]
        }),
        clovers: Rc::new(vec![
            Rc::new(Value::NativeFn(NativeFn { code: builtins::lt_2i })),
            Rc::new(Value::NativeFn(NativeFn { code: builtins::sub_2i })),
            Rc::new(Value::NativeFn(NativeFn { code: builtins::mul_2i })),
            Rc::new(Value::NativeFn(NativeFn { code: builtins::halt }))
        ])
    }));

    // Things are broken here because factorial should somehow be able to call
    // itself. 
            
    println!("\n{:?}", VM::new().spawn(main, vec![]).run());
}
