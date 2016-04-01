// use std::rc::Rc;

// mod value;
// mod vm;
// mod environment;
// mod bytecode;
// mod builtins;

// use value::{Value, Closure, NativeFn};
// use bytecode::Bytecode::{Arg, Const, Closed, Brf, Fn, Call};
// use vm::{VM, CodeObject};

use std::mem::transmute;

mod gc;
mod bytecode;
mod vm;

use gc::{Value, DeflatedProcedure};
use bytecode::Bytecode;
use vm::VM;

fn main() {
    // let mut heap = GcHeap::with_capacity(256);

    // let nref = heap.alloc(Value::Int(-3));
    // let mref = heap.alloc(Value::Int(5));
    // let tupref1 = heap.alloc(Value::Tuple(&[nref, mref]));
    // let tupref2 = heap.alloc(Value::Tuple(&[mref, nref, tupref1]));
    // let bref = heap.alloc(Value::Buffer(&[1,2,3,4,5,6,7,8,9,10]));

    // let mut roots = [tupref2, tupref1, bref];
    // heap = heap.collect(&mut roots);

    // for rootref in &roots {
    //     if let Value::Tuple(vrefs) = heap.deref(*rootref) {
    //         for vref in vrefs {
    //             println!("{:?}", heap.deref(*vref));
    //         }
    //     } else {
    //         println!("{:?}", heap.deref(*rootref));
    //     }
    // }
    
    // println!("{:?}", heap);

    let addition = DeflatedProcedure {
        instrs: vec![Bytecode::cnst(0),     // 2
                     Bytecode::cnst(1),     // 3
                     Bytecode::addi(0, 1),  // 2 + 3 = 5
                     Bytecode::cnst(2),     // 6
                     Bytecode::subi(3, 0),  // 6 - 2 = 4
                     Bytecode::muli(2, 4),  // 5*4 = 20
                     Bytecode::subi(0, 1),  // 2 - 3 = -1
                     Bytecode::divi(5, 6)], // 20/-1 = -20
        consts: vec![Value::Int(2), Value::Int(3), Value::Int(6)],
        codeobjs: vec![],
        clover_count: 0
    };

    let vm = VM::new();
    let mut vmproc = vm.spawn(&addition); // 'inflates' the bytecode
    println!("\n{:?}isize", vmproc.run().unwrap().get_int().unwrap());

    // unsafe {
    //     println!("\n{:?}", transmute::<[u32; 2], [u8; 8]>([0xFFFF_FFFF, 0]));
    // }
    
    // let mull = Rc::new(CodeObject {
    //     instrs: vec![Closed(0), // *
    //                  Closed(1), // k
    //                  Closed(2), // n
    //                  Arg(0),    // m
    //                  Call(3)    // (* k n m)
    //     ],
    //     consts: vec![],
    //     codeobjs: vec![]
    // });

    // let recur = Rc::new(CodeObject {
    //     instrs: vec![Closed(0), // fact
            
    //                  Closed(1), // *
    //                  Closed(2), // k
    //                  Closed(3), // n
    //                  Fn(0, 3),  // mull
                     
    //                  Arg(0),    // m
    //                  Call(2)    // (fact mull m)
    //     ],
    //     consts: vec![],
    //     codeobjs: vec![mull]
    // });
    
    // let branch = Rc::new(CodeObject {
    //     instrs: vec![Arg(0),    // c
    //                  Brf(3),    // if c
    //                  Const(0),  // 1
    //                  Closed(0), // -

    //                  Closed(1), // fact
    //                  Closed(2), // *
    //                  Closed(3), // k
    //                  Closed(4), // n
    //                  Fn(0, 4),  // recur
                     
    //                  Closed(3), // n
    //                  Const(0),  // 1
    //                  Call(3)    // (- mull n 1)
    //     ],
    //     consts: vec![Rc::new(Value::Int(1))],
    //     codeobjs: vec![recur]
    // });
    
    // let fact = Rc::new(CodeObject {
    //     instrs: vec![Closed(0), // <
                     
    //                  Closed(1), // -
    //                  Closed(2), // fact
    //                  Closed(3), // *
    //                  Arg(0),    // k
    //                  Arg(1),    // n
    //                  Fn(0, 4),  // branch
                     
    //                  Arg(1),    // n
    //                  Const(0),  // 1
    //                  Call(3)    // (< branch n 1)
    //     ],
    //     consts: vec![Rc::new(Value::Int(1))],
    //     codeobjs: vec![branch]
    // });

    // let main = Rc::new(Value::Fn(Closure {
    //     codeobj: Rc::new(CodeObject {
    //         instrs: vec![Closed(0), // <
    //                      Closed(1), // -
    //                      Closed(2), // *
    //                      Fn(0, 3),  // fact
    //                      Closed(3), // halt
    //                      Const(0),  // 5
    //                      Call(2)    // (fact halt 5)
    //         ],
    //         consts: vec![Rc::new(Value::Int(5))],
    //         codeobjs: vec![fact]
    //     }),
    //     clovers: Rc::new(vec![
    //         Rc::new(Value::NativeFn(NativeFn { code: builtins::lt_2i })),
    //         Rc::new(Value::NativeFn(NativeFn { code: builtins::sub_2i })),
    //         Rc::new(Value::NativeFn(NativeFn { code: builtins::mul_2i })),
    //         Rc::new(Value::NativeFn(NativeFn { code: builtins::halt }))
    //     ])
    // }));

    // // Things are broken here because factorial should somehow be able to call
    // // itself. 
            
    // println!("\n{:?}", VM::new().spawn(main, vec![]).run());
}
