mod gc;
mod bytecode;
mod vm;

use gc::{Value, DeflatedProcedure};
use bytecode::{cnst, local, clover, addi, subi, muli, divi, fun, call};
use vm::VM;

fn main() {
    let division = DeflatedProcedure {
        instrs: vec![clover(0),
                     divi(1, 2)],
        consts: vec![],
        codeobjs: vec![],
        clover_count: 1
    };
    let addition = DeflatedProcedure {
        instrs: vec![cnst(0),    // 2
                     cnst(1),    // 3
                     addi(0, 1), // 2 + 3 = 5
                     cnst(2),    // 6
                     subi(3, 0), // 6 - 2 = 4
                     muli(2, 4), // 5*4 = 20
                     subi(0, 1), // 2 - 3 = -1
                     fun(0),     // closure(division, -1)
                     local(5),   // 20
                     call(1)],   // division(20) = -20
        consts: vec![Value::Int(2), Value::Int(3), Value::Int(6)],
        codeobjs: vec![division],
        clover_count: 0
    };

    let vm = VM::new();
    let mut vmproc = vm.spawn(&addition); // 'inflates' the bytecode
    println!("\n{:?}", vmproc.run().unwrap().get_int().unwrap());
}
