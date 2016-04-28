mod gc;
mod hashtable;
mod bytecode;
mod vm;

use gc::{Value, DeflatedProcedure};
use bytecode::{load, iadd, isub, imul, idiv, fun, call, halt, l, f, c};
use vm::VM;

fn main() {
    // (let ((x (* (+ 2 3) (- 6 2)))
    //       (y (- 2 3)))
    //   ((fn (z) (/ z y)) x))
    let division = DeflatedProcedure {
        instrs: vec![idiv(l(1), f(0)), // args[0] / clovers[0]
                     halt(l(2))],
        consts: vec![],
        codeobjs: vec![],
        clover_count: 1
    };
    let arith = DeflatedProcedure {
        instrs: vec![iadd(c(0), c(1)), // 2 + 3 = 5
                     isub(c(2), c(0)), // 6 - 2 = 4
                     imul(l(1), l(2)), // 5*4 = 20
                     isub(c(0), c(1)), // 2 - 3 = -1
                     load(l(4)),       // -1
                     fun(0),           // closure(division, -1)
                     load(l(3)),       // 20
                     call(5)],         // division(20) = -20
        consts: vec![Value::Int(2), Value::Int(3), Value::Int(6)],
        codeobjs: vec![division],
        clover_count: 0
    };

    let vm = VM::new();
    let mut vmproc = vm.spawn(&arith); // 'inflates' the bytecode
    println!("{:?}", vmproc.run().unwrap().get_int().unwrap());
}
