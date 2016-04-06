#![feature(plugin)]
#![plugin(peg_syntax_ext)]

mod gc;
mod bytecode;
mod vm;
mod compiler;
peg_file! grammar("grammar.rustpeg");

// use gc::{Value, DeflatedProcedure};
// use bytecode::{local, add, sub, mul, div, fun, call, halt, l, f, c};
// use vm::VM;
use grammar::expr;
use compiler::cps;

fn main() {
    println!("{:?}", cps(expr("foo").unwrap()));
    
    // // (let ((x (* (+ 2 3) (- 6 2)))
    // //       (y (- 2 3)))
    // //   ((fn (z) (/ z y)) x))
    // let division = DeflatedProcedure {
    //     instrs: vec![divi(l(1), f(0))], // args[0] / clovers[0]
    //     consts: vec![],
    //     codeobjs: vec![],
    //     clover_count: 1
    // };
    // let arith = DeflatedProcedure {
    //     instrs: vec![addi(c(0), c(1)), // 2 + 3 = 5
    //                  subi(c(2), c(0)), // 6 - 2 = 4
    //                  muli(l(0), l(1)), // 5*4 = 20
    //                  subi(c(0), c(1)), // 2 - 3 = -1
    //                  local(3),         // -1
    //                  fun(0),           // closure(division, -1)
    //                  local(2),         // 20
    //                  call(1)],         // division(20) = -20
    //     consts: vec![Value::Int(2), Value::Int(3), Value::Int(6)],
    //     codeobjs: vec![division],
    //     clover_count: 0
    // };

    // // (* (+ 2 3) (- 6 2))
    // let simp_arith = DeflatedProcedure {
    //     instrs: vec![add(c(0), c(1)), // 2 + 3 = 5
    //                  sub(c(2), c(0)), // 6 - 2 = 4
    //                  mul(l(0), l(1)), // 5*4 = 20
    //                  halt(l(2))],     // 20
    //     consts: vec![Value::Int(2), Value::Int(3), Value::Int(6)],
    //     codeobjs: vec![],
    //     clover_count: 0
    // };

    // let vm = VM::new();
    // let mut vmproc = vm.spawn(&simp_arith); // 'inflates' the bytecode
    // println!("{:?}", vmproc.run().unwrap().get_int().unwrap());

    // // (* (+ 2 3) (- 6 2))
    // let simp_arith = DeflatedProcedure {
    //     instrs: vec![add(c(0), c(1)), // 2 + 3 = 5
    //                  sub(c(2), c(0)), // 6 - 2 = 4
    //                  mul(l(0), l(1)), // 5*4 = 20
    //                  halt(l(2))],     // 20
    //     consts: vec![Value::Int(2), Value::Int(3), Value::Int(6)],
    //     codeobjs: vec![],
    //     clover_count: 0
    // };

    // let vm = VM::new();
    // let mut vmproc = vm.spawn(&simp_arith); // 'inflates' the bytecode
    // println!("{:?}", vmproc.run().unwrap().get_int().unwrap());
}
