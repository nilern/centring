use gc::ValueRef;
use bytecode::Bytecode;
use bytecode::Bytecode::{Const, Local, AddI, SubI, MulI, DivI};

// Types

pub struct VM;

pub struct VMProcess<'a> {
    ip: usize,

    instrs: &'a [Bytecode],
    consts: Vec<ValueRef>,
    // codeobjs: &'a [ValueRef],
    // clovers: Vec<ValueRef>,

    stack: Vec<ValueRef>
}

#[derive(Debug)]
pub enum VMError {
    ArgcMismatch(usize, usize),
    TypeMismatch // TODO: include the types
}

pub type VMResult = Result<ValueRef, VMError>;

pub struct DeflatedProcedure {
    pub instrs: Vec<Bytecode>,
    pub consts: Vec<DeflatedValue>,
    pub codeobjs: Vec<DeflatedProcedure>,
    pub clover_count: usize
}

pub enum DeflatedValue {
    Int(isize)
}

// Behaviour

impl VM {
    pub fn new() -> VM {
        VM
    }

    pub fn spawn<'a>(&'a self, inflatee: &'a DeflatedProcedure) -> VMProcess {
        VMProcess {
            ip: 0,
            instrs: inflatee.instrs.as_slice(),
            consts: inflatee.consts.iter().map(DeflatedValue::inflate).collect(),
            stack: vec![]
        }
    }
}

impl<'a> VMProcess<'a> {
    pub fn run(&mut self) -> VMResult {
        while self.ip < self.instrs.len() {
            match self.instrs[self.ip] {
                Const(i) => {
                    println!("const  {}   {:?}", i, self.stack);
                    
                    self.stack.push(self.consts[i as usize].clone());
                },
                Local(i) => {
                    println!("local  {}   {:?}", i, self.stack);

                    let vref = self.stack[i as usize];
                    self.stack.push(vref);
                },

                AddI(i, j) => {
                    println!("addi   {} {} {:?}", i, j, self.stack);

                    let a = self.stack[i as usize];
                    let b = self.stack[j as usize];
                    self.stack.push(try!(a.addi(b)));
                },

                SubI(i, j) => {
                    println!("subi   {} {} {:?}", i, j, self.stack);

                    let a = self.stack[i as usize];
                    let b = self.stack[j as usize];
                    self.stack.push(try!(a.subi(b)));
                },

                MulI(i, j) => {
                    println!("muli   {} {} {:?}", i, j, self.stack);

                    let a = self.stack[i as usize];
                    let b = self.stack[j as usize];
                    self.stack.push(try!(a.muli(b)));
                },

                DivI(i, j) => {
                    println!("divi   {} {} {:?}", i, j, self.stack);

                    let a = self.stack[i as usize];
                    let b = self.stack[j as usize];
                    self.stack.push(try!(a.divi(b)));
                },
            }
            self.ip += 1;
        }
        
        Ok(self.stack.pop().unwrap())
    }
}

impl DeflatedValue {
    fn inflate(&self) -> ValueRef {
        match *self {
            DeflatedValue::Int(i) => ValueRef::new((i << 1) as usize | 1)
        }
    }
}

// use std::rc::Rc;
// use std::collections::HashMap;

// use value::{Value, ValueRef, Closure, NativeFn};
// use environment::EnvRef;
// use bytecode::{Bytecode, Arg, Const, Closed, Brf, Call, Fn};

// // Types

// pub struct VM {
//     modules: HashMap<String, EnvRef>
// }

// pub struct VMProcess {
//     ip: usize,
    
//     codeobj: Rc<CodeObject>,
//     clovers: Rc<Vec<ValueRef>>,
    
//     stack: Vec<ValueRef>
// }

// #[derive(Debug)]
// pub struct CodeObject {
//     pub instrs: Vec<Bytecode>,
//     pub consts: Vec<ValueRef>,
//     pub codeobjs: Vec<Rc<CodeObject>>
// }

// #[derive(Debug)]
// pub enum VMError {
//     ArgcMismatch(usize, usize)
// }

// pub type VMResult = Result<ValueRef, VMError>;

// // Behaviour

// impl VM {
//     pub fn new() -> VM {
//         VM {
//             modules: HashMap::new()
//         }
//     }

//     pub fn spawn(&self, closure: ValueRef, args: Vec<ValueRef>) -> VMProcess {
//         VMProcess {
//             ip: 0,
//             codeobj: closure.unwrap_codeobj().clone(),
//             clovers: closure.unwrap_clovers().clone(),
//             stack: args
//         }
//     }
// }

// impl VMProcess {
//     pub fn run(&mut self) -> VMResult {
//         while self.ip < self.codeobj.instrs.len() {
//             match self.codeobj.instrs[self.ip] {
//                 Arg(n) => {
//                     println!("arg    {} {:?}", n, self.stack);

//                     let arg = self.arg(n);
//                     self.stack.push(arg);
//                     self.ip += 1;
//                 },
//                 Const(n) => {
//                     println!("const  {} {:?}", n, self.stack);
                    
//                     self.stack.push(self.codeobj.consts[n].clone());
//                     self.ip += 1;
//                 },
//                 Closed(n) => {
//                     println!("closed {} {:?}", n, self.stack);
                    
//                     self.stack.push(self.clovers[n].clone());
//                     self.ip += 1;
//                 },

//                 Brf(n) => {
//                     println!("brf    {} {:?}", n, self.stack);
                    
//                     let cond = self.stack.pop().unwrap().unwrap_bool();
//                     if !cond {
//                         self.ip = n;
//                     } else {
//                         self.ip += 1;
//                     }
//                 },

//                 Fn(i, n) => {
//                     println!("fn     {} {} {:?}", i, n, self.stack);
                    
//                     let codeobj = self.codeobj.codeobjs[i].clone();
//                     let l = self.stack.len();
//                     let clovers = Rc::new(self.stack.split_off(l - n));
//                     let closure = Rc::new(Value::Fn(Closure {
//                         codeobj: codeobj,
//                         clovers: clovers
//                     }));
//                     self.stack.push(closure);
//                     self.ip += 1;
//                 },

//                 Call(n) => {
//                     println!("call   {} {:?}", n, self.stack);
                    
//                     try!(self.call(n));
//                 }
//             }
//         }
//         Ok(self.stack.pop().unwrap())
//     }

//     pub fn arg(&self, n: usize) -> ValueRef {
//         self.stack[n].clone()
//     }

//     pub fn push(&mut self, v: ValueRef) {
//         self.stack.push(v)
//     }

//     pub fn call(&mut self, n: usize) -> VMResult {
//         let l = self.stack.len();
//         let start = l - n;
//         let f = self.stack[l - n - 1].clone();
//         for i in 0..n {
//             self.stack[i] = self.stack[start + i].clone()
//         }
//         self.stack.truncate(n);
        
//         match *f {
//             Value::Fn(Closure { ref codeobj, ref clovers }) => {
//                 self.ip = 0;
//                 self.codeobj = codeobj.clone();
//                 self.clovers = clovers.clone();
//             },
//             Value::NativeFn(NativeFn { ref code }) => return code(self),
//             _ => panic!()
//         }
//         Ok(Rc::new(Value::Tuple(vec![])))
//     }

//     pub fn halt(&mut self) {
//         self.ip = self.codeobj.instrs.len()
//     }
// }
