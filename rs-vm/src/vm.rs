use std::mem::size_of;
use std::slice;

use gc::{GcHeap, Value, ValueRef, DeflatedProcedure};
use bytecode::{Bytecode, CONST, LOCAL, ADDI, SUBI, MULI, DIVI};

// Types

pub struct VM;

pub struct VMProcess<'a> {
    vm: &'a VM,           // The parent VM of this process
    heap: GcHeap,         // The GC'ed memory for this process
    stack: Vec<ValueRef>, // Workspace

    // These slices point into the GcHeap:
    instrs: &'a [Bytecode],      // Instructions of the current Fn
    consts: &'a [ValueRef],      // Constants of the current Fn
    // codeobjs: &'a [ValueRef], // Inner Procedures of the current Fn
    // clovers: &'a [ValueRef],  // Closed-over values of the current Fn
        
    ip: usize // Index of the next instruction to run
}

#[derive(Debug)]
pub enum VMError {
    ArgcMismatch(usize, usize),
    TypeMismatch // TODO: include the types
}

pub type VMResult = Result<ValueRef, VMError>;

// Behaviour

impl VM {
    pub fn new() -> VM {
        VM
    }

    pub fn spawn<'a>(&'a self, inflatee: &'a DeflatedProcedure) -> VMProcess {
        let mut vmproc = VMProcess {
            vm: self,
            heap: GcHeap::with_capacity(256),
            stack: Vec::with_capacity(256),
            instrs: &[],
            consts: &[],
            ip: 0
        };
        
        let ic = inflatee.instrs.len();
        let bc = ic * size_of::<Bytecode>();
        let instrref = vmproc.heap.alloc(&Value::Buffer(unsafe {
            slice::from_raw_parts(inflatee.instrs.as_ptr() as *const _, bc)
        }));

        let constrefs: Vec<ValueRef> = inflatee.consts.iter()
            .map(|v| vmproc.heap.alloc(v)).collect();
        let consttupref = vmproc.heap.alloc(&Value::Tuple(constrefs.as_slice()));
        
        vmproc.fetch_instrs(instrref);
        vmproc.fetch_consts(consttupref);
        vmproc
    }
}

impl<'a> VMProcess<'a> {
    pub fn run(&mut self) -> VMResult {
        while self.ip < self.instrs.len() {
            let instr = self.instrs[self.ip];
            match instr.op() {
                CONST => {
                    let i = instr.arg();
                    println!("const {}   {:?}", i, self.stack);
                    
                    self.stack.push(self.consts[i].clone());
                },
                LOCAL => {
                    let i = instr.arg();
                    println!("local {}   {:?}", i, self.stack);

                    let vref = self.stack[i];
                    self.stack.push(vref);
                },

                ADDI => {
                    let (i, j) = instr.args();
                    println!("addi  {} {} {:?}", i, j, self.stack);

                    let a = self.stack[i];
                    let b = self.stack[j];
                    self.stack.push(try!(a.addi(b)));
                },

                SUBI => {
                    let (i, j) = instr.args();
                    println!("subi  {} {} {:?}", i, j, self.stack);

                    let a = self.stack[i];
                    let b = self.stack[j];
                    self.stack.push(try!(a.subi(b)));
                },

                MULI => {
                    let (i, j) = instr.args();
                    println!("muli  {} {} {:?}", i, j, self.stack);

                    let a = self.stack[i];
                    let b = self.stack[j];
                    self.stack.push(try!(a.muli(b)));
                },

                DIVI => {
                    let (i, j) = instr.args();
                    println!("divi  {} {} {:?}", i, j, self.stack);

                    let a = self.stack[i];
                    let b = self.stack[j];
                    self.stack.push(try!(a.divi(b)));
                },

                op => panic!("Unrecognized bytecode {:x}", op)
            }
            self.ip += 1;
        }
        
        Ok(self.stack.pop().unwrap())
    }

    fn fetch_instrs(&mut self, bufref: ValueRef) {
        if let Value::Buffer(bufbytes) = self.heap.deref(bufref) {
            self.instrs = unsafe {
                slice::from_raw_parts(bufbytes.as_ptr() as *const _,
                                      bufbytes.len() / size_of::<Bytecode>())
            };
        } else {
            panic!();
        }
    }

    fn fetch_consts(&mut self, constsref: ValueRef) {
        if let Value::Tuple(const_ref_slice) = self.heap.deref(constsref) {
            self.consts = unsafe {
                slice::from_raw_parts(const_ref_slice.as_ptr(),
                                      const_ref_slice.len())
            };
        } else {
            panic!();
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
