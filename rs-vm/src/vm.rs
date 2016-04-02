use std::mem::size_of;
use std::slice;

use gc::{GcHeap, Value, ValueRef, DeflatedProcedure, Closure};
use bytecode::{Bytecode, Opcode};

// TODO:
// - Make GC work
// - let stack grow somewhat before 'renormalizing' it

// Types

pub struct VM;

pub struct VMProcess<'a> {
    vm: &'a VM,           // The parent VM of this process
    heap: GcHeap,         // The GC'ed memory for this process
    stack: Vec<ValueRef>, // Workspace

    // These slices point into the GcHeap:
    instrs: &'a [Bytecode],   // Instructions of the current Fn
    consts: &'a [ValueRef],   // Constants of the current Fn
    codeobjs: &'a [ValueRef], // Inner Procedures of the current Fn
    clovers: &'a [ValueRef],  // Closed-over values of the current Fn
        
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
            codeobjs: &[],
            clovers: &[],
            ip: 0
        };

        // Fill the slice fields:
        let procref = vmproc.heap.inflate(inflatee);
        vmproc.fetch_proc(procref);
        
        vmproc
    }
}

impl<'a> VMProcess<'a> {
    pub fn run(&'a mut self) -> VMResult {
        while self.ip < self.instrs.len() {
            let instr = self.instrs[self.ip];
            self.ip += 1;
            match instr.op() {
                Opcode::Const => {
                    let i = instr.arg();
                    println!("const  {}   {:?}", i, self.stack);
                    
                    self.stack.push(self.consts[i].clone());
                },
                
                Opcode::Local => {
                    let i = instr.arg();
                    println!("local  {}   {:?}", i, self.stack);

                    let vref = self.stack[i];
                    self.stack.push(vref);
                },
                
                Opcode::Clover => {
                    let i = instr.arg();
                    println!("clover {}   {:?}", i, self.stack);

                    let vref = self.clovers[i];
                    self.stack.push(vref);
                },

                Opcode::Add => {
                    let (i, j) = instr.args();
                    println!("add    {} {} {:?}", i, j, self.stack);

                    let a = self.stack[i];
                    let b = self.stack[j];
                    self.stack.push(try!(a.add(b)));
                },

                Opcode::Sub => {
                    let (i, j) = instr.args();
                    println!("sub    {} {} {:?}", i, j, self.stack);

                    let a = self.stack[i];
                    let b = self.stack[j];
                    self.stack.push(try!(a.sub(b)));
                },

                Opcode::Mul => {
                    let (i, j) = instr.args();
                    println!("mul    {} {} {:?}", i, j, self.stack);

                    let a = self.stack[i];
                    let b = self.stack[j];
                    self.stack.push(try!(a.mul(b)));
                },

                Opcode::Div => {
                    let (i, j) = instr.args();
                    println!("div    {} {} {:?}", i, j, self.stack);

                    let a = self.stack[i];
                    let b = self.stack[j];
                    self.stack.push(try!(a.div(b)));
                },

                Opcode::Fn => {
                    let i = instr.arg();
                    println!("fn     {}   {:?}", i, self.stack);

                    let cob = self.codeobjs[i];
                    if let Value::Procedure(vmproc) = self.heap.deref(cob) {
                        let new_len = self.stack.len() - vmproc.clover_count;
                        let fnref = self.heap.alloc(&Value::Closure(Closure {
                            codeobj: cob,
                            clovers: &self.stack[new_len..]
                        }));
                        self.stack.truncate(new_len);
                        self.stack.push(fnref);
                    } else {
                        panic!();
                    }
                },

                Opcode::Call => {
                    let mut n = instr.arg();
                    println!("call   {}   {:?}", n, self.stack);

                    n += 1;
                    let start = self.stack.len() - n;
                    for i in 0..n {
                        self.stack[i] = self.stack[start + i].clone();
                    }
                    self.stack.truncate(n);

                    let fnref = self.stack[0];
                    let (cob, cls) = match self.heap.deref(fnref) {
                        Value::Closure(cls) => (
                            cls.codeobj,
                            unsafe {
                                slice::from_raw_parts(cls.clovers.as_ptr(),
                                                      cls.clovers.len())
                            }),
                        _ => panic!()
                    };
                    self.fetch_proc(cob);
                    self.clovers = cls;
                    self.ip = 0;
                }
            }
        }
        
        Ok(self.stack.pop().unwrap())
    }

    fn fetch_proc(&mut self, procref: ValueRef) {
        if let Value::Procedure(vmproc) = self.heap.deref(procref) {
            self.fetch_instrs(vmproc.instrs);
            self.fetch_consts(vmproc.consts);
            self.fetch_subprocs(vmproc.codeobjs);
        } else {
            panic!()
        }
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

    fn fetch_subprocs(&mut self, cobsref: ValueRef) {
        if let Value::Tuple(cob_ref_slice) = self.heap.deref(cobsref) {
            self.codeobjs = unsafe {
                slice::from_raw_parts(cob_ref_slice.as_ptr(),
                                      cob_ref_slice.len())
            };
        } else {
            panic!();
        }
    }
}
