use std::rc::Rc;
use std::collections::HashMap;

use value::{Value, ValueRef, Closure, NativeFn};
use environment::EnvRef;
use bytecode::{Bytecode, Arg, Const, Closed, Brf, Call, Fn};

// Types

pub struct VM {
    modules: HashMap<String, EnvRef>
}

pub struct VMProcess {
    ip: usize,
    
    codeobj: Rc<CodeObject>,
    clovers: Rc<Vec<ValueRef>>,
    
    stack: Vec<ValueRef>
}

#[derive(Debug)]
pub struct CodeObject {
    pub instrs: Vec<Bytecode>,
    pub consts: Vec<ValueRef>,
    pub codeobjs: Vec<Rc<CodeObject>>
}

#[derive(Debug)]
pub enum VMError {
    ArgcMismatch(usize, usize)
}

pub type VMResult = Result<ValueRef, VMError>;

// Behaviour

impl VM {
    pub fn new() -> VM {
        VM {
            modules: HashMap::new()
        }
    }

    pub fn spawn(&self, closure: ValueRef, args: Vec<ValueRef>) -> VMProcess {
        VMProcess {
            ip: 0,
            codeobj: closure.unwrap_codeobj().clone(),
            clovers: closure.unwrap_clovers().clone(),
            stack: args
        }
    }
}

impl VMProcess {
    pub fn run(&mut self) -> VMResult {
        while self.ip < self.codeobj.instrs.len() {
            match self.codeobj.instrs[self.ip] {
                Arg(n) => {
                    println!("arg    {} {:?}", n, self.stack);

                    let arg = self.arg(n);
                    self.stack.push(arg);
                    self.ip += 1;
                },
                Const(n) => {
                    println!("const  {} {:?}", n, self.stack);
                    
                    self.stack.push(self.codeobj.consts[n].clone());
                    self.ip += 1;
                },
                Closed(n) => {
                    println!("closed {} {:?}", n, self.stack);
                    
                    self.stack.push(self.clovers[n].clone());
                    self.ip += 1;
                },

                Brf(n) => {
                    println!("brf    {} {:?}", n, self.stack);
                    
                    let cond = self.stack.pop().unwrap().unwrap_bool();
                    if !cond {
                        self.ip = n;
                    } else {
                        self.ip += 1;
                    }
                },

                Fn(i, n) => {
                    println!("fn     {} {} {:?}", i, n, self.stack);
                    
                    let codeobj = self.codeobj.codeobjs[i].clone();
                    let l = self.stack.len();
                    let clovers = Rc::new(self.stack.split_off(l - n));
                    let closure = Rc::new(Value::Fn(Closure {
                        codeobj: codeobj,
                        clovers: clovers
                    }));
                    self.stack.push(closure);
                    self.ip += 1;
                },

                Call(n) => {
                    println!("call   {} {:?}", n, self.stack);
                    
                    try!(self.call(n));
                }
            }
        }
        Ok(self.stack.pop().unwrap())
    }

    pub fn arg(&self, n: usize) -> ValueRef {
        self.stack[n].clone()
    }

    pub fn push(&mut self, v: ValueRef) {
        self.stack.push(v)
    }

    pub fn call(&mut self, n: usize) -> VMResult {
        let l = self.stack.len();
        let start = l - n;
        let f = self.stack[l - n - 1].clone();
        for i in 0..n {
            self.stack[i] = self.stack[start + i].clone()
        }
        self.stack.truncate(n);
        
        match *f {
            Value::Fn(Closure { ref codeobj, ref clovers }) => {
                self.ip = 0;
                self.codeobj = codeobj.clone();
                self.clovers = clovers.clone();
            },
            Value::NativeFn(NativeFn { ref code }) => return code(self),
            _ => panic!()
        }
        Ok(Rc::new(Value::Tuple(vec![])))
    }

    pub fn halt(&mut self) {
        self.ip = self.codeobj.instrs.len()
    }
}
