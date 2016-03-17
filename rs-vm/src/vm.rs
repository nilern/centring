use std::rc::Rc;
use std::collections::HashMap;
use std::mem::transmute;

use value::{Value, ValueRef, CodeObject, Closure, NativeFn};
use environment::EnvRef;
use bytecode::{Bytecode, CONSTANT, CLOSED, CALL};

pub struct VM {
    modules: HashMap<String, EnvRef>
}

pub struct VMProcess {
    // instrs: Rc<Vec<Bytecode>>,
    ip: usize,
    codeobj: Rc<CodeObject>,
    clovers: Vec<ValueRef>,
    
    // consts: Rc<Vec<ValueRef>>,
    // codeobjs: Rc<Vec<Rc<CodeObject>>>,
    
    // clovers: Rc<Vec<ValueRef>>,
    stack: Vec<ValueRef>
}

pub enum VMError {
    ArgcMismatch(usize, usize)
}

pub type VMResult = Result<ValueRef, VMError>;

impl VMProcess {
    pub fn new(closure: ValueRef, args: Vec<ValueRef>) -> VMProcess {
        VMProcess {
            ip: 0,
            codeobj: closure.unwrap_codeobj().clone(),
            clovers: closure.unwrap_clovers().clone(),
            stack: args
        }
    }
    
    pub fn run(&mut self) {
        while self.ip < self.codeobj.instrs.len() {
            match self.current_opcode() {
                CONSTANT => {
                    let n = self.current_oparg() as usize;
                    self.stack.push(self.codeobj.consts[n].clone());
                    self.ip += 1;
                    
                    println!("const {} {:?}", n, self.stack);
                },
                CLOSED => {
                    let n = self.current_oparg() as usize;
                    self.stack.push(self.clovers[n].clone());
                    self.ip += 1;

                    println!("closed {} {:?}", n, self.stack);
                },

                CALL => {
                    let n = self.current_oparg() as usize;
                    self.call(n);

                    println!("call {} {:?}", n, self.stack);
                },

                op => panic!("unknown opcode {:x}", op)
            }
        }
    }

    fn current_opcode(&self) -> u8 {
        unsafe {
            transmute::<Bytecode, [u8; 4]>(self.codeobj.instrs[self.ip])[3]
        }
    }

    fn current_oparg(&self) -> u32 {
        unsafe {
            transmute::<Bytecode, u32>(self.codeobj.instrs[self.ip]) & 0x00ffffff
        }
    }

    pub fn arg(&self, n: usize) -> ValueRef {
        self.stack[n].clone()
    }

    pub fn push(&mut self, v: ValueRef) {
        self.stack.push(v)
    }

    pub fn call(&mut self, n: usize) {
        let l = self.stack.len();
        let f = self.stack[l - n - 1].clone();
        for i in 0..n {
            self.stack[i] = self.stack[l - n + i].clone()
        }
        self.stack.truncate(n);
        
        match *f {
            Value::Fn(Closure { ref codeobj, ref clovers }) => {
                self.ip = 0;
                self.codeobj = codeobj.clone();
                self.clovers = clovers.clone();
            },
            Value::NativeFn(NativeFn { ref code }) => code(self),
            _ => panic!()
        }
    }

    pub fn halt(&mut self) {
        self.ip = self.codeobj.instrs.len()
    }
}
