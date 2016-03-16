use std::rc::Rc;
use std::collections::HashMap;
use std::mem::transmute;

use value::{Value, ValueRef, CodeObject, Closure, NativeFn};
use environment::EnvRef;
use bytecode::{Bytecode, BRF, FN, CALL};

pub struct VM {
    modules: HashMap<String, EnvRef>
}

pub struct VMProcess {
    instrs: Rc<Vec<Bytecode>>,
    ip: usize,
    
    consts: Rc<Vec<ValueRef>>,
    codeobjs: Rc<Vec<Rc<CodeObject>>>,
    
    clovers: Rc<Vec<ValueRef>>,
    args: Vec<ValueRef>,
    stack: Vec<ValueRef>
}

pub enum VMError {
    ArgcMismatch(usize, usize)
}

pub type VMResult = Result<ValueRef, VMError>;

impl VMProcess {
    fn run(&mut self) {
        while self.ip < self.instrs.len() {
            match self.current_opcode() {
                BRF => if !self.stack.pop().unwrap().unwrap_bool() {
                    self.ip = self.current_oparg() as usize;
                } else {
                    self.ip += 1;
                },
                
                FN => {
                    let n = self.current_oparg() as usize;
                    let clovers = Rc::new(self.stack.split_off(n));
                    let code = self.codeobjs[n].clone();
                    self.stack.push(Rc::new(Value::Fn(Closure {
                        code: code,
                        clovers: clovers
                    })));
                    self.ip += 1;
                },
                
                CALL => {
                    let n = self.current_oparg() as usize;
                    self.args = self.stack.split_off(n);
                    let f = self.stack.pop().unwrap();
                    self.stack.clear();
                    self.call(f);
                },

                op => panic!("unknown opcode {:x}", op)
            }
        }
    }

    fn call(&mut self, f: ValueRef) {
        match *f {
            Value::Fn(Closure { ref code, ref clovers }) => {
                self.instrs = code.instrs.clone();
                self.ip = 0;
                self.clovers = clovers.clone();
                self.consts = code.consts.clone();
                self.codeobjs = code.codeobjs.clone();
            },
            Value::NativeFn(NativeFn { ref code }) => code(self),
            _ => panic!("centring.lang/call is uniplemented!")
        }
    }

    fn current_opcode(&self) -> u8 {
        unsafe { transmute::<Bytecode, [u8; 4]>(self.instrs[self.ip])[0] }
    }

    fn current_oparg(&self) -> u32 {
        unsafe { transmute::<Bytecode, u32>(self.instrs[self.ip]) & 0x00ffffff }
    }
}
