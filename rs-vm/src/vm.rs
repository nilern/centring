use std::mem::{size_of, transmute};
use std::slice;
use std::hash::{Hash, Hasher};

use gc::{GcHeap, Value, ValueRef, DeflatedProcedure, Closure};
use bytecode::{Bytecode, Opcode, Arg, Domain};

// Types

pub struct VM;

pub struct VMProcess<'a> {
    vm: &'a VM,           // The parent VM of this process
    heap: GcHeap,         // The GC'ed memory for this process
    // curr_mod: ValueRef,   // The currently executing Module
    // mod_registry: HashMap<String, ValueRef> // Keeps 'em reachable
    stack: Vec<ValueRef>, // Workspace

    // These slices point into the GcHeap and are updated on calls:
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
        vmproc.close_over(procref);
        vmproc.fetch_proc(procref);
        
        vmproc
    }
}

impl<'a> VMProcess<'a> {
    pub fn run(&'a mut self) -> VMResult {
        while self.ip < self.instrs.len() {
            let instr = self.instrs[self.ip];
            self.ip += 1;
            println!("{:?}", instr.op());

            macro_rules! binop {
                ($op:ident) => {{
                    let (a, b) = self.fetch_args(instr);
                    self.stack.push(try!(a.$op(b)));
                }};
            }
            
            match instr.op() {
                Opcode::Load => {
                    let a = self.fetch_arg(instr.arg());
                    self.stack.push(a);
                },
                Opcode::Splat => {
                    let a = self.fetch_arg(instr.arg());
                    let vs = a.deref_contents();
                    self.stack.extend_from_slice(vs);
                },

                Opcode::Tuple => {
                    let i = instr.index();
                    let tup = self.heap.alloc(&Value::Tuple(&self.stack[i..]));
                    self.stack.truncate(i);
                    self.stack.push(tup);
                },

                Opcode::IAdd => binop!(iadd),
                Opcode::ISub => binop!(isub),
                Opcode::IMul => binop!(imul),
                Opcode::IDiv => binop!(idiv),

                Opcode::Halt => {
                    let a = self.fetch_arg(instr.arg());
                    return Ok(a);
                },

                Opcode::Fn => {
                    let i = instr.index();
                    let cob = self.codeobjs[i];
                    self.close_over(cob);
                },

                Opcode::Call => {
                    // TODO: Only clean up stack & heap when necessary
                    
                    // Throw away stack items we don't need anymore:
                    let i = instr.index();
                    let len = self.stack.len();
                    let n = len - i;
                    for (i, j) in (0..n).zip(i..len) {
                        self.stack[i] = self.stack[j]
                    }
                    self.stack.truncate(n);

                    // Release unreachable objects:
                    self.heap.collect(self.stack.as_mut_slice());

                    // Set things up and jump to the callee:
                    let fnref = self.stack[0];
                    self.fetch_closure(fnref);
                    self.ip = 0;
                }
            }
        }
        
        Ok(self.stack.pop().unwrap())
    }

    fn close_over(&mut self, cob: ValueRef) {
        if let Value::Procedure(vmproc) = cob.deref() {
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
    }

    fn fetch_args(&self, instr: Bytecode) -> (ValueRef, ValueRef) {
        (self.fetch_arg(instr.arg0()), self.fetch_arg(instr.arg1()))
    }

    fn fetch_arg(&self, i: Arg) -> ValueRef {
        match i.domain() {
            Domain::Local => self.stack[i.index()],
            Domain::Clover => self.clovers[i.index()],
            Domain::Const => self.consts[i.index()],
            // Domain::Global => ...
        }
    }

    fn fetch_closure(&mut self, fnref: ValueRef) {
        let (cob, cls) = match fnref.deref() {
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
    }

    fn fetch_proc(&mut self, procref: ValueRef) {
        if let Value::Procedure(vmproc) = procref.deref() {
            self.fetch_instrs(vmproc.instrs);
            self.fetch_consts(vmproc.consts);
            self.fetch_subprocs(vmproc.codeobjs);
        } else {
            panic!()
        }
    }

    fn fetch_instrs(&mut self, bufref: ValueRef) {
        if let Value::Buffer(bufbytes) = bufref.deref() {
            self.instrs = unsafe {
                slice::from_raw_parts(bufbytes.as_ptr() as *const _,
                                      bufbytes.len() / size_of::<Bytecode>())
            };
        } else {
            panic!();
        }
    }

    fn fetch_consts(&mut self, constsref: ValueRef) {
        if let Value::Tuple(const_ref_slice) = constsref.deref() {
            self.consts = unsafe {
                slice::from_raw_parts(const_ref_slice.as_ptr(),
                                      const_ref_slice.len())
            };
        } else {
            panic!();
        }
    }

    fn fetch_subprocs(&mut self, cobsref: ValueRef) {
        if let Value::Tuple(cob_ref_slice) = cobsref.deref() {
            self.codeobjs = unsafe {
                slice::from_raw_parts(cob_ref_slice.as_ptr(),
                                      cob_ref_slice.len())
            };
        } else {
            panic!();
        }
    }
}

macro_rules! op_impl {
    ($name:ident, $a:ident, $formula:expr) => {
        pub fn $name(self) -> VMResult {
            let $a: isize = From::from(self);
            Ok($formula)
        }
    };
    ($name:ident, $a:ident, $b:ident, $formula:expr) => {
        pub fn $name(self, other: ValueRef) -> VMResult {
            let $a: isize = From::from(self);
            let $b: isize = From::from(other);
            Ok($formula)
        }
    };
}

impl ValueRef {
    op_impl!(iadd, a, b, ValueRef((a + b - 1) as usize));
    op_impl!(isub, a, b, ValueRef((a - b + 1) as usize));
    op_impl!(imul, a, b, ValueRef(((a - 1)*(b - 1)/2 + 1) as usize));
    op_impl!(idiv, a, b, ValueRef(((a - 1)/(b - 1)*2 + 1) as usize));

    pub fn identical_to(&self, other: &ValueRef) -> bool {
        self.0 == other.0
    }

    pub fn aget(&self, i: usize) -> Result<Option<&ValueRef>, VMError> {
        match self.deref() {
            Value::Tuple(vals) => Ok(vals.get(i)),
            Value::Array(vals) => Ok(vals.get(i)),
            _ => Err(VMError::TypeMismatch)
        }
    }

    pub fn aget_mut<'a>(self, i: usize)
                       -> Result<Option<&'a mut ValueRef>, VMError> {
        match self.deref() {
            Value::Array(vals) => unsafe {
                Ok(Some(transmute(vals.as_ptr())))
            },
            _ => Err(VMError::TypeMismatch)
        }
    }

    pub fn alength(&self) -> Result<usize, VMError> {
        match self.deref() {
            Value::Tuple(vals) => Ok(vals.len()),
            Value::Array(vals) => Ok(vals.len()),
            _ => Err(VMError::TypeMismatch)
        }
    }   
}

impl PartialEq for ValueRef {
    fn eq(&self, other: &ValueRef) -> bool {
        if self.identical_to(other) { // identical immediates or pointers
            true
        } else if self.is_immediate() || other.is_immediate() {
            false
        } else { // both are pointers
            self.deref() == other.deref()
        }
    }
}

impl Hash for ValueRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.deref().hash(state)
    }
}
