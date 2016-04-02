use std::slice;
use std::mem::{size_of, transmute};

use vm::{VMError, VMResult};
use bytecode::Bytecode;

// TODO:
// - Add more Value variants
// - Implement a growth strategy for the semispaces
// - Support mutation (Value::Ref, Value::Array, Value::Buffer ???)
// - Use raw pointers
// - Make generational

// Constants

const IMMEDIACY_BITS: usize = 0x03;
const INT_BITS: usize = 0x01;

const REF_SHIFT: usize = 2;
pub const INT_SHIFT: usize = 1;

const BYTEBLOCK_BIT: usize =  0x4000_0000_0000_0000;
const FORWARDING_BIT: usize = 0x8000_0000_0000_0000;
const TYPE_BITS: usize =      0x0F00_0000_0000_0000;
const LENGTH_BITS: usize =    0x00FF_FFFF_FFFF_FFFF;

pub const INT_TAG: usize = 1;

const TUPLE_TAG: usize =  0x0100_0000_0000_0000;
const BUFFER_TAG: usize = 0x0200_0000_0000_0000;
const COB_TAG: usize = 0x0300_0000_0000_0000;
const FN_TAG: usize = 0x0400_0000_0000_0000;

// Types

#[derive(Debug, Clone, Copy)]
pub struct ValueRef(usize);

#[derive(Debug)]
pub struct GcHeap {
    fromspace: Vec<ValueRef>,
    tospace: Vec<ValueRef>
}

#[derive(Debug)]
pub enum Value<'a> {
    Int(isize),

    Tuple(&'a [ValueRef]),

    Buffer(&'a [u8]),

    Closure(Closure<'a>),
    Procedure(Procedure)
}

#[derive(Debug)]
pub struct Closure<'a> {
    pub codeobj: ValueRef,
    pub clovers: &'a [ValueRef]
}

#[derive(Debug)]
struct Procedure {
    pub instrs: ValueRef,
    pub consts: ValueRef,
    pub codeobjs: ValueRef,
    pub clover_count: usize
}

pub struct DeflatedProcedure<'a> {
    pub instrs: Vec<Bytecode>,
    pub consts: Vec<Value<'a>>,
    pub codeobjs: Vec<DeflatedProcedure<'a>>,
    pub clover_count: usize
}

// Behaviour

impl ValueRef {
    pub fn new(data: usize) -> ValueRef {
        ValueRef(data)
    }
    
    pub fn is_immediate(&self) -> bool {
        self.0 & IMMEDIACY_BITS != 0
    }

    fn from_index(i: usize) -> ValueRef {
        ValueRef(i << REF_SHIFT)
    }

    pub fn get_int(&self) -> Option<isize> {
        if self.0 & INT_BITS == INT_TAG {
            Some(self.0 as isize >> INT_SHIFT)
        } else {
            None
        }
    }

    pub fn addi(self, other: ValueRef) -> VMResult {
        if let (INT_TAG, INT_TAG) = (self.0 & INT_BITS, other.0 & INT_BITS) {
            Ok(ValueRef((self.0 as isize + other.0 as isize - 1) as usize))
        } else {
            Err(VMError::TypeMismatch)
        }
    }

    pub fn subi(self, other: ValueRef) -> VMResult {
        if let (INT_TAG, INT_TAG) = (self.0 & INT_BITS, other.0 & INT_BITS) {
            Ok(ValueRef((self.0 as isize - other.0 as isize + 1) as usize))
        } else {
            Err(VMError::TypeMismatch)
        }
    }

    pub fn muli(self, other: ValueRef) -> VMResult {
        if let (INT_TAG, INT_TAG) = (self.0 & INT_BITS, other.0 & INT_BITS) {
            Ok(ValueRef(((self.0 as isize - 1)*(other.0 as isize - 1)/2 + 1)
                        as usize))
        } else {
            Err(VMError::TypeMismatch)
        }
    }

    pub fn divi(self, other: ValueRef) -> VMResult {
        if let (INT_TAG, INT_TAG) = (self.0 & INT_BITS, other.0 & INT_BITS) {
            Ok(ValueRef(((self.0 as isize - 1)/(other.0 as isize - 1)*2 + 1)
                        as usize))
        } else {
            Err(VMError::TypeMismatch)
        }
    }
}

impl GcHeap {
    pub fn with_capacity(capacity: usize) -> GcHeap {
        GcHeap {
            fromspace: Vec::with_capacity(capacity),
            tospace: Vec::with_capacity(capacity)
        }
    }
    
    pub fn alloc(&mut self, val: &Value) -> ValueRef {
        match *val {
            Value::Int(i) => ValueRef((i << INT_SHIFT) as usize | INT_TAG),

            Value::Tuple(vals) => {
                let start = self.fromspace.len();
                let header = TUPLE_TAG | vals.len();
                self.fromspace.push(ValueRef(header));

                self.fromspace.extend_from_slice(vals);
                
                ValueRef::from_index(start)
            },

            Value::Buffer(bytes) => {
                let start = self.fromspace.len();
                let bc = bytes.len();
                
                let header = BYTEBLOCK_BIT | BUFFER_TAG | bc;
                self.fromspace.push(ValueRef(header));

                let wc = if bc % 8 == 0 { bc/8 } else { bc/8 + 1 };
                let words = unsafe {
                    let ptr = transmute(bytes.as_ptr());
                    slice::from_raw_parts(ptr, wc)
                };
                self.fromspace.extend_from_slice(words);
                
                ValueRef::from_index(start)
            },

            Value::Procedure(ref cob) => {
                let start = self.fromspace.len();
                let header = COB_TAG | 3;

                let ccref = self.alloc(&Value::Int(cob.clover_count as isize));
                self.fromspace.push(ValueRef(header));
                self.fromspace.push(cob.instrs);
                self.fromspace.push(cob.consts);
                self.fromspace.push(cob.codeobjs);
                self.fromspace.push(ccref);

                ValueRef::from_index(start)
            },

            Value::Closure(ref fun) => {
                let start = self.fromspace.len();
                let header = FN_TAG | 1 + fun.clovers.len();

                self.fromspace.push(ValueRef(header));
                self.fromspace.push(fun.codeobj);
                self.fromspace.extend_from_slice(fun.clovers);

                ValueRef::from_index(start)
            }
        }
    }

    pub fn inflate(&mut self, inflatee: &DeflatedProcedure) -> ValueRef {
        let ic = inflatee.instrs.len();
        let bc = ic * size_of::<Bytecode>();
        let instrref = self.alloc(&Value::Buffer(unsafe {
            slice::from_raw_parts(inflatee.instrs.as_ptr() as *const _, bc)
        }));

        let constrefs: Vec<ValueRef> = inflatee.consts.iter()
            .map(|v| self.alloc(v)).collect();
        let consttupref = self.alloc(&Value::Tuple(constrefs.as_slice()));

        let cobrefs: Vec<ValueRef> = inflatee.codeobjs.iter()
            .map(|v| self.inflate(v)).collect();
        let cobtupref = self.alloc(&Value::Tuple(cobrefs.as_slice()));

        self.alloc(&Value::Procedure(Procedure {
            instrs: instrref,
            consts: consttupref,
            codeobjs: cobtupref,
            clover_count: inflatee.clover_count
        }))
    }

    pub fn deref(&self, vref: ValueRef) -> Value {
        match vref.0 & INT_BITS {
            INT_TAG => Value::Int((vref.0 as isize) >> INT_SHIFT),
            0 => {
                let start = vref.0 >> REF_SHIFT;
                let header = self.fromspace[start].0;
                let data_start = start + 1;
                match header & TYPE_BITS {
                    TUPLE_TAG => {
                        let len = header & LENGTH_BITS;
                        Value::Tuple(&self.fromspace[data_start..data_start+len])
                    },

                    BUFFER_TAG => {
                        let bc = header & LENGTH_BITS;
                        let bsl = unsafe {
                            let ptr = transmute(&self.fromspace[data_start]);
                            slice::from_raw_parts(ptr, bc)
                        };
                        Value::Buffer(&bsl)
                    },

                    COB_TAG => Value::Procedure(Procedure {
                        instrs: self.fromspace[data_start],
                        consts: self.fromspace[data_start + 1],
                        codeobjs: self.fromspace[data_start + 2],
                        clover_count: self.fromspace[data_start + 3]
                            .get_int().unwrap() as usize
                    }),

                    FN_TAG => {
                        let len = header & LENGTH_BITS;
                        Value::Closure(Closure {
                            codeobj: self.fromspace[data_start],
                            clovers: &self.fromspace[data_start+1..data_start+len]
                        })
                    },
                    
                    _ => panic!()
                }
            },
            _ => panic!()
        }
    }

    pub fn collect(mut self, roots: &mut [ValueRef]) -> GcHeap {
        for i in 0..roots.len() {
            roots[i] = self.copy(roots[i]);
        }
        
        let mut scan = 0;
        while scan < self.tospace.len() {
            let header = self.tospace[scan].0;
            let data_len = header & LENGTH_BITS;
            let start = scan + 1;
            scan = start + data_len;
            if header & BYTEBLOCK_BIT == 0 {
                for i in start..scan {
                    let old = self.tospace[i];
                    self.tospace[i] = self.copy(old);
                }
            }
        }

        self.fromspace.clear();
        
        GcHeap {
            fromspace: self.tospace,
            tospace: self.fromspace
        }
    }

    fn copy(&mut self, vref: ValueRef) -> ValueRef {
        if let Some(fwref) = self.get_fwd(vref) {
            fwref
        } else if vref.is_immediate() {
            vref
        } else {
            let newstart = self.tospace.len();
            
            let oldstart = vref.0 >> REF_SHIFT;
            {
                let header = self.fromspace[oldstart].0;
                let data_len = if header & BYTEBLOCK_BIT == 0 {
                    header & LENGTH_BITS
                } else {
                    let bc = header & LENGTH_BITS;
                    if bc % 8 == 0 { bc/8 } else { bc/8 + 1 }
                };
                let data = &self.fromspace[oldstart..oldstart+data_len+1];
                self.tospace.extend_from_slice(data);
            }

            self.fromspace[oldstart] =
                ValueRef(FORWARDING_BIT | newstart);

            ValueRef::from_index(newstart)
        }
    }

    fn get_fwd(&self, vref: ValueRef) -> Option<ValueRef> {
        if vref.is_immediate() {
            return None
        }

        let header = self.fromspace[vref.0 >> REF_SHIFT].0;
        if header & FORWARDING_BIT == 0 {
            None
        } else {
            // This will also shift out the leftmost forwarding bit:
            Some(ValueRef::from_index(header))
        }
    }
}
