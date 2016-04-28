use std::slice;
use std::mem::{size_of, transmute, swap};

use bytecode::Bytecode;

// TODO:
// - Add more Value variants
// - Implement a growth strategy for the semispaces
// - Support mutation (Value::Ref, Value::Array, Value::Buffer ???)
// - Make generational

// Constants

const IMMEDIACY_BITS: usize = 0x03;
const INT_BITS: usize = 0x01;

const FORWARDING_SHIFT: usize = 2;
pub const INT_SHIFT: usize = 1;

const BYTEBLOCK_BIT: usize =  0x4000_0000_0000_0000;
const FORWARDING_BIT: usize = 0x8000_0000_0000_0000;
const TYPE_BITS: usize =      0x0F00_0000_0000_0000;
const LENGTH_BITS: usize =    0x00FF_FFFF_FFFF_FFFF;

pub const INT_TAG: usize = 1;
pub const IMMEDIACY_TAG: usize = 2;
pub const UNBOUND_TAG: usize = 0x2E;

const TUPLE_TAG: usize =  0x0100_0000_0000_0000;
const ARRAY_TAG: usize =  0x0200_0000_0000_0000;
const BUFFER_TAG: usize = 0x0300_0000_0000_0000;
const COB_TAG: usize =    0x0400_0000_0000_0000;
const FN_TAG: usize =     0x0500_0000_0000_0000;

// Types

#[derive(Debug, Clone, Copy, Eq)]
pub struct ValueRef(pub usize);

#[derive(Debug)]
pub struct GcHeap {
    fromspace: Vec<ValueRef>,
    tospace: Vec<ValueRef>
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Value<'a> {
    Int(isize),
    Unbound,

    Tuple(&'a [ValueRef]),
    Array(&'a [ValueRef]),

    Buffer(&'a [u8]),

    Closure(Closure<'a>),
    Procedure(Procedure)
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Closure<'a> {
    pub codeobj: ValueRef,
    pub clovers: &'a [ValueRef]
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Procedure {
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
    pub fn deref<'a>(&'a self) -> Value<'a> {
        match self.0 & IMMEDIACY_BITS {
            INT_TAG => Value::Int((self.0 as isize) >> INT_SHIFT),
            IMMEDIACY_TAG => match self.0 & 0xFF { // HACK
                UNBOUND_TAG => Value::Unbound,
                _ => panic!()
            },
            0 => unsafe {
                let start = self.as_ptr();
                let header = (*start).0;
                let data_start = start.offset(1);
                match header & TYPE_BITS {
                    TUPLE_TAG => {
                        let len = header & LENGTH_BITS;
                        Value::Tuple(slice::from_raw_parts(data_start, len))
                    },
                    
                    ARRAY_TAG => {
                        let len = header & LENGTH_BITS;
                        Value::Array(slice::from_raw_parts(data_start, len))
                    },

                    BUFFER_TAG => {
                        let bc = header & LENGTH_BITS;
                        let bsl =
                            slice::from_raw_parts(data_start as *const u8, bc);
                        Value::Buffer(&bsl)
                    },

                    COB_TAG => Value::Procedure(Procedure {
                        instrs: *data_start,
                        consts: *data_start.offset(1),
                        codeobjs: *data_start.offset(2),
                        clover_count: (*data_start.offset(3))
                            .get_int().unwrap() as usize
                    }),

                    FN_TAG => {
                        let len = header & LENGTH_BITS;
                        Value::Closure(Closure {
                            codeobj: *data_start,
                            clovers: slice::from_raw_parts(data_start.offset(1),
                                                           len - 1),
                        })
                    },
                    
                    _ => panic!()
                }
            },
            _ => panic!()
        }
    }

    pub fn deref_contents(&self) -> &[ValueRef] {
        unsafe {
            let start = self.as_ptr();
            let header = (*start).0;
            let data_start = start.offset(1);
            let len = header & LENGTH_BITS;
            slice::from_raw_parts(data_start, len)
        }
    }
    
    pub fn is_immediate(&self) -> bool {
        self.0 & IMMEDIACY_BITS != 0
    }

    pub fn is_int(&self) -> bool {
        self.0 & INT_BITS == INT_TAG
    }

    pub fn get_int(&self) -> Option<isize> {
        if self.is_int() {
            Some(self.0 as isize >> INT_SHIFT)
        } else {
            None
        }
    }

    fn header(&self) -> Option<usize> {
        if self.is_immediate() {
            None
        } else {
            unsafe { Some((*self.as_ptr()).0) }
        }
    }

    fn as_ptr(&self) -> *const ValueRef {
        self.0 as *const ValueRef
    }

    fn as_mut_ptr(&self) -> *mut ValueRef {
        self.0 as *mut ValueRef
    }
}

impl From<ValueRef> for usize {
    fn from(vref: ValueRef) -> usize {
        vref.0
    }
}

impl From<ValueRef> for isize {
    fn from(vref: ValueRef) -> isize {
        vref.0 as isize
    }
}

impl From<*const ValueRef> for ValueRef {
    fn from(ptr: *const ValueRef) -> ValueRef {
        ValueRef(ptr as usize)
    }
}

impl GcHeap {
    pub fn with_capacity(capacity: usize) -> GcHeap {
        GcHeap {
            fromspace: Vec::with_capacity(capacity),
            tospace: Vec::with_capacity(capacity)
        }
    }

    fn next_ptr(&self) -> *const ValueRef {
        unsafe {
            self.fromspace.as_ptr().offset(self.fromspace.len() as isize)
        }
    }
    
    pub fn alloc(&mut self, val: &Value) -> ValueRef {
        match *val {
            Value::Int(i) => ValueRef((i << INT_SHIFT) as usize | INT_TAG),
            Value::Unbound => ValueRef(UNBOUND_TAG),

            Value::Tuple(vals) => {
                let start = self.next_ptr();
                let header = TUPLE_TAG | vals.len();
                self.fromspace.push(ValueRef(header));

                self.fromspace.extend_from_slice(vals);

                ValueRef::from(start)
            },

            Value::Array(vals) => {
                let start = self.next_ptr();
                let header = ARRAY_TAG | vals.len();
                self.fromspace.push(ValueRef(header));

                self.fromspace.extend_from_slice(vals);

                ValueRef::from(start)
            },

            Value::Buffer(bytes) => {
                let start = self.next_ptr();
                let bc = bytes.len();
                
                let header = BYTEBLOCK_BIT | BUFFER_TAG | bc;
                self.fromspace.push(ValueRef(header));

                let wc = wc_from_bc(bc);
                let words = unsafe {
                    let ptr = transmute(bytes.as_ptr());
                    slice::from_raw_parts(ptr, wc)
                };
                self.fromspace.extend_from_slice(words);
                
                ValueRef::from(start)
            },

            Value::Procedure(ref cob) => {
                let ccref = self.alloc(&Value::Int(cob.clover_count as isize));
                let start = self.next_ptr();
                let header = COB_TAG | 4;
                
                self.fromspace.push(ValueRef(header));
                self.fromspace.push(cob.instrs);
                self.fromspace.push(cob.consts);
                self.fromspace.push(cob.codeobjs);
                self.fromspace.push(ccref);

                ValueRef::from(start)
            },

            Value::Closure(ref fun) => {
                let start = self.next_ptr();
                let header = FN_TAG | 1 + fun.clovers.len();

                self.fromspace.push(ValueRef(header));
                self.fromspace.push(fun.codeobj);
                self.fromspace.extend_from_slice(fun.clovers);

                ValueRef::from(start)
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

    pub fn collect(&mut self, roots: &mut [ValueRef]) {
        // Relocate roots:
        for i in 0..roots.len() {
            roots[i] = self.relocate(roots[i]);
        }

        // Copy live objects breadth-first ('Cheney algorithm'):
        let mut scan = 0;
        while scan < self.tospace.len() {
            let header = self.tospace[scan].0;
            let data_len = data_wlen(header);
            let data_start = scan + 1;
            scan = data_start + data_len; // Index of next object header
            // Skip over byteblocks, they don't contain (GC'd) pointers:
            if header & BYTEBLOCK_BIT == 0 {
                // Relocate the children of this object:
                for i in data_start..scan {
                    let old = self.tospace[i];
                    self.tospace[i] = self.relocate(old);
                }
            }
        }

        self.fromspace.clear();

        swap(&mut self.fromspace, &mut self.tospace);
    }

    fn relocate(&mut self, vref: ValueRef) -> ValueRef {
        if vref.is_immediate() {
            vref // Not a heap index => no change necessary
        } else if let Some(fwref) = self.get_fwd(vref) {
            fwref // Has already been moved to this location
        } else {
            unsafe {
                let newstart = self.tospace.as_ptr()
                    .offset(self.tospace.len() as isize);

                // Copy object:
                let oldstart = vref.as_mut_ptr();
                {
                    let header = (*oldstart).0;
                    let data_len = data_wlen(header);
                    let data = slice::from_raw_parts(oldstart, data_len + 1);
                    self.tospace.extend_from_slice(data);
                }

                // Replace the header in fromspace with a forward:
                *oldstart = ValueRef(FORWARDING_BIT |
                                     newstart as usize >> FORWARDING_SHIFT);

                ValueRef::from(newstart) // Here's where we moved it
            }
        }
    }

    fn get_fwd(&self, vref: ValueRef) -> Option<ValueRef> {
        if let Some(header) = vref.header() {
            if header & FORWARDING_BIT == 0 {
                None
            } else {
                // This will simultaneously shift out FORWARDING_BIT:
                Some(ValueRef(header << FORWARDING_SHIFT))
            }
        } else {
            None
        }
    }
}

fn data_wlen(header: usize) -> usize {
    if header & BYTEBLOCK_BIT == 0 {
        header & LENGTH_BITS
    } else {
        wc_from_bc(header & LENGTH_BITS)
    }
}

fn wc_from_bc(bc: usize) -> usize {
    if bc % size_of::<ValueRef>() == 0 {
        bc / size_of::<ValueRef>()
    } else {
        bc / size_of::<ValueRef>() + 1
    }
}
