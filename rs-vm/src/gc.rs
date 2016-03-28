// TODO:
// - Add more Value variants
// - Implement a growth strategy for the semispaces
// - Support mutation (Value::Ref, Value::Array, Value::Buffer ???)
// - Use raw pointers

// Constants

const IMMEDIACY_BITS: usize = 0x03;
const INT_BITS: usize = 0x01;

const REF_SHIFT: usize = 2;
const INT_SHIFT: usize = 1;

const TUPLE_TAG: usize = 0x0100_0000_0000_0000;

const TYPE_BITS: usize = 0x0F00_0000_0000_0000;
const LENGTH_BITS: usize = 0x00FF_FFFF_FFFF_FFFF;

const LEFTMOST_SHIFT: usize = 63;

// Types

#[derive(Debug)]
pub enum Value<'a> {
    Int(isize),

    Tuple(&'a [ValueRef])
}

#[derive(Debug, Clone, Copy)]
pub struct ValueRef(usize);

#[derive(Debug)]
pub struct GcHeap {
    fromspace: Vec<ValueRef>,
    tospace: Vec<ValueRef>
}

// Behaviour

impl ValueRef {
    pub fn is_immediate(&self) -> bool {
        self.0 & IMMEDIACY_BITS != 0
    }

    fn from_index(i: usize) -> ValueRef {
        ValueRef(i << REF_SHIFT)
    }
}

impl GcHeap {
    pub fn with_capacity(capacity: usize) -> GcHeap {
        GcHeap {
            fromspace: Vec::with_capacity(capacity),
            tospace: Vec::with_capacity(capacity)
        }
    }
    
    pub fn alloc(&mut self, val: Value) -> ValueRef {
        match val {
            Value::Int(i) => ValueRef((i << INT_SHIFT) as usize | INT_BITS),

            Value::Tuple(vals) => {
                let start = self.fromspace.len();
                let header = TUPLE_TAG | vals.len();
                self.fromspace.push(ValueRef(header));
                for val in vals {
                    self.fromspace.push(val.clone());
                }
                ValueRef::from_index(start)
            }
        }
    }

    pub fn deref(&self, vref: ValueRef) -> Value {
        match vref.0 & 1 {
            1 => Value::Int((vref.0 as isize) >> 1),
            0 => {
                let start = vref.0 >> REF_SHIFT;
                let header = self.fromspace[start].0;
                match header & TYPE_BITS {
                    TUPLE_TAG => {
                        let len = header & LENGTH_BITS;
                        Value::Tuple(&self.fromspace[start+1..start+1+len])
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
            for i in start..scan {
                let old = self.tospace[i];
                self.tospace[i] = self.copy(old);
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
                let data_len = header & LENGTH_BITS;
                let data = &self.fromspace[oldstart..oldstart+data_len+1];
                self.tospace.extend_from_slice(data);
            }

            self.fromspace[oldstart] =
                ValueRef(1 << LEFTMOST_SHIFT | newstart);

            ValueRef::from_index(newstart)
        }
    }

    fn get_fwd(&self, vref: ValueRef) -> Option<ValueRef> {
        if vref.is_immediate() {
            return None
        }

        let header = self.fromspace[vref.0 >> REF_SHIFT].0;
        if header >> LEFTMOST_SHIFT == 0 {
            None
        } else {
            // This will also shift out the leftmost forwarding bit:
            Some(ValueRef::from_index(header))
        }
    }
}
