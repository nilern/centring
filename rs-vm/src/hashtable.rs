use std::hash::{Hash, Hasher, SipHasher};

use gc::{Value, ValueRef, GcHeap};
use vm::VMError;

struct HashTable<'a> {
    buckets: &'a [ValueRef]
}

impl GcHeap {
    pub fn alloc_hashtable(&mut self, capacity: usize) -> ValueRef {
        // TODO: Don't allocate temp Vec
        let vals = vec![self.alloc(&Value::Unbound); capacity];
        self.alloc(&Value::Array(&vals))
    }
}

impl<'a> HashTable<'a> {
    pub fn insert(heap: &mut GcHeap, tabref: ValueRef, k: ValueRef, v: ValueRef)
                  -> Result<(), VMError> {
        // TODO: resize when linked lists start to add up
        let i = Self::hash(k) as usize % try!(tabref.alength());
        let mut node = try!(tabref.aget_mut(i)).unwrap() as *mut ValueRef;
        loop {
            unsafe {
                match (*node).deref() {
                    Value::Array(vals) => if vals[0] == k {
                        // found old value, replace:
                        *node.offset(2) = v;
                        return Ok(());
                    } else if vals.len() == 2 { // last node, append:
                        let new_node = heap.alloc(&Value::Array(&[k, v]));
                        let replacement = heap.alloc(
                            &Value::Array(&[vals[0], vals[1], new_node]));
                        *node = replacement;
                        return Ok(())
                    } else { // more nodes to traverse:
                        node = try!((*node).aget_mut(i)).unwrap() as *mut ValueRef;
                    },
                    Value::Unbound => { // bucket was empty, add first node:
                        *node = heap.alloc(&Value::Array(&[k, v]));
                        return Ok(());
                    }
                    _ => return Err(VMError::TypeMismatch)
                }
            }
        }
    }

    pub fn get(tabref: ValueRef, k: ValueRef) -> Result<Option<ValueRef>, VMError> {
        let i = Self::hash(k) as usize % try!(tabref.alength());
        let mut node = *try!(tabref.aget(i)).unwrap();
        loop {
            match node.clone().deref() {
                Value::Array(vals) => if vals[0] == k { // found it!:
                    return Ok(Some(vals[1]));
                } else if vals.len() == 2 { // the last node, give up:
                    return Ok(None)
                } else { // not this one, try next node:
                    node = vals[2];
                },
                Value::Unbound => return Ok(None), // empty bucket, give up
                _ => return Err(VMError::TypeMismatch)
            }
        }
    }

    fn hash(vref: ValueRef) -> u64 {
        let mut hasher = SipHasher::new();
        vref.hash(&mut hasher);
        hasher.finish()
    }
}
