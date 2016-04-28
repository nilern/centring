use std::hash::{Hash, Hasher, SipHasher};

use gc::{Value, ValueRef, GcHeap};
use vm::VMError;

struct HashTable<'a> {
    buckets: &'a [ValueRef]
}

impl GcHeap {
    pub fn alloc_hashtable(&mut self, table: HashTable) -> ValueRef {
        self.alloc(&Value::Array(table.buckets))
    }
}

impl<'a> HashTable<'a> {
    pub fn insert(tabref: ValueRef, k: ValueRef, v: ValueRef) -> ValueRef {
        tabref // placeholder, of course
    }

    pub fn get(tabref: ValueRef, k: ValueRef) -> Result<Option<ValueRef>, VMError> {
        let i = Self::hash(k) as usize % try!(tabref.alength());
        let mut node = try!(tabref.aget(i)).unwrap();
        loop {
            match node.clone().deref() {
                Value::Tuple(vals) => if vals[0] == k { // found it!
                    return Ok(Some(vals[1]));
                } else if vals.len() == 2 { // the last node, give up
                    return Ok(None)
                } else { // not this one, try next node
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
