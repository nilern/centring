use gc::Collector;
use value::{CtrValue, Any, Int, ListPair, ListEmpty, Const};
use refs::{Root, WeakRoot, ValueHandle, ValuePtr};

use std::cmp::Ordering;
use std::ptr;

/// An `Interpreter` holds all the Centring state. This arrangement is inspired
/// by `lua_State` in PUC Lua.
pub struct Interpreter {
    gc: Collector,
    stack_roots: Vec<WeakRoot>,
    type_t: Root<ListEmpty>,
    pair_t: Root<ListEmpty>,
    nil_t: Root<ListEmpty>,
    int_t: Root<ListEmpty>,
    const_t: Root<ListEmpty>
}

pub enum CtrError {
    Argc {
        expected: (Ordering, usize),
        received: usize
    }
}

pub type CtrResult<T> = Result<Root<T>, CtrError>;

impl Interpreter {
    /// Make a new `Interpreter` to execute Centring with.
    pub fn new() -> Interpreter {
        // FIXME: make this sensible
        let mut itp = Interpreter {
            gc: Collector::new(1024),
            stack_roots: vec![],
            type_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            pair_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            nil_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            int_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            const_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) }
        };

        let type_t = itp.alloc_nil();
        itp.type_t = type_t.clone();
        itp.pair_t = itp.alloc_nil();
        itp.nil_t = itp.alloc_nil();
        itp.int_t = itp.alloc_nil();
        itp.const_t = itp.alloc_nil();

        itp.type_t.clone().as_any_ref().set_type(type_t.ptr());
        itp.pair_t.clone().as_any_ref().set_type(type_t.ptr());
        itp.nil_t.clone().as_any_ref().set_type(type_t.ptr());
        itp.int_t.clone().as_any_ref().set_type(type_t.ptr());
        itp.const_t.clone().as_any_ref().set_type(type_t.ptr());

        itp
    }

    pub fn alloc_rec<'a, T: CtrValue,>(&mut self, typ: ValueHandle<ListEmpty>,
        fields: &[ValueHandle<Any>]) -> Root<T> {
        let res = unsafe {
            let raw_fields = fields.iter().map(|vh| vh.ptr());
            Root::new(self.gc.alloc_rec(fields.len(), typ.ptr(), raw_fields))
        };
        self.stack_roots.push(Root::downgrade(&res));
        res
    }

    pub fn alloc_pair(&mut self, head: ValueHandle<Any>, tail: ValueHandle<Any>)
        -> Root<ListPair> {
        let typ = self.pair_t.clone();
        let fields = [head, tail];
        self.alloc_rec(typ.borrow(), &fields)
    }

    pub fn alloc_nil(&mut self) -> Root<ListEmpty> {
        let typ = self.nil_t.clone();
        let fields = [];
        self.alloc_rec(typ.borrow(), &fields)
    }

    pub fn alloc_int(&mut self, v: isize) -> Root<Int> {
        let typ = self.int_t.clone();
        let res = unsafe {
            Root::new(self.gc.alloc_bits(typ.ptr(), v))
        };
        self.stack_roots.push(Root::downgrade(&res));
        res
    }

    pub fn alloc_const(&mut self, v: ValueHandle<Any>) -> Root<Const> {
        let typ = self.const_t.clone();
        let fields = [v];
        self.alloc_rec(typ.borrow(), &fields)
    }

    pub unsafe fn collect(&mut self) {
        let gc = &mut self.gc;
        // The following also takes care of Root members of self since they
        // were created by self.alloc*.
        self.stack_roots.retain(|whandle|
            if let Some(mut handle) = whandle.upgrade::<Any>() {
                // Rust still has a live Root so this is a GC root and
                // needs to be marked and retained:
                handle.mark(gc);
                true
            } else {
                // The WeakRoot has lapsed so Rust no longer has live
                // Root:s to this and we can throw this away:
                false
            });
        gc.collect();
    }
}

/// # Tests

#[cfg(test)]
mod tests {
    use super::Interpreter;
    use value::{Bits, Unbox};

    #[test]
    fn collect() {
        let mut itp = Interpreter::new();
        let a = itp.alloc_int(3);
        let b = itp.alloc_int(5);
        let tup = itp.alloc_pair(a.borrow().as_any_ref(),
                                 b.borrow().as_any_ref());
        unsafe {
            itp.collect();
            assert_eq!((*(a.ptr() as *const Bits<i64>)).unbox(), 3);
        }
    }
}
