use gc::Collector;
use value::ValueRef;
use ops::CheckedAdd;

use std::rc::{Rc, Weak};
use std::cell::Cell;

pub type ValueHandle = Rc<Cell<ValueRef>>;
pub type WeakValueHandle = Weak<Cell<ValueRef>>;

/// An `Interpreter` holds all the Centring state. This arrangement is inspired
/// by `lua_State` in PUC Lua.
pub struct Interpreter {
    gc: Collector,
    stack_roots: Vec<WeakValueHandle>
}

impl Interpreter {
    /// Make a new `Interpreter` to execute Centring with.
    pub fn new() -> Interpreter {
        Interpreter {
            gc: Collector::new(1024),
            stack_roots: vec![]
        }
    }

    /// Allocate a bits type. Return a handle and do the bookkeeping necessary
    /// to treat the handle as a GC root if non-weak copies of it are live when
    /// GC is initiated.
    pub fn alloc<T: Clone>(&mut self, v: T) -> ValueHandle {
        let res = unsafe { Rc::new(Cell::new(self.gc.alloc(v))) };
        self.stack_roots.push(Rc::downgrade(&res));
        res
    }

    unsafe fn collect(&mut self) {
        let gc = &mut self.gc;
        self.stack_roots.retain(|whandle|
            if let Some(handle) = whandle.upgrade() {
                // Rust still has a live ValueHandle so this is a GC root and
                // needs to be marked and retained:
                handle.set(handle.get().mark(gc));
                true
            } else {
                // The WeakValueHandle has lapsed so Rust no longer has live
                // ValueHandle:s to this and we can throw this away:
                false
            });
        gc.collect();
    }
}

/// # Tests

#[cfg(test)]
mod tests {
    use super::Interpreter;

    #[test]
    fn collect() {
        unsafe {
            let mut itp = Interpreter::new();
            let a = itp.alloc::<i64>(5);
            a.get().set_typeref(a.get());
            itp.collect();
            assert_eq!(a.get().unbox::<i64>(), 5);
        }
    }
}
