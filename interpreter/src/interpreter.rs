use gc::{ValueRef, GCState};
use ops::CheckedAdd;

use std::rc::{Rc, Weak};
use std::cell::Cell;

type ValueHandle = Rc<Cell<ValueRef>>;
type WeakValueHandle = Weak<Cell<ValueRef>>;

pub struct Interpreter {
    gc: GCState,
    stack_roots: Vec<WeakValueHandle>
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            gc: GCState::new(1024),
            stack_roots: vec![]
        }
    }

    pub fn alloc<T: Clone>(&mut self, v: T) -> ValueHandle {
        let res = unsafe { Rc::new(Cell::new(self.gc.alloc(v))) };
        self.stack_roots.push(Rc::downgrade(&res));
        res
    }

    fn collect(&mut self) {
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
        let mut itp = Interpreter::new();
        let a = itp.alloc::<i64>(5);
        a.get().set_typeref(a.get());
        itp.collect();
        unsafe {
            assert_eq!(a.get().unbox::<i64>(), 5);
        }
    }
}
