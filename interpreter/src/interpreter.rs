use gc::Collector;
use value::CtrValue;
use refs::{Root, WeakRoot};

/// An `Interpreter` holds all the Centring state. This arrangement is inspired
/// by `lua_State` in PUC Lua.
pub struct Interpreter {
    gc: Collector,
    stack_roots: Vec<WeakRoot>
}

impl Interpreter {
    /// Make a new `Interpreter` to execute Centring with.
    pub fn new() -> Interpreter {
        Interpreter {
            gc: Collector::new(1024),
            stack_roots: vec![]
        }
    }

    pub fn alloc<T: CtrValue>(&mut self, v: T) -> Root {
        let res = unsafe { Root::new(self.gc.alloc(v)) };
        self.stack_roots.push(Root::downgrade(&res));
        res
    }

    pub unsafe fn collect(&mut self) {
        let gc = &mut self.gc;
        self.stack_roots.retain(|whandle|
            if let Some(mut handle) = whandle.upgrade() {
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
        unsafe {
            let mut itp = Interpreter::new();
            let a = itp.alloc(Bits::new(5i64));
            let ptr = a.ptr();
            (*a.ptr()).typ = ptr;
            itp.collect();
            assert_eq!((*(a.ptr() as *const Bits<i64>)).unbox(),
                       5);
        }
    }
}
