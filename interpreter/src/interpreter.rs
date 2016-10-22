use gc::Collector;
use value::{CtrValue, ConcreteType, Downcast, Any, Bits, Int, ListPair, ListEmpty, Type, Const,
            Halt};
use refs::{Root, WeakRoot, ValueHandle, ValuePtr};

use std::cmp::Ordering;
use std::ptr;
use std::mem;
use std::slice;

/// An `Interpreter` holds all the Centring state. This arrangement is inspired
/// by `lua_State` in PUC Lua.
pub struct Interpreter {
    gc: Collector,
    stack_roots: Vec<WeakRoot>,
    pub type_t: Root<Type>,
    pub pair_t: Root<Type>,
    pub nil_t: Root<Type>,
    pub int_t: Root<Type>,
    pub const_t: Root<Type>,
    pub halt_t: Root<Type>,
}

enum InterpreterState {
    Eval(Root<Any>, Root<Any>),
    Cont(Root<Any>, Root<Any>),
    Halt(Root<Any>),
}

pub enum CtrError {
    Argc {
        expected: (Ordering, usize),
        received: usize,
    },
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
            const_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            halt_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
        };

        let type_t = itp.alloc_type();
        itp.type_t = type_t.clone();
        itp.pair_t = itp.alloc_type();
        itp.nil_t = itp.alloc_type();
        itp.int_t = itp.alloc_type();
        itp.const_t = itp.alloc_type();
        itp.halt_t = itp.alloc_type();

        itp.type_t.clone().as_any_ref().set_type(type_t.borrow());
        itp.pair_t.clone().as_any_ref().set_type(type_t.borrow());
        itp.nil_t.clone().as_any_ref().set_type(type_t.borrow());
        itp.int_t.clone().as_any_ref().set_type(type_t.borrow());
        itp.const_t.clone().as_any_ref().set_type(type_t.borrow());
        itp.halt_t.clone().as_any_ref().set_type(type_t.borrow());

        itp
    }

    pub fn interpret(&mut self, ast: ValueHandle<Any>) -> CtrResult<Any> {
        let mut state = InterpreterState::Eval(unsafe { Root::new(ast.ptr()) },
                                               self.alloc_halt().as_any_ref());
        loop {
            // Need to use a trampoline/state machine here for manual TCO.
            state = match state {
                InterpreterState::Eval(ctrl, k) => self.eval(ctrl, k),
                InterpreterState::Cont(ctrl, k) => self.cont(ctrl, k),
                InterpreterState::Halt(v) => return Ok(v),
            };
        }
    }

    fn eval(&mut self, ctrl: Root<Any>, k: Root<Any>) -> InterpreterState {
        let oc: Option<Root<Const>> = ctrl.downcast(self);
        if let Some(c) = oc {
            InterpreterState::Cont(unsafe { Root::new(c.val) }, k)
        } else {
            unimplemented!()
        }
    }

    fn cont(&mut self, v: Root<Any>, k: Root<Any>) -> InterpreterState {
        if k.borrow().instanceof(Halt::typ(self)) {
            InterpreterState::Halt(v)
        } else {
            unimplemented!()
        }
    }

    pub fn alloc_rec<'a, T: CtrValue>(&mut self,
                                      typ: ValueHandle<Type>,
                                      fields: &[ValueHandle<Any>])
                                      -> Root<T> {
        if !self.gc.rec_poll(fields.len()) {
            self.mark_roots();
            unsafe {
                self.gc.collect();
            }
        }
        let res = unsafe { Root::new(self.gc.alloc_rec(typ, fields)) };
        self.stack_roots.push(Root::downgrade(&res));
        res
    }

    pub fn alloc_bytes<T: CtrValue>(&mut self, typ: ValueHandle<Type>, data: &[u8]) -> Root<T> {
        if !self.gc.bytes_poll(data.len()) {
            self.mark_roots();
            unsafe {
                self.gc.collect();
            }
        }
        let res = unsafe { Root::new(self.gc.alloc_bytes(typ, data)) };
        self.stack_roots.push(Root::downgrade(&res));
        res
    }

    pub fn alloc_bits<T: Copy>(&mut self, typ: ValueHandle<Type>, data: T) -> Root<Bits<T>> {
        let data_bytes = unsafe {
            slice::from_raw_parts(mem::transmute::<&T, *mut u8>(&data), mem::size_of::<T>())
        };
        self.alloc_bytes(typ, data_bytes)
    }

    pub fn alloc_pair(&mut self, head: ValueHandle<Any>, tail: ValueHandle<Any>) -> Root<ListPair> {
        let typ = self.pair_t.clone();
        let fields = [head, tail];
        self.alloc_rec(typ.borrow(), &fields)
    }

    pub fn alloc_nil(&mut self) -> Root<ListEmpty> {
        let typ = self.nil_t.clone();
        let fields = [];
        self.alloc_rec(typ.borrow(), &fields)
    }

    pub fn alloc_type(&mut self) -> Root<Type> {
        let typ = self.type_t.clone();
        let fields = [];
        self.alloc_rec(typ.borrow(), &fields)
    }

    pub fn alloc_int(&mut self, v: isize) -> Root<Int> {
        let typ = self.int_t.clone();
        self.alloc_bits(typ.borrow(), v)
    }

    pub fn alloc_const(&mut self, v: ValueHandle<Any>) -> Root<Const> {
        let typ = self.const_t.clone();
        let fields = [v];
        self.alloc_rec(typ.borrow(), &fields)
    }

    pub fn alloc_halt(&mut self) -> Root<Halt> {
        let typ = self.halt_t.clone();
        let fields = [];
        self.alloc_rec(typ.borrow(), &fields)
    }

    fn mark_roots(&mut self) {
        let gc = &mut self.gc;
        // The following also takes care of Root members of self since they
        // were created by self.alloc*.
        self.stack_roots.retain(|whandle| {
            if let Some(mut handle) = whandle.upgrade::<Any>() {
                // Rust still has a live Root so this is a GC root and
                // needs to be marked and retained:
                handle.mark(gc);
                true
            } else {
                // The WeakRoot has lapsed so Rust no longer has live
                // Root:s to this and we can throw this away:
                false
            }
        });
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
        let tup = itp.alloc_pair(a.borrow().as_any_ref(), b.borrow().as_any_ref());
        itp.mark_roots();
        unsafe {
            itp.gc.collect();
            assert_eq!((*(a.ptr() as *const Bits<i64>)).unbox(), 3);
        }
    }
}
