use gc::Collector;
use value::{CtrValue, ConcreteType,
            Any, Bits, ListEmpty, Type, Symbol,
            Expr, Do, Const, ExprCont, DoCont, Halt};
use refs::{Root, WeakRoot, ValueHandle, ValuePtr};

use std::cmp::Ordering;
use std::ptr;
use std::mem;
use std::slice;
use std::iter;

trait Eval {
    fn eval(self, itp: &mut Interpreter, k: Root<Any>) -> Result<State, CtrError>;
}

trait Continuation {
    fn continu(self, itp: &mut Interpreter, v: Root<Any>) -> Result<State, CtrError>;
}

trait Primop {
    fn exec(self, itp: &mut Interpreter) -> Result<State, CtrError>;
}

/// An `Interpreter` holds all the Centring state. This arrangement is inspired
/// by `lua_State` in PUC Lua.
pub struct Interpreter {
    gc: Collector,
    stack_roots: Vec<WeakRoot>,
    pub type_t: Root<Type>,
    pub pair_t: Root<Type>,
    pub nil_t: Root<Type>,
    pub array_mut_t: Root<Type>,
    pub int_t: Root<Type>,
    pub uint_t: Root<Type>,
    pub bool_t: Root<Type>,
    pub voidptr_t: Root<Type>,
    pub symbol_t: Root<Type>,
    pub string_t: Root<Type>,
    pub env_t: Root<Type>,
    pub env_bucket_t: Root<Type>,
    pub expr_t: Root<Type>,
    pub do_t: Root<Type>,
    pub const_t: Root<Type>,
    pub exprcont_t: Root<Type>,
    pub docont_t: Root<Type>,
    pub halt_t: Root<Type>,
}

pub enum State {
    Eval(Root<Any>, Root<Any>),
    Cont(Root<Any>, Root<Any>),
    Halt(Root<Any>),
}

#[derive(Debug)]
pub enum CtrError {
    Argc {
        expected: (Ordering, usize),
        received: usize,
    },
    UnknownSf(String),
    UnknownIntr(String),
    ImproperList(Root<Any>),
    Index(usize, usize),
    Type(Root<Type>),
    SetUnbound(Root<Symbol>)
}

pub type CtrResult<T> = Result<Root<T>, CtrError>;

impl Interpreter {
    /// Make a new `Interpreter` to execute Centring with.
    pub fn new() -> Interpreter {
        let mut itp = Interpreter {
            gc: Collector::new(1024),
            stack_roots: vec![],
            type_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            pair_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            array_mut_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            nil_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            int_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            uint_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            bool_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            voidptr_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            symbol_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            string_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            env_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            env_bucket_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            expr_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            do_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            const_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            exprcont_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            docont_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            halt_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
        };

        // TODO: structure for the types
        let type_t = Type::new(&mut itp);
        itp.type_t = type_t.clone();
        itp.pair_t = Type::new(&mut itp);
        itp.array_mut_t = Type::new(&mut itp);
        itp.nil_t = Type::new(&mut itp);
        itp.int_t = Type::new(&mut itp);
        itp.uint_t = Type::new(&mut itp);
        itp.bool_t = Type::new(&mut itp);
        itp.voidptr_t = Type::new(&mut itp);
        itp.symbol_t = Type::new(&mut itp);
        itp.string_t = Type::new(&mut itp);
        itp.env_t = Type::new(&mut itp);
        itp.env_bucket_t = Type::new(&mut itp);
        itp.expr_t = Type::new(&mut itp);
        itp.do_t = Type::new(&mut itp);
        itp.const_t = Type::new(&mut itp);
        itp.exprcont_t = Type::new(&mut itp);
        itp.docont_t = Type::new(&mut itp);
        itp.halt_t = Type::new(&mut itp);

        itp.type_t.clone().as_any_ref().set_type(type_t.borrow());

        itp
    }

    pub fn interpret(&mut self, ast: ValueHandle<Any>) -> CtrResult<Any> {
        let mut state = State::Eval(ast.root(), Halt::new(self).as_any_ref());
        loop {
            // Need to use a trampoline/state machine here for manual TCO.
            state = match state {
                State::Eval(ctrl, k) => try!(self.eval(ctrl, k)),
                State::Cont(ctrl, k) => try!(self.cont(ctrl, k)),
                State::Halt(v) => return Ok(v),
            };
        }
    }

    fn eval(&mut self, ctrl: Root<Any>, k: Root<Any>) -> Result<State, CtrError> {
        let ctrl = ctrl.borrow();
        if let Some(expr) = ctrl.downcast::<Expr>(self) {
            expr.eval(self, k)
        } else if let Some(d) = ctrl.downcast::<Do>(self) {
            d.eval(self, k)
        } else if let Some(c) = ctrl.downcast::<Const>(self) {
            c.eval(self, k)
        } else {
            unimplemented!()
        }
    }

    fn cont(&mut self, v: Root<Any>, k: Root<Any>) -> Result<State, CtrError> {
        let k = k.borrow();
        if let Some(halt) = k.downcast::<Halt>(self) {
            halt.continu(self, v)
        } else if let Some(ec) = k.downcast::<ExprCont>(self) {
            ec.continu(self, v)
        } else if let Some(dc) = k.downcast::<DoCont>(self) {
            dc.continu(self, v)
        } else {
            unimplemented!()
        }
    }

    pub fn alloc_rec<'a, T, I>(&mut self, typ: ValueHandle<Type>, fields: I)
        -> Root<T> where T: CtrValue, I: Iterator<Item=Root<Any>> + ExactSizeIterator {
        while !self.gc.rec_poll(fields.len()) {
            self.mark_roots();
            unsafe { self.gc.collect(); }
        }
        let res = unsafe { Root::new(self.gc.alloc_rec(typ, fields)) };
        self.stack_roots.push(Root::downgrade(&res));
        res
    }

    pub fn alloc_bytes<T: CtrValue>(&mut self, typ: ValueHandle<Type>, data: &[u8]) -> Root<T> {
        if !self.gc.bytes_poll(data.len()) {
            self.mark_roots();
            unsafe { self.gc.collect(); }
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

impl<'a> Eval for ValueHandle<'a, Expr> {
    fn eval(self, itp: &mut Interpreter, k: Root<Any>) -> Result<State, CtrError> {
        if let Some(arg) = self.args(0) {
            Ok(State::Eval(arg, ExprCont::new(itp, k, self.root(), 0, self.args_iter())
                .as_any_ref()))
        } else {
            let l = ExprCont::new(itp, k, self.root(), 0, iter::empty());
            l.borrow().exec(itp)
        }
    }
}

impl<'a> Eval for ValueHandle<'a, Do> {
    fn eval(self, itp: &mut Interpreter, k: Root<Any>) -> Result<State, CtrError> {
        if let Some(stmt) = self.stmts(0) {
            Ok(State::Eval(stmt, DoCont::new(itp, k, self.root(), 0).as_any_ref()))
        } else {
            // TODO: continue with a tuple:
            Ok(State::Cont(ListEmpty::new(itp).as_any_ref(), k))
        }
    }
}

impl<'a> Eval for ValueHandle<'a, Const> {
    fn eval(self, _: &mut Interpreter, k: Root<Any>) -> Result<State, CtrError> {
        Ok(State::Cont(self.val(), k))
    }
}

impl<'a> Continuation for ValueHandle<'a, DoCont> {
    fn continu(self, itp: &mut Interpreter, v: Root<Any>) -> Result<State, CtrError> {
        if let Some(mut i) = self.index(itp) {
            i += 1;
            if let Some(d) = self.do_ast(itp) {
                if let Some(stmt) = d.borrow().stmts(i) {
                    let new_k = DoCont::new(itp, self.parent(), d, i);
                    Ok(State::Eval(stmt, new_k.as_any_ref()))
                } else {
                    Ok(State::Cont(v, self.parent()))
                }
            } else {
                panic!()
            }
        } else {
            panic!()
        }
    }
}

impl<'a> Continuation for ValueHandle<'a, ExprCont> {
    fn continu(self, itp: &mut Interpreter, v: Root<Any>) -> Result<State, CtrError> {
        if let Some(i) = self.index(itp) {
            let j = i + 1;
            if let Some(e) = self.ast(itp) {
                let new_k = ExprCont::new(itp, self.parent(), e.clone(), j, self.args_iter());
                try!(new_k.borrow().set_arg(i, v));
                if let Some(arg) = e.borrow().args(j) {
                    Ok(State::Eval(arg, new_k.as_any_ref()))
                } else {
                    new_k.borrow().exec(itp)
                }
            } else {
                panic!()
            }
        } else {
            panic!()
        }
    }
}

impl<'a> Continuation for ValueHandle<'a, Halt> {
    fn continu(self, _: &mut Interpreter, v: Root<Any>) -> Result<State, CtrError> {
        Ok(State::Halt(v))
    }
}

impl<'a> Primop for ValueHandle<'a, ExprCont> {
    fn exec(self, itp: &mut Interpreter) -> Result<State, CtrError> {
        if let Some(expr) = self.ast(itp) {
            let res = try!(expr.borrow().op(itp)(itp, self.args_iter()));
            Ok(State::Cont(res, self.parent()))
        } else {
            Err(CtrError::Type(Expr::typ(itp).root()))
        }
    }
}

/// # Tests
#[cfg(test)]
mod tests {
    use super::Interpreter;
    use value::{ListPair, Int, Unbox};

    #[test]
    fn collect() {
        let mut itp = Interpreter::new();
        let a = Int::new(&mut itp, 3);
        let b = Int::new(&mut itp, 5);
        let tup = ListPair::new(&mut itp, a.clone().as_any_ref(), b.as_any_ref());
        itp.mark_roots();
        unsafe {
            itp.gc.collect();
            assert_eq!(a.unbox(), 3);
        }
    }
}
