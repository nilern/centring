use gc::Collector;
use value::{CtrValue,
            Any, Bits, ListEmpty, Array, Type, FnClosure, Env,
            FnNode, App, Def, Expr, Stmt, Ctrl, Closure, Do, Var, Const,
            FnCont, ArgCont, DefCont, ExprCont, StmtCont, CtrlCont, DoCont, Halt};
use refs::{Root, WeakRoot, ValueHandle, ValuePtr};
use primops;
use primops::{ExprFn, StmtFn, CtrlFn};

use std::collections::HashMap;
use std::cmp::Ordering;
use std::ptr;
use std::mem;
use std::slice;
use std::iter;

trait Primop<T: CtrValue> {
    fn apply_primop<'a>(&mut self, k: ValueHandle<'a, T>) -> Result<State, CtrError>;
}

/// An `Interpreter` holds all the Centring state. This arrangement is inspired
/// by `lua_State` in PUC Lua.
pub struct Interpreter {
    gc: Collector,
    stack_roots: Vec<WeakRoot>,
    pub type_t: Root<Type>,
    pub pair_t: Root<Type>,
    pub nil_t: Root<Type>,
    pub array_t: Root<Type>,
    pub array_mut_t: Root<Type>,
    pub int_t: Root<Type>,
    pub uint_t: Root<Type>,
    pub bool_t: Root<Type>,
    pub voidptr_t: Root<Type>,
    pub symbol_t: Root<Type>,
    pub string_t: Root<Type>,
    pub fn_t: Root<Type>,
    pub env_t: Root<Type>,
    pub env_bucket_t: Root<Type>,
    pub fn_node_t: Root<Type>,
    pub app_t: Root<Type>,
    pub def_t: Root<Type>,
    pub expr_t: Root<Type>,
    pub stmt_t: Root<Type>,
    pub ctrl_t: Root<Type>,
    pub closure_t: Root<Type>,
    pub do_t: Root<Type>,
    pub var_t: Root<Type>,
    pub const_t: Root<Type>,
    pub fncont_t: Root<Type>,
    pub argcont_t: Root<Type>,
    pub defcont_t: Root<Type>,
    pub exprcont_t: Root<Type>,
    pub stmtcont_t: Root<Type>,
    pub ctrlcont_t: Root<Type>,
    pub docont_t: Root<Type>,
    pub halt_t: Root<Type>,
    pub exprfns: HashMap<&'static str, ExprFn>,
    pub stmtfns: HashMap<&'static str, StmtFn>,
    pub ctrlfns: HashMap<&'static str, CtrlFn>
}

pub enum State {
    Eval(Root<Any>, Root<Env>, Root<Any>),
    Cont(Root<Any>, Root<Any>),
    Halt(Root<Any>),
}

#[derive(Debug, Clone)]
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
    SetUnbound(String),
    GetUnbound(String),
    DefArgs,
    FnArgs,
    FnCase,
    ErrIntr(Root<Any>, Root<Any>)
}

pub type CtrResult<T> = Result<Root<T>, CtrError>;

impl Interpreter {
    /// Make a new `Interpreter` to execute Centring with.
    pub fn new() -> Interpreter {
        // FIXME: Should be possible to avoid all these nulls. They crash the collector...
        let mut itp = Interpreter {
            gc: Collector::new(1024),
            stack_roots: vec![],
            type_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            pair_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            array_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            array_mut_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            nil_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            int_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            uint_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            bool_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            voidptr_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            symbol_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            string_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            fn_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            env_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            env_bucket_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            fn_node_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            app_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            def_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            expr_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            stmt_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            ctrl_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            closure_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            do_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            var_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            const_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            fncont_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            argcont_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            exprcont_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            stmtcont_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            ctrlcont_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            defcont_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            docont_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            halt_t: unsafe { Root::new(ptr::null::<Any>() as ValuePtr) },
            exprfns: HashMap::new(),
            stmtfns: HashMap::new(),
            ctrlfns: HashMap::new()
        };

        // TODO: structure for the types
        let type_t = Type::new(&mut itp);
        itp.type_t = type_t.clone();
        itp.pair_t = Type::new(&mut itp);
        itp.array_t = Type::new(&mut itp);
        itp.array_mut_t = Type::new(&mut itp);
        itp.nil_t = Type::new(&mut itp);
        itp.int_t = Type::new(&mut itp);
        itp.uint_t = Type::new(&mut itp);
        itp.bool_t = Type::new(&mut itp);
        itp.voidptr_t = Type::new(&mut itp);
        itp.symbol_t = Type::new(&mut itp);
        itp.string_t = Type::new(&mut itp);
        itp.fn_t = Type::new(&mut itp);
        itp.env_t = Type::new(&mut itp);
        itp.env_bucket_t = Type::new(&mut itp);
        itp.fn_node_t = Type::new(&mut itp);
        itp.app_t = Type::new(&mut itp);
        itp.def_t = Type::new(&mut itp);
        itp.expr_t = Type::new(&mut itp);
        itp.stmt_t = Type::new(&mut itp);
        itp.ctrl_t = Type::new(&mut itp);
        itp.closure_t = Type::new(&mut itp);
        itp.do_t = Type::new(&mut itp);
        itp.var_t = Type::new(&mut itp);
        itp.const_t = Type::new(&mut itp);
        itp.fncont_t = Type::new(&mut itp);
        itp.argcont_t = Type::new(&mut itp);
        itp.defcont_t = Type::new(&mut itp);
        itp.exprcont_t = Type::new(&mut itp);
        itp.stmtcont_t = Type::new(&mut itp);
        itp.ctrlcont_t = Type::new(&mut itp);
        itp.docont_t = Type::new(&mut itp);
        itp.halt_t = Type::new(&mut itp);

        itp.type_t.clone().as_any_ref().set_type(type_t.borrow());

        itp.exprfns.insert("rec", primops::rec);
        itp.exprfns.insert("rref", primops::rref);
        itp.exprfns.insert("iadd", primops::iadd);
        itp.stmtfns.insert("err", primops::err);
        itp.ctrlfns.insert("brf", primops::brf);

        itp
    }

    pub fn interpret(&mut self, ast: ValueHandle<Any>, env: Root<Env>) -> CtrResult<Any> {
        let mut state = State::Eval(ast.root(), env, Halt::new(self).as_any_ref());
        loop {
            // Need to use a trampoline/state machine here for manual TCO.
            state = match state {
                State::Eval(ctrl, env, k) => try!(self.eval(ctrl, env, k)),
                State::Cont(ctrl, k) => try!(self.cont(ctrl, k)),
                State::Halt(v) => return Ok(v),
            };
        }
    }

    fn eval(&mut self, ctrl: Root<Any>, env: Root<Env>, k: Root<Any>) -> Result<State, CtrError> {
        let ctrl = ctrl.borrow();
        typecase!(ctrl, self; {
            ctrl: FnNode => {
                let name = try!(ctrl.name(self));
                let formal = try!(ctrl.formal(self));
                let body = Closure::new(self, env.clone(), // TODO: only do this for mono-fns
                                             ctrl.case(0).unwrap().get(1).unwrap()).as_any_ref();
                let cases = ctrl.case_iter()
                                .map(|case| {
                                    assert_eq!(case.len(), 2);
                                    let cond = case.get(0).unwrap();
                                    let body = case.get(1).unwrap();
                                    Array::new(self,
                                        vec![cond, body, env.clone().as_any_ref()].into_iter())
                                })
                                .collect::<Vec<_>>();
                Ok(State::Cont(FnClosure::new(self, name, formal, body,
                    cases.into_iter()).as_any_ref(), k))
            },
            ctrl: App => {
                let l = FnCont::new(self, k, ctrl.root(), env.clone());
                Ok(State::Eval(ctrl.callee(), env, l.as_any_ref()))
            },
            ctrl: Def => {
                let name = try!(ctrl.name(self));
                let l = DefCont::new(self, k, name, env.clone());
                Ok(State::Eval(ctrl.value(), env, l.as_any_ref()))
            },
            ctrl: Expr => {
                if let Some(arg) = ctrl.args(0) {
                    let l = ExprCont::new(self, k, ctrl.root(), 0, env.clone(), ctrl.args_iter());
                    Ok(State::Eval(arg.clone(), env, l.as_any_ref()))
                } else {
                    let l = ExprCont::new(self, k, ctrl.root(), 0, env, iter::empty());
                    self.apply_primop(l.borrow())
                }
            },
            ctrl: Stmt => {
                if let Some(arg) = ctrl.args(0) {
                    let l = StmtCont::new(self, k, ctrl.root(), 0, env.clone(), ctrl.args_iter());
                    Ok(State::Eval(arg.clone(), env, l.as_any_ref()))
                } else {
                    let l = StmtCont::new(self, k, ctrl.root(), 0, env, iter::empty());
                    self.apply_primop(l.borrow())
                }
            },
            ctrl: Ctrl => {
                let l = CtrlCont::new(self, k, ctrl.root(), env.clone());
                Ok(State::Eval(ctrl.determinant(), env, l.as_any_ref()))
            },
            ctrl: Closure => {
                let mut senv = try!(ctrl.env(self));
                senv = try!(env.borrow().concat(self, senv.borrow()));
                Ok(State::Eval(ctrl.expr(), senv, k))
            },
            ctrl: Do => {
                if let Some(stmt) = ctrl.stmts(0) {
                    let l = DoCont::new(self, k, ctrl.root(), 0, env.clone());
                    Ok(State::Eval(stmt, env, l.as_any_ref()))
                } else {
                    // TODO: continue with a tuple:
                    Ok(State::Cont(ListEmpty::new(self).as_any_ref(), k))
                }
            },
            ctrl: Var => {
                env.lookup(self, try!(ctrl.name(self)).borrow()).map(|v| State::Cont(v, k))
            },
            ctrl: Const => {
                Ok(State::Cont(ctrl.val(), k))
            },
            _ => { unimplemented!() }
        })
    }

    fn cont(&mut self, v: Root<Any>, k: Root<Any>) -> Result<State, CtrError> {
        let k = k.borrow();
        typecase!(k, self; {
            k: FnCont => {
                let env = try!(k.env(self));
                let l = ArgCont::new(self, k.parent(), v, env.clone());
                let arg = try!(k.ast(self)).arg();
                Ok(State::Eval(arg, env, l.as_any_ref()))
            },
            k: ArgCont => {
                let callee = k.callee();
                typecase!(callee.borrow(), self; {
                    f: FnClosure => {
                        let name = try!(f.name(self));
                        let formal = try!(f.formal(self));
                        let env = Env::new(self, None);
                        try!(env.borrow().def(self, name.borrow(), f.as_any_ref()));
                        try!(env.borrow().def(self, formal.borrow(), v.borrow()));
                        Ok(State::Eval(f.body(), env, k.parent()))
                    },
                    _ => { unimplemented!() }
                })
            },
            k: DefCont => {
                try!(k.env(self).and_then(|env| {
                    let name = try!(k.name(self));
                    env.borrow().def(self, name.borrow(), v.borrow())
                }));
                // TODO: continue with a tuple:
                Ok(State::Cont(ListEmpty::new(self).as_any_ref(), k.parent()))
            },
            k: ExprCont => {
                let i = try!(k.index(self)); // v is the value of the i:th argument.
                let j = i + 1;                 // The index of the next argument to evaluate
                let ast = try!(k.ast(self));
                let env = try!(k.env(self));
                let new_k =
                    ExprCont::new(self, k.parent(), ast.clone(), j, env.clone(), k.args_iter());
                try!(new_k.borrow().set_arg(i, v)); // The new continuation knows that args[i] => v
                if let Some(arg) = ast.borrow().args(j) {
                    Ok(State::Eval(arg, env, new_k.as_any_ref())) // Eval the next arg.
                } else {
                    self.apply_primop(new_k.borrow())
                }
            },
            k: StmtCont => {
                let i = try!(k.index(self)); // v is the value of the i:th argument.
                let j = i + 1;                    // The index of the next argument to evaluate
                let ast = try!(k.ast(self));
                let env = try!(k.env(self));
                let new_k =
                    StmtCont::new(self, k.parent(), ast.clone(), j, env.clone(), k.args_iter());
                try!(new_k.borrow().set_arg(i, v)); // The new continuation knows that args[i] => v.
                if let Some(arg) = ast.borrow().args(j) {
                    Ok(State::Eval(arg, env, new_k.as_any_ref())) // Eval the next arg.
                } else {
                    self.apply_primop(new_k.borrow())
                }
            },
            k: CtrlCont => {
                let branch = try!(k.ast(self).and_then(|ctrl|
                    ctrl.borrow().op(self)(self, v.borrow(), ctrl.args_iter())));
                Ok(State::Eval(branch, try!(k.env(self)), k.parent()))
            },
            k: DoCont => {
                let i = try!(k.index(self)); // v is the value of the i:th argument.
                let j = i + 1;                    // The index of the next argument to evaluate.
                let ast = try!(k.do_ast(self));
                if let Some(stmt) = ast.borrow().stmts(j) {
                    // Ignore v and move on to evaluate the next statement:
                    let env = try!(k.env(self));
                    let new_k = DoCont::new(self, k.parent(), ast, j, env.clone());
                    Ok(State::Eval(stmt, env, new_k.as_any_ref()))
                } else {
                    Ok(State::Cont(v, k.parent())) // This was the last statement so continue with v.
                }
            },
            k: Halt => {
                Ok(State::Halt(v))
            },
            _ => { unimplemented!() }
        })
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

impl Primop<ExprCont> for Interpreter {
    fn apply_primop<'a>(&mut self, k: ValueHandle<'a, ExprCont>) -> Result<State, CtrError> {
        let res = try!(k.ast(self).and_then(|expr|
            expr.borrow().op(self)(self, k.args_iter())));
        Ok(State::Cont(res, k.parent()))
    }
}

impl Primop<StmtCont> for Interpreter {
    fn apply_primop<'a>(&mut self, k: ValueHandle<'a, StmtCont>) -> Result<State, CtrError> {
        try!(k.ast(self).and_then(|expr| expr.borrow().op(self)(self, k.args_iter())));
        // TODO: continue with a tuple:
        Ok(State::Cont(ListEmpty::new(self).as_any_ref(), k.parent()))
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
