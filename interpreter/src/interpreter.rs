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

trait Eval {
    fn eval(self, itp: &mut Interpreter, env: Root<Env>, k: Root<Any>) -> Result<State, CtrError>;
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
    pub global_env: Root<Env>,
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
            global_env: unsafe { Root::new(ptr::null::<Env>() as ValuePtr) },
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

        itp.global_env = Env::new(&mut itp, None);

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
            ctrl: FnNode | App | Def | Expr | Stmt | Ctrl | Closure | Do | Var | Const => {
                ctrl.eval(self, env, k)
            },
            _ => { unimplemented!() }
        })
    }

    fn cont(&mut self, v: Root<Any>, k: Root<Any>) -> Result<State, CtrError> {
        let k = k.borrow();
        typecase!(k, self; {
            k: FnCont | ArgCont | DefCont |
               ExprCont | StmtCont | CtrlCont | DoCont | Halt => { k.continu(self, v) },
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

impl<'a> Eval for ValueHandle<'a, FnNode> {
    fn eval(self, itp: &mut Interpreter, env: Root<Env>, k: Root<Any>) -> Result<State, CtrError> {
        let name = try!(self.name(itp));
        let formal = try!(self.formal(itp));
        let body = Closure::new(itp, env.clone(), // TODO: only do this for mono-fns
                                     self.case(0).unwrap().get(1).unwrap()).as_any_ref();
        let cases = self.case_iter()
                        .map(|case| {
                            assert_eq!(case.len(), 2);
                            let cond = case.get(0).unwrap();
                            let body = case.get(1).unwrap();
                            Array::new(itp, vec![cond, body, env.clone().as_any_ref()].into_iter())
                        })
                        .collect::<Vec<_>>();
        Ok(State::Cont(FnClosure::new(itp, name, formal, body, cases.into_iter()).as_any_ref(), k))
    }
}

impl<'a> Eval for ValueHandle<'a, App> {
    fn eval(self, itp: &mut Interpreter, env: Root<Env>, k: Root<Any>) -> Result<State, CtrError> {
        let l = FnCont::new(itp, k, self.root(), env.clone());
        Ok(State::Eval(self.callee(), env, l.as_any_ref()))
    }
}

impl<'a> Eval for ValueHandle<'a, Def> {
    fn eval(self, itp: &mut Interpreter, env: Root<Env>, k: Root<Any>) -> Result<State, CtrError> {
        let name = try!(self.name(itp));
        let l = DefCont::new(itp, k, name, env.clone());
        Ok(State::Eval(self.value(), env, l.as_any_ref()))
    }
}

impl<'a> Eval for ValueHandle<'a, Expr> {
    fn eval(self, itp: &mut Interpreter, env: Root<Env>, k: Root<Any>) -> Result<State, CtrError> {
        if let Some(arg) = self.args(0) {
            let l = ExprCont::new(itp, k, self.root(), 0, env.clone(), self.args_iter());
            Ok(State::Eval(arg.clone(), env, l.as_any_ref()))
        } else {
            let l = ExprCont::new(itp, k, self.root(), 0, env, iter::empty());
            l.borrow().exec(itp)
        }
    }
}

impl<'a> Eval for ValueHandle<'a, Stmt> {
    fn eval(self, itp: &mut Interpreter, env: Root<Env>, k: Root<Any>) -> Result<State, CtrError> {
        if let Some(arg) = self.args(0) {
            let l = StmtCont::new(itp, k, self.root(), 0, env.clone(), self.args_iter());
            Ok(State::Eval(arg.clone(), env, l.as_any_ref()))
        } else {
            let l = StmtCont::new(itp, k, self.root(), 0, env, iter::empty());
            l.borrow().exec(itp)
        }
    }
}

impl<'a> Eval for ValueHandle<'a, Ctrl> {
    fn eval(self, itp: &mut Interpreter, env: Root<Env>, k: Root<Any>) -> Result<State, CtrError> {
        let l = CtrlCont::new(itp, k, self.root(), env.clone());
        Ok(State::Eval(self.determinant(), env, l.as_any_ref()))
    }
}

impl<'a> Eval for ValueHandle<'a, Closure> {
    fn eval(self, itp: &mut Interpreter, env: Root<Env>, k: Root<Any>) -> Result<State, CtrError> {
        let mut senv = try!(self.env(itp));
        senv = try!(env.borrow().concat(itp, senv.borrow()));
        Ok(State::Eval(self.expr(), senv, k))
    }
}

impl<'a> Eval for ValueHandle<'a, Do> {
    fn eval(self, itp: &mut Interpreter, env: Root<Env>, k: Root<Any>) -> Result<State, CtrError> {
        if let Some(stmt) = self.stmts(0) {
            let l = DoCont::new(itp, k, self.root(), 0, env.clone());
            Ok(State::Eval(stmt, env, l.as_any_ref()))
        } else {
            // TODO: continue with a tuple:
            Ok(State::Cont(ListEmpty::new(itp).as_any_ref(), k))
        }
    }
}

impl<'a> Eval for ValueHandle<'a, Var> {
    fn eval(self, itp: &mut Interpreter, env: Root<Env>, k: Root<Any>) -> Result<State, CtrError> {
        env.lookup(itp, try!(self.name(itp)).borrow())
           .map(|v| State::Cont(v, k))
    }
}

impl<'a> Eval for ValueHandle<'a, Const> {
    fn eval(self, _: &mut Interpreter, _: Root<Env>, k: Root<Any>) -> Result<State, CtrError> {
        Ok(State::Cont(self.val(), k))
    }
}

impl<'a> Continuation for ValueHandle<'a, FnCont> {
    fn continu(self, itp: &mut Interpreter, v: Root<Any>) -> Result<State, CtrError> {
        let env = try!(self.env(itp));
        let l = ArgCont::new(itp, self.parent(), v, env.clone());
        let arg = try!(self.ast(itp)).arg();
        Ok(State::Eval(arg, env, l.as_any_ref()))
    }
}

impl<'a> Continuation for ValueHandle<'a, ArgCont> {
    fn continu(self, itp: &mut Interpreter, arg: Root<Any>) -> Result<State, CtrError> {
        let callee = self.callee();
        typecase!(callee.borrow(), itp; {
            f: FnClosure => {
                let name = try!(f.name(itp));
                let formal = try!(f.formal(itp));
                let env = Env::new(itp, None);
                try!(env.borrow().def(itp, name.borrow(), f.as_any_ref()));
                try!(env.borrow().def(itp, formal.borrow(), arg.borrow()));
                Ok(State::Eval(f.body(), env, self.parent()))
            },
            _ => { unimplemented!() }
        })
    }
}

impl<'a> Continuation for ValueHandle<'a, DefCont> {
    fn continu(self, itp: &mut Interpreter, v: Root<Any>) -> Result<State, CtrError> {
        try!(self.env(itp).and_then(|env| {
            let name = try!(self.name(itp));
            env.borrow().def(itp, name.borrow(), v.borrow())
        }));
        // TODO: continue with a tuple:
        Ok(State::Cont(ListEmpty::new(itp).as_any_ref(), self.parent()))
    }
}

impl<'a> Continuation for ValueHandle<'a, ExprCont> {
    fn continu(self, itp: &mut Interpreter, v: Root<Any>) -> Result<State, CtrError> {
        let i = try!(self.index(itp)); // v is the value of the i:th argument.
        let j = i + 1;                 // The index of the next argument to evaluate
        let ast = try!(self.ast(itp));
        let env = try!(self.env(itp));
        let new_k =
            ExprCont::new(itp, self.parent(), ast.clone(), j, env.clone(), self.args_iter());
        try!(new_k.borrow().set_arg(i, v)); // The new continuation knows that args[i] => v.
        if let Some(arg) = ast.borrow().args(j) {
            Ok(State::Eval(arg, env, new_k.as_any_ref())) // Eval the next arg.
        } else {
            new_k.borrow().exec(itp) // Run primop.
        }
    }
}

impl<'a> Continuation for ValueHandle<'a, StmtCont> {
    fn continu(self, itp: &mut Interpreter, v: Root<Any>) -> Result<State, CtrError> {
        let i = try!(self.index(itp)); // v is the value of the i:th argument.
        let j = i + 1;                    // The index of the next argument to evaluate
        let ast = try!(self.ast(itp));
        let env = try!(self.env(itp));
        let new_k =
            StmtCont::new(itp, self.parent(), ast.clone(), j, env.clone(), self.args_iter());
        try!(new_k.borrow().set_arg(i, v)); // The new continuation knows that args[i] => v.
        if let Some(arg) = ast.borrow().args(j) {
            Ok(State::Eval(arg, env, new_k.as_any_ref())) // Eval the next arg.
        } else {
            new_k.borrow().exec(itp) // Run primop.
        }
    }
}

impl<'a> Continuation for ValueHandle<'a, CtrlCont> {
    fn continu(self, itp: &mut Interpreter, v: Root<Any>) -> Result<State, CtrError> {
        let branch = try!(self.ast(itp).and_then(|ctrl|
            ctrl.borrow().op(itp)(itp, v.borrow(), ctrl.args_iter())));
        Ok(State::Eval(branch, try!(self.env(itp)), self.parent()))
    }
}

impl<'a> Continuation for ValueHandle<'a, DoCont> {
    fn continu(self, itp: &mut Interpreter, v: Root<Any>) -> Result<State, CtrError> {
        let i = try!(self.index(itp)); // v is the value of the i:th argument.
        let j = i + 1;                    // The index of the next argument to evaluate.
        let ast = try!(self.do_ast(itp));
        if let Some(stmt) = ast.borrow().stmts(j) {
            // Ignore v and move on to evaluate the next statement:
            let env = try!(self.env(itp));
            let new_k = DoCont::new(itp, self.parent(), ast, j, env.clone());
            Ok(State::Eval(stmt, env, new_k.as_any_ref()))
        } else {
            Ok(State::Cont(v, self.parent())) // This was the last statement so continue with v.
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
        let res = try!(self.ast(itp).and_then(|expr|
            expr.borrow().op(itp)(itp, self.args_iter())));
        Ok(State::Cont(res, self.parent()))
    }
}

impl<'a> Primop for ValueHandle<'a, StmtCont> {
    fn exec(self, itp: &mut Interpreter) -> Result<State, CtrError> {
        try!(self.ast(itp).and_then(|expr| expr.borrow().op(itp)(itp, self.args_iter())));
        // TODO: continue with a tuple:
        Ok(State::Cont(ListEmpty::new(itp).as_any_ref(), self.parent()))
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
