use interpreter::{Interpreter, CtrResult, CtrError};
use interpreter::CtrError::Argc;
use value::{CtrValue, Unbox, ConcreteType, IndexedFields, Any, Type, Int, Bool,
            Ctrl, ExprCont, StmtCont};
use refs::{Root, ValueHandle, ValuePtr};

use std::cmp::Ordering::{Greater, Equal};
use std::slice;

type ExprArgIter = IndexedFields<ExprCont>;
pub type ExprFn = fn(&mut Interpreter, ExprArgIter) -> CtrResult<Any>;

type StmtArgIter = IndexedFields<StmtCont>;
pub type StmtFn = fn(&mut Interpreter, StmtArgIter) -> CtrResult<Any>;

type CtrlBranchIter = IndexedFields<Ctrl>;
pub type CtrlFn = fn(&mut Interpreter, ValueHandle<Any>, CtrlBranchIter) -> CtrResult<Any>;

pub fn rec(itp: &mut Interpreter, mut args: ExprArgIter) -> CtrResult<Any> {
    if args.len() > 0 {
        typecase!(args.next().unwrap().borrow(), itp; {
            t: Type => { Ok(itp.alloc_rec(t, args)) },
            _ => { Err(CtrError::Type(Type::typ(itp).root())) }
        })
    } else {
        Err(Argc {
            expected: (Greater, 0),
            received: args.len(),
        })
    }
}

pub fn rref(itp: &mut Interpreter, mut args: ExprArgIter) -> CtrResult<Any> {
    if args.len() == 2 {
        let rec = args.next().unwrap();
        let i = args.next().unwrap();
        typecase!(i, itp; {
            i: Int => {
                let i = i.unbox();
                assert!(i >= 0); // FIXME
                let fields = unsafe { slice::from_raw_parts(
                    (rec.ptr() as *mut ValuePtr).offset(2),
                    rec.alloc_len())
                };
                Ok(unsafe { Root::new(fields[i as usize]) })
            },
            _ => { Err(CtrError::Type(Int::typ(itp).root())) }
        })
    } else {
        Err(Argc {
            expected: (Equal, 2),
            received: args.len(),
        })
    }
}

pub fn brf(itp: &mut Interpreter, cond: ValueHandle<Any>, mut branches: CtrlBranchIter)
           -> CtrResult<Any> {
    if branches.len() == 2 {
        typecase!(cond, itp; {
            b: Bool => {
                if b.unbox() {
                    Ok(branches.next().unwrap())
                } else {
                    branches.next();
                    Ok(branches.next().unwrap())
                }
            },
            _ => { Ok(branches.next().unwrap()) }
        })
    } else {
        Err(Argc {
            expected: (Equal, 2),
            received: branches.len(),
        })
    }
}

pub fn err(_: &mut Interpreter, mut args: StmtArgIter) -> CtrResult<Any> {
    if args.len() == 2 {
        Err(CtrError::ErrIntr(args.next().unwrap(), args.next().unwrap()))
    } else {
        Err(Argc {
            expected: (Equal, 2),
            received: args.len(),
        })
    }
}

pub fn iadd(itp: &mut Interpreter, mut args: ExprArgIter) -> CtrResult<Any> {
    if args.len() == 2 {
        typecase!(args.next().unwrap().borrow(), itp; {
            a: Int => {
                typecase!(args.next().unwrap().borrow(), itp; {
                    b: Int => { Ok(Int::new(itp, a.unbox() + b.unbox()).as_any_ref()) },
                    _ => { Err(CtrError::Type(Int::typ(itp).root())) }
                })
            },
            _ => { Err(CtrError::Type(Int::typ(itp).root())) }
        })
    } else {
        Err(Argc {
            expected: (Equal, 2),
            received: args.len(),
        })
    }
}
