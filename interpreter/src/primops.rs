use interpreter::{Interpreter, CtrResult, CtrError};
use interpreter::CtrError::Argc;
use value::{CtrValue, Unbox, ConcreteType, IndexedFields, Any, Type, Int, ExprCont};
use refs::Root;

use std::cmp::Ordering::{Greater, Equal};

type ExprArgIter = IndexedFields<ExprCont>;
pub type ExprFn = fn(&mut Interpreter, ExprArgIter) -> CtrResult<Any>;

fn rec<T: CtrValue>(itp: &mut Interpreter, args: &[Root<Any>]) -> CtrResult<T> {
    if args.len() > 0 {
        if let Some(t) = args[0].borrow().downcast::<Type>(itp) {
            Ok(itp.alloc_rec(t, args[1..].into_iter().cloned()))
        } else {
            Err(CtrError::Type(Type::typ(itp).root()))
        }
    } else {
        Err(Argc {
            expected: (Greater, 0),
            received: args.len(),
        })
    }
}

pub fn iadd(itp: &mut Interpreter, mut args: ExprArgIter) -> CtrResult<Any> {
    if args.len() == 2 {
        if let Some(a) = args.next().unwrap().borrow().downcast::<Int>(itp) {
            if let Some(b) = args.next().unwrap().borrow().downcast::<Int>(itp) {
                Ok(Int::new(itp, a.unbox() + b.unbox()).as_any_ref())
            } else {
                Err(CtrError::Type(Int::typ(itp).root()))
            }
        } else {
            Err(CtrError::Type(Int::typ(itp).root()))
        }
    } else {
        Err(Argc {
            expected: (Equal, 2),
            received: args.len(),
        })
    }
}
