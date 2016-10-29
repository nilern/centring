use interpreter::{Interpreter, CtrResult, CtrError};
use interpreter::CtrError::Argc;
use value::{CtrValue, ConcreteType, Any, Type};
use refs::Root;

use std::cmp::Ordering::Greater;

pub type ExprFn<I> = fn(&mut Interpreter, I) -> CtrResult<Any>;

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
