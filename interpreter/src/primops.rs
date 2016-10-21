use interpreter::{Interpreter, CtrResult};
use interpreter::CtrError::Argc;
use value::CtrValue;
use refs::ValueHandle;

use std::clone::Clone;
use std::cmp::Ordering::Greater;
use std::mem;

fn rec<T: CtrValue>(itp: &mut Interpreter, args: &[ValueHandle<T>]) -> CtrResult<T> {
    if args.len() > 0 {
        unsafe {
            Ok(itp.alloc_rec(mem::transmute(args[0].clone()),
                             mem::transmute(&args[1..])))
        }
    } else {
        Err(Argc {
            expected: (Greater, 0),
            received: args.len()
        })
    }
}
