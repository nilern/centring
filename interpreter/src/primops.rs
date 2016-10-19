use interpreter::{Interpreter, CtrResult};
use interpreter::CtrError::Argc;
use refs::ValueHandle;

use std::cmp::Ordering::Greater;

fn rec(itp: &mut Interpreter, args: &[ValueHandle]) -> CtrResult {
    if args.len() > 0 {
        Ok(itp.alloc_rec(args[0].clone(), &args[1..]))
    } else {
        Err(Argc {
            expected: (Greater, 0),
            received: args.len()
        })
    }
}
