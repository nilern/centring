use interpreter::{Interpreter, CtrResult};
use value::{Downcast, Any, Int};
use refs::ValueHandle;

pub fn analyze(itp: &mut Interpreter, v: ValueHandle<Any>) -> CtrResult<Any> {
    let on: ValueHandle<Int> = v.downcast(itp).unwrap();
    Ok(itp.alloc_const(v).as_any_ref())
}
