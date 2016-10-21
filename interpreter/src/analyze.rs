use interpreter::{Interpreter, CtrResult};
use value::{TypePtr, Any, Int};
use refs::ValueHandle;

pub fn analyze(itp: &mut Interpreter, v: ValueHandle<Any>) -> CtrResult<Any> {
    if v.instanceof(Int::typ(itp)) {
        Ok(itp.alloc_const(v).as_any_ref())
    } else {
        unimplemented!()
    }
}
