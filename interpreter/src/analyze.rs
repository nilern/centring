use interpreter::{Interpreter, CtrResult};
use value::{ConcreteType, Downcast, Any, Int, Const};
use refs::{Root, ValueHandle};

pub fn analyze(itp: &mut Interpreter, v: ValueHandle<Any>) -> CtrResult<Any> {
    if v.instanceof(Int::typ(itp)) {
        Ok(itp.alloc_const(v).as_any_ref())
    } else {
        unimplemented!()
    }
}

pub fn ast_to_sexpr(itp: &mut Interpreter, ast: ValueHandle<Any>) -> CtrResult<Any> {
    let oc: Option<ValueHandle<Const>> = ast.downcast(itp);
    if let Some(c) = oc {
        Ok(unsafe { Root::new(c.val) })
    } else {
        unimplemented!()
    }
}
