use interpreter::Interpreter;
use refs::{Root, ValueHandle};

fn rec(itp: &mut Interpreter, typ: ValueHandle, fields: &[ValueHandle]) -> Root {
    itp.alloc_rec(typ, fields)
}
