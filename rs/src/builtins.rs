use value::{Value, ValueRef};
use eval::Interpreter;
use std::rc::Rc;

pub fn add_2i(_: &mut Interpreter, args: Vec<ValueRef>) -> ValueRef {
    let a = args[0].get_int().unwrap();
    let b = args[1].get_int().unwrap();
    Rc::new(Value::Int(a + b))
}
