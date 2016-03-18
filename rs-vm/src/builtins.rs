use std::rc::Rc;

use value::Value;
use vm::{VMProcess, VMResult};

pub type NativeFnCode = fn(&mut VMProcess) -> VMResult;

// Function pointers for builtins

pub fn add_2i(fiber: &mut VMProcess) -> VMResult {
    let ret = fiber.arg(0);
    let a = fiber.arg(1).unwrap_int();
    let b = fiber.arg(2).unwrap_int();
    fiber.push(ret);
    fiber.push(Rc::new(Value::Int(a + b)));
    fiber.call(1)
}

pub fn sub_2i(fiber: &mut VMProcess) -> VMResult {
    let ret = fiber.arg(0);
    let a = fiber.arg(1).unwrap_int();
    let b = fiber.arg(2).unwrap_int();
    fiber.push(ret);
    fiber.push(Rc::new(Value::Int(a - b)));
    fiber.call(1)
}

pub fn mul_2i(fiber: &mut VMProcess) -> VMResult {
    let ret = fiber.arg(0);
    let a = fiber.arg(1).unwrap_int();
    let b = fiber.arg(2).unwrap_int();
    fiber.push(ret);
    fiber.push(Rc::new(Value::Int(a * b)));
    fiber.call(1)
}

pub fn lt_2i(fiber: &mut VMProcess) -> VMResult {
    let ret = fiber.arg(0);
    let a = fiber.arg(1).unwrap_int();
    let b = fiber.arg(2).unwrap_int();
    fiber.push(ret);
    fiber.push(Rc::new(Value::Bool(a < b)));
    fiber.call(1)
}

pub fn halt(fiber: &mut VMProcess) -> VMResult {
    fiber.halt();
    Ok(Rc::new(Value::Tuple(vec![])))
}
