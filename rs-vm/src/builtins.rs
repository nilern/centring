use value::ValueRef;
use vm::{VMProcess, VMResult};

pub type NativeFnCode = fn(&mut VMProcess);
