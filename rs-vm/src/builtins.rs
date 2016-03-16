use value::ValueRef;
use vm::{VMProcess, VMError};

pub type NativeFnCode = fn(&mut VMProcess, Vec<ValueRef>, Option<Vec<ValueRef>>)
                           -> Result<ValueRef, VMError>;
