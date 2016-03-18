
#[derive(Debug)]
pub enum Bytecode {
    Arg(usize),
    Closed(usize),
    Const(usize),

    Brf(usize),

    Fn(usize, usize),
    
    Call(usize)
}

pub use self::Bytecode::*;
