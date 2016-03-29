
#[derive(Debug, Clone, Copy)]
pub enum Bytecode {
    Const(u32),
    Local(u32),

    AddI(u8, u8),
    SubI(u8, u8),
    MulI(u8, u8),
    DivI(u8, u8)
    
    // Arg(usize),
    // Closed(usize),
    // Const(usize),

    // Brf(usize),

    // Fn(usize, usize),
    
    // Call(usize)
}

pub use self::Bytecode::*;
