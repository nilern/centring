
#[derive(Debug, Clone, Copy)]
pub struct Bytecode(u32);

pub const CLOSED: u8 = 0x00;
pub const CONSTANT: u8 = 0x01;

pub const BRF: u8 = 0x10;
pub const FN: u8 = 0x11;
pub const CALL: u8 = 0x12;

pub fn bc(op: u8, arg: u32) -> Bytecode {
    Bytecode(((op as u32) << 24) | arg)
}

// pub enum Instruction {
//     Def(u16),

//     Bound(u16),
//     Free(u16),
//     Const(u16),
//     Global(u16),

//     Brf(u16),

//     Fn(u16),

//     Call(u16)
// }
