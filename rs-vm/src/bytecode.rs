
#[derive(Clone, Copy)]
pub struct Bytecode(u32);

pub const BRF: u8 = 0x00;
pub const FN: u8 = 0x01;
pub const CALL: u8 = 0x02;

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
