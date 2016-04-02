use std::mem::transmute;

// TODO: use 4/2 bits to encode non-stack source operands:
// - 00 stack
// - 01 clover
// - 10 constant
// - 11 global

// Constants

pub const OP_BITS: u32 = 0x0000_00FF;
pub const ARG0_SHIFT: u32 = 8;
pub const ARG1_SHIFT: u32 = 16;

// Types

#[repr(u32)]
pub enum Opcode {
    Const,
    Local,
    Clover,
    // Global,

    AddI,
    SubI,
    MulI,
    DivI,

    // Brf,

    Fn,
    Call
}

#[derive(Debug, Clone, Copy)]
pub struct Bytecode(u32);

// Behaviour

macro_rules! bytecode_ctor {
    ($name:ident, $opcode:path, $a:ident) => {
        pub fn $name(i: u16) -> Bytecode {
            Bytecode((i as u32) << ARG0_SHIFT
                     | $opcode as u32)
        }
    };

    ($name:ident, $opcode:path, $a:ident, $b:ident) => {
        pub fn $name(i: u8, j: u8) -> Bytecode {
            Bytecode((j as u32) << ARG1_SHIFT
                     | (i as u32) << ARG0_SHIFT
                     | $opcode as u32)
        }
    };
}

bytecode_ctor!(cnst, Opcode::Const, i);
bytecode_ctor!(local, Opcode::Local, i);
bytecode_ctor!(clover, Opcode::Clover, i);

bytecode_ctor!(addi, Opcode::AddI, a, b);
bytecode_ctor!(subi, Opcode::SubI, a, b);
bytecode_ctor!(muli, Opcode::MulI, a, b);
bytecode_ctor!(divi, Opcode::DivI, a, b);

bytecode_ctor!(fun, Opcode::Fn, i);
bytecode_ctor!(call, Opcode::Call, n);

impl Bytecode {
    pub fn op(&self) -> Opcode {
        unsafe { transmute(self.0 & OP_BITS) }
    }

    pub fn arg(&self) -> usize {
        (self.0 >> ARG0_SHIFT) as usize
    }

    pub fn args(&self) -> (usize, usize) {
        let argbits = self.0 >> 8;
        let a = argbits & 0x00FF;
        let b = argbits >> 8;
        (a as usize, b as usize)
    }   
}
