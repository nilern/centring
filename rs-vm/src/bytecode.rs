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

pub fn cnst(i: u16) -> Bytecode {
    Bytecode((i as u32) << ARG0_SHIFT | Opcode::Const as u32)
}

pub fn local(i: u16) -> Bytecode {
    Bytecode((i as u32) << ARG0_SHIFT | Opcode::Local as u32)
}

pub fn clover(i: u16) -> Bytecode {
    Bytecode((i as u32) << ARG0_SHIFT | Opcode::Clover as u32)
}

pub fn addi(i: u8, j: u8) -> Bytecode {
    Bytecode((j as u32) << ARG1_SHIFT | (i as u32) << ARG0_SHIFT
             | Opcode::AddI as u32)
}

pub fn subi(i: u8, j: u8) -> Bytecode {
    Bytecode((j as u32) << ARG1_SHIFT | (i as u32) << ARG0_SHIFT
             | Opcode::SubI as u32)
}

pub fn muli(i: u8, j: u8) -> Bytecode {
    Bytecode((j as u32) << ARG1_SHIFT | (i as u32) << ARG0_SHIFT
             | Opcode::MulI as u32)
}

pub fn divi(i: u8, j: u8) -> Bytecode {
    Bytecode((j as u32) << ARG1_SHIFT | (i as u32) << ARG0_SHIFT
             | Opcode::DivI as u32)
}

pub fn fun(i: u16) -> Bytecode {
    Bytecode((i as u32) << ARG0_SHIFT | Opcode::Fn as u32)
}

pub fn call(i: u16) -> Bytecode {
    Bytecode((i as u32) << ARG0_SHIFT | Opcode::Call as u32)
}

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
