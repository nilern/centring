
pub const OP_BITS: u32 = 0x0000_00FF;

pub const ARG0_SHIFT: u32 = 8;
pub const ARG1_SHIFT: u32 = 16;

pub const CONST: u32 = 0x00;
pub const LOCAL: u32 = 0x01;
pub const ADDI: u32 = 0x02;
pub const SUBI: u32 = 0x03;
pub const MULI: u32 = 0x04;
pub const DIVI: u32 = 0x05;

// Types

#[derive(Debug, Clone, Copy)]
pub struct Bytecode(u32);

// Behaviour

impl Bytecode {
    pub fn cnst(i: u16) -> Bytecode {
        Bytecode((i as u32) << ARG0_SHIFT | CONST)
    }

    pub fn local(i: u16) -> Bytecode {
        Bytecode((i as u32) << ARG0_SHIFT | LOCAL)
    }

    pub fn addi(i: u8, j: u8) -> Bytecode {
        Bytecode((j as u32) << ARG1_SHIFT | (i as u32) << ARG0_SHIFT | ADDI)
    }

    pub fn subi(i: u8, j: u8) -> Bytecode {
        Bytecode((j as u32) << ARG1_SHIFT | (i as u32) << ARG0_SHIFT | SUBI)
    }

    pub fn muli(i: u8, j: u8) -> Bytecode {
        Bytecode((j as u32) << ARG1_SHIFT | (i as u32) << ARG0_SHIFT | MULI)
    }

    pub fn divi(i: u8, j: u8) -> Bytecode {
        Bytecode((j as u32) << ARG1_SHIFT | (i as u32) << ARG0_SHIFT | DIVI)
    }

    pub fn op(&self) -> u32 {
        self.0 & OP_BITS
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
