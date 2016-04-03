use std::mem::transmute;

// Constants

const OP_BITS: u32 = 0x0000_00FF;
const ARG_BITS: u32 = 0b11_1111_1111;
const ARG0_SHIFT: u32 = 8;
const ARG1_SHIFT: u32 = 18;

const DOMAIN_BITS: u32 = 0xF00;
const INDEX_BITS: u32 = 0xFF;

const LOCAL_TAG: u32 = 0x000;
const CLOVER_TAG: u32 = 0x100;
const CONST_TAG: u32 = 0x200;
// const GLOBAL_TAG: u32 = 0x300;

// Types

#[derive(Debug, Clone, Copy)]
pub struct Bytecode(u32);

#[repr(u32)]
pub enum Opcode {
    Const,
    Local,
    Clover,
    // Global,

    Add,
    Sub,
    Mul,
    Div,

    // Brf,

    Fn,
    Call
}

#[derive(Debug, Clone, Copy)]
pub struct Arg(u32);

#[repr(u32)]
pub enum Domain {
    Const = CONST_TAG,
    Local = LOCAL_TAG,
    Clover = CLOVER_TAG,
    // Global = GLOBAL_TAG
}

// Behaviour

macro_rules! bytecode_ctor {
    ($name:ident, $opcode:path, $a:ident) => {
        pub fn $name(i: u16) -> Bytecode {
            Bytecode((i as u32) << ARG0_SHIFT
                     | $opcode as u32)
        }
    };

    ($name:ident, $opcode:path, $a:ident, $b:ident) => {
        pub fn $name(i: u32, j: u32) -> Bytecode {
            Bytecode(j << ARG1_SHIFT
                     | i << ARG0_SHIFT
                     | $opcode as u32)
        }
    };
}

bytecode_ctor!(cnst, Opcode::Const, i);
bytecode_ctor!(local, Opcode::Local, i);
bytecode_ctor!(clover, Opcode::Clover, i);

bytecode_ctor!(addi, Opcode::Add, a, b);
bytecode_ctor!(subi, Opcode::Sub, a, b);
bytecode_ctor!(muli, Opcode::Mul, a, b);
bytecode_ctor!(divi, Opcode::Div, a, b);

bytecode_ctor!(fun, Opcode::Fn, i);
bytecode_ctor!(call, Opcode::Call, n);

pub fn l(i: u8) -> u32 { LOCAL_TAG | i as u32 }
pub fn f(i: u8) -> u32 { CLOVER_TAG | i as u32 }
pub fn c(i: u8) -> u32 { CONST_TAG | i as u32 }
//pub fn g(i: u8) -> u32 { GLOBAL_TAG | i as u32 }

impl Bytecode {
    pub fn op(self) -> Opcode {
        unsafe { transmute(self.0 & OP_BITS) }
    }

    pub fn arg(self) -> usize {
        (self.0 >> ARG0_SHIFT) as usize
    }

    pub fn arg0(self) -> Arg {
        Arg(self.0 >> ARG0_SHIFT & ARG_BITS)
    }

    pub fn arg1(self) -> Arg {
        Arg(self.0 >> ARG1_SHIFT & ARG_BITS)
    }
}

impl Arg {
    pub fn domain(self) -> Domain {
        unsafe { transmute(self.0 & DOMAIN_BITS) }
    }

    pub fn index(self) -> usize {
        (self.0 & INDEX_BITS) as usize
    }
}
