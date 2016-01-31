use std::ops::Index;
use std::mem::transmute;
use std::fmt;

// Examine tags of Words:
const INT_MASK: usize = 0b1;
const LOW_TAG_MASK: usize = 0b11;
const HIGH_TAG_MASK: usize = 0b1100;
const TAG_MASK: usize = 0b1111;

// Int, other immediate or block?:
const INT_BIT: usize = 0b1;
const IMMEDIATE_BITS: usize = 0b10;
const BLOCK_BITS: usize = 0b00;

// Tags of other immediates:
const BOOL_BITS: usize = 0b0110;
const CHAR_BITS: usize = 0b1010;
const SPECIAL_BITS: usize = 0b1110;

// Shifts for conversions:
const INT_SHIFT: usize = 1;
const BOOL_SHIFT: usize = 4;
const CHAR_SHIFT: usize = 8;

// Bool representation:
const TRUE: usize = 0b10110;
const FALSE: usize = 0b00110;

// Examine tags of block objects:
const BH_TYPE_BITS: usize = 0xf00000000000000;

#[derive(Debug)]
struct Word(usize);

//// Garbage-collected Heap

struct GcHeap {
    fromspace: Vec<Word>,
    tospace: Vec<Word>
}

impl Index<usize> for GcHeap {
    type Output = Word;
    fn index<'a>(&'a self, index: usize) -> &'a Word {
        & self.fromspace[index >> 2]
    }
}

//// Immediate Values

#[derive(Debug)]
enum ImmediateValue {
    Int(isize),
    Bool(bool),
    Char(char)
}

use ImmediateValue::*;

impl From<isize> for Word {
    fn from(i: isize) -> Word {
        unsafe { Word(transmute::<isize, usize>(i << INT_SHIFT) | INT_BIT) }
    }
}

impl From<bool> for Word {
    fn from(b: bool) -> Word {
        Word(if b { TRUE } else { FALSE })
    }
}

impl From<char> for Word {
    fn from(c: char) -> Word {
        Word((c as usize) << CHAR_SHIFT | CHAR_BITS)
    }
}

impl Word {
    fn imm_val(&self) -> Option<ImmediateValue> {
        let Word(w) = *self;
        if w & INT_MASK == INT_BIT {
            unsafe { Some(Int(transmute::<usize, isize>(w) >> INT_SHIFT)) }
        } else {
            match w & LOW_TAG_MASK {
                IMMEDIATE_BITS => match w & TAG_MASK {
                    BOOL_BITS => match w {
                        FALSE => Some(Bool(false)),
                        TRUE  => Some(Bool(true)),
                        _     => panic!("No such Bool value `{}`!", w >> BOOL_SHIFT)
                    },
                    CHAR_BITS => unsafe {
                        // This only works on 64-bit architectures:
                        Some(Char(transmute::<usize, [char; 2]>(w >> CHAR_SHIFT)[0]))
                    },
                    bp => panic!("Unimplemented immediate tag `{:b}`!", bp)
                },
                BLOCK_BITS => None, // Non-immediate value
                _ => unreachable!() // 0b01 and 0b11 are Ints
            }
        }
    }
}

//// Block Values

enum BlockValue<'a> {
    Array(&'a mut [Word])
}

use BlockValue::*;

//// Print

impl fmt::Display for ImmediateValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Int(i)  => write!(f, "{}", i),
            Bool(b) => if b { write!(f, "True") } else { write!(f, "False") },
            Char(c) => write!(f, "{}", c),
        }
    }
}

//// Eval

fn eval(expr: Word) -> Word {
    if let Some(_) = expr.imm_val() {
        expr
    } else {
        panic!()
    }
}

//// Main

fn main() {
//     let gc_heap = GcHeap { fromspace: vec![], tospace: vec![] };

    let w: Word = From::from(true);
    println!("{}", eval(w).imm_val().unwrap());
}
