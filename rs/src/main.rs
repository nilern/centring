use std::ops::{Index, IndexMut, Range};
use std::mem::transmute;
use std::io;
use std::io::stdout;
use std::collections::HashMap;

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
const BLOCK_SHIFT: usize = 2;

// Bool representation:
const TRUE: usize = 0b10110;
const FALSE: usize = 0b00110;

// Special objects:
const EMPTY_LIST: usize = 0b1110;

// Examine tags of block objects:
// (these only work on 64-bit architectures)
const BH_TYPE_MASK: usize = 0x0f00000000000000;
const BH_LENGTH_MASK: usize = 0x00ffffffffffffff;
const BH_SHIFT: usize = 56;

// Type tags of block objects:
const ARRAY_BITS: usize = 0;
const SYMBOL_BITS: usize = 0x0100000000000000;
const STRING_BITS: usize = 0x0200000000000000;
const PAIR_BITS: usize = 0x0300000000000000;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Word(usize);

//// Garbage-collected Heap

trait Allocate {
    fn alloc(&mut self, v: BlockValue) -> Word;
    fn intern(&mut self, v: BlockValue) -> Word;
}

#[derive(Debug)]
struct GcHeap {
    fromspace: Vec<Word>,
    tospace: Vec<Word>,
    symtab: HashMap<BlockValue, Word>
}

impl Index<usize> for GcHeap {
    type Output = Word;
    fn index<'a>(&'a self, index: usize) -> &'a Word {
        & self.fromspace[index]
    }
}

impl Index<Range<usize>> for GcHeap {
    type Output = [Word];
    fn index<'a>(&'a self, idxs: Range<usize>) -> &'a [Word] {
        & self.fromspace[idxs]
    }
}

impl GcHeap {
    fn new() -> GcHeap {
        GcHeap {
            fromspace: vec![],
            tospace: vec![],
            symtab: HashMap::new()
        }
    }
}


impl Allocate for GcHeap {
    fn alloc(&mut self, v: BlockValue) -> Word {
        match v {
            BlockValue::Array(ws) => {
                let res = From::from(self.fromspace.len());
                let header = Word(ws.len() | ARRAY_BITS);
                self.fromspace.push(header);
                for w in ws {
                    self.fromspace.push(w)
                }
                res
            },
            BlockValue::Symbol(ns, name) => {
                let header = Word(2 | SYMBOL_BITS);
                let nsw = self.alloc(BlockValue::String(ns));
                let nmw = self.alloc(BlockValue::String(name));
                let res = From::from(self.fromspace.len());
                self.fromspace.push(header);
                self.fromspace.push(nsw);
                self.fromspace.push(nmw);
                res
            },
            BlockValue::String(s) => {
                let res = From::from(self.fromspace.len());
                let header = Word(s.len() | STRING_BITS);
                self.fromspace.push(header);
                for c in s.chars() {
                    self.fromspace.push(From::from(c))
                }
                res
            },
            BlockValue::Pair(car, cdr) => {
                let res = From::from(self.fromspace.len());
                let header = Word(2 | PAIR_BITS);
                self.fromspace.push(header);
                self.fromspace.push(car);
                self.fromspace.push(cdr);
                res
            }
        }
    }

    fn intern(&mut self, v: BlockValue) -> Word {
        if let Some(w) = self.symtab.get(&v) {
            return w.clone()
        }
        let w = self.alloc(v.clone());
        self.symtab.insert(v, w.clone());
        w
    }
}

//// Immediate Values

#[derive(Debug)]
enum ImmediateValue {
    Int(isize),
    Bool(bool),
    Char(char),
    EmptyList
}

use ImmediateValue::*;

impl From<isize> for Word {
    fn from(i: isize) -> Word {
        Word((i << INT_SHIFT) as usize | INT_BIT)
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

impl From<Word> for char {
    fn from(word: Word) -> char {
        let Word(w) = word;
        unsafe {
            // This only works on 64-bit architectures:
            transmute::<usize, [char; 2]>(w >> CHAR_SHIFT)[0]
        }
    }
}

impl Word {
    fn imm_val(&self) -> Option<ImmediateValue> {
        let Word(w) = *self;
        if w & INT_MASK == INT_BIT {
            Some(Int((w as isize) >> INT_SHIFT))
        } else {
            match w & LOW_TAG_MASK {
                IMMEDIATE_BITS => match w & TAG_MASK {
                    BOOL_BITS => match w {
                        FALSE => Some(Bool(false)),
                        TRUE  => Some(Bool(true)),
                        _     => panic!("No such Bool value `{}`!", w >> BOOL_SHIFT)
                    },
                    CHAR_BITS => Some(Char(From::from(self.clone()))),
                    SPECIAL_BITS => match w {
                        EMPTY_LIST => Some(EmptyList),
                        bp => panic!("Unimplemented special value `{:b}`!", bp)
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

impl From<usize> for Word {
    fn from(u: usize) -> Word {
        Word(u << BLOCK_SHIFT)
    }
}

impl<'a> From<&'a Word> for usize {
    fn from(word: &'a Word) -> usize {
        let Word(w) = *word;
        w >> BLOCK_SHIFT
    }
}

// Hmm, those Words might get invalidated...
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
enum BlockValue {
    Array(Vec<Word>),
    Symbol(String, String),
    String(String),
    Pair(Word, Word)

// These should be stripped of refs
//     BuiltinType {
//         name: &'a Word
//     },
//     RecordType {
//         name: &'a Word,
//         field_names: &'a [Word]
//     },
//     EnumType {
//         name: &'a Word,
//         shards: &'a [Word]
//     },
// 
//     EnumShard {
//         parent_type: &'a Word,
//         name: &'a Word,
//         field_names: &'a [Word]
//     },
// 
//     Record {
//         typ: &'a Word,
//         field_values: &'a [Word]
//     },
//     Tagged {
//         shard: &'a Word,
//         field_values: &'a [Word]
//     }
}

use BlockValue::{Array, Symbol};

impl Word {
    fn block_val(& self, vm: &VM) -> Option<BlockValue> {
        let Word(w) = *self;
        if w & LOW_TAG_MASK == BLOCK_BITS {
            let hdri: usize = From::from(self);
            let Word(header) = vm.memory[hdri].clone();
            let len = header & BH_LENGTH_MASK;
            match header & BH_TYPE_MASK {
                ARRAY_BITS => {
                    let bdi = hdri + 1;
                    Some(Array(vm.memory[bdi..(bdi + len)].iter()
                                                          .map(Clone::clone)
                                                          .collect()))
                },
                SYMBOL_BITS => {
                    if let (Some(BlockValue::String(ns)), Some(BlockValue::String(name)))
                           = (vm.memory[hdri + 1].block_val(vm),
                              vm.memory[hdri + 2].block_val(vm)) {
                        Some(Symbol(ns, name))
                    } else {
                        panic!()
                    }
                },
                STRING_BITS => {
                    let bdi = hdri + 1;
                    let mut s = String::new();
                    for w in vm.memory[bdi..(bdi + len)].iter() {
                        s.push(From::from(w.clone()));
                    }
                    Some(BlockValue::String(s))
                },
                PAIR_BITS => {
                    Some(BlockValue::Pair(vm.memory[hdri + 1].clone(),
                                          vm.memory[hdri + 2].clone()))
                },
                bp => panic!("Unimplemented block tag `{:b}`!", bp >> BH_SHIFT)
            }
        } else {
            None
        }
    }
}

//// Print

trait Write {
    fn write(&self, vm: &mut VM) -> io::Result<()>;
}

impl Write for ImmediateValue {
    fn write(&self, vm: &mut VM) -> io::Result<()> {
        match *self {
            Int(i)  => write!(vm.curr_output, "{}", i),
            Bool(true) => write!(vm.curr_output, "True"),
            Bool(false) => write!(vm.curr_output, "True"),
            Char(c) => write!(vm.curr_output, "\\{}", c),
            EmptyList => write!(vm.curr_output, "()")
        }
    }
}

impl Write for BlockValue {
    fn write(&self, vm: &mut VM) -> io::Result<()> {
        match *self {
            Array(ref ws) => {
                try!(write!(vm.curr_output, "#.(core/Array"));
                for w in ws.iter() {
                    try!(write!(vm.curr_output, " "));
                    try!(w.write(vm));
                }
                write!(vm.curr_output, ")")
            },
            Symbol(ref ns, ref name) => {
                write!(vm.curr_output, "{}/{}", ns, name)
            },
            BlockValue::String(ref s) => {
                write!(vm.curr_output, "\"{}\"", s)
            },
            BlockValue::Pair(ref car, ref cdr) => {
                let mut first = car.clone();
                let mut rest = cdr.clone();
                try!(write!(vm.curr_output, "("));
                try!(first.write(vm));
                loop {
                    if let Some(iv) = rest.imm_val() {
                        if rest.0 != EMPTY_LIST {
                            try!(write!(vm.curr_output, " . "));
                            try!(iv.write(vm));
                        }
                        break;
                    } else {
                        let bvr = rest.block_val(vm);
                        match bvr {
                            Some(BlockValue::Pair(cadr, cddr)) => {
                                first = cadr;
                                rest = cddr;
                                try!(write!(vm.curr_output, " "));
                                try!(first.write(vm));
                            },
                            Some(bv) => {
                                try!(write!(vm.curr_output, " . "));
                                try!(bv.write(vm));
                                break;
                            },
                            None => unreachable!() // No such bit patterns
                        }
                    }
                }
                write!(vm.curr_output, ")")
            }
        }
    }
}

impl Write for Word {
    fn write(&self, vm: &mut VM) -> io::Result<()> {
        if let Some(iv) = self.imm_val() {
            iv.write(vm)
        } else {
            // .unwrap() can't panic since .*_val() would have already:
            self.block_val(vm).unwrap().write(vm)
        }
    }
}

fn writeln<T>(v: &T, vm: &mut VM) -> io::Result<()>
    where T: Write {
    try!(v.write(vm));
    io::Write::write(&mut vm.curr_output, b"\n").map(|_| ())
}

//// Read

struct Parser {
    pos: usize,
    input: String
}

impl Parser {
    fn peek(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn char(&mut self) -> Option<char> {
        let mut iter = self.input[self.pos..].char_indices();
        let res = iter.next().map(|(_, c)| c);
        self.pos += iter.next().unwrap_or((1, ' ')).0;
        res
    }

    fn eof(&self) -> bool {
        self.pos >= self.input.len()
    }

    // Conditions

    fn starts_with(&self, s: &str) -> bool {
        self.input[self.pos ..].starts_with(s)
    }

    fn consume_if<F>(&mut self, pred: F) -> Option<char>
        where F: Fn(char) -> bool {
        self.peek().and_then(|c| if pred(c) { self.pos += 1; Some(c) } else { None })
    }

    fn consume_while<F>(&mut self, pred: F) -> String
        where F: Fn(char) -> bool {
        let mut res = String::new();
        loop {
            match self.peek() {
                Some(c) if pred(c) => res.push(self.char().unwrap()),
                _ => return res
            }
        }
    }

    fn whitespace(&mut self) -> String {
        self.consume_while(|c| c.is_whitespace())
    }

    // Immediate values

    fn parse_isize(&mut self) -> Result<isize, ()> {
        self.consume_while(|c| c.is_digit(10)).parse::<isize>().or(Err(()))
    }


    fn parse_char(&mut self) -> Result<char, ()> {
        try!(self.consume_if(|c| c == '#').ok_or(()));
        self.consume_if(|c| !c.is_whitespace()).ok_or(())
    }

    fn parse_bool(&mut self) -> Result<bool, ()> {
        try!(self.consume_if(|c| c == '#').ok_or(()));
        match self.peek() {
            Some('t') => { self.pos += 1; Ok(true)},
            Some('f') => { self.pos += 1; Ok(false)},
            _ => { self.pos -= 1; Err(()) }
        }
    }

    fn parse_list(&mut self, vm: &mut VM) -> Result<Word, ()> {
        try!(self.consume_if(|c| c == '(').ok_or(()));
        let mut vals = vec![];
        let mut tail = Word(EMPTY_LIST);
        loop {
            self.whitespace();
            match self.peek() {
                Some(')') => {self.char(); break; },
                Some('.') => {
                    // FIXME
                    tail = try!(self.parse_expr(vm));
                    self.whitespace();
                    try!(self.consume_if(|c| c == ')').ok_or(()));
                    break;
                },
                Some(c) => match self.parse_expr(vm) {
                    Ok(w) => vals.push(w),
                    err => return err
                },
                None => return Err(())
            }
            self.whitespace();
        }
        for head in vals.into_iter().rev() {
            tail = vm.alloc(BlockValue::Pair(head, tail));
        }
        Ok(tail)
    }

    fn parse_symbol(&mut self, vm: &mut VM) -> Result<Word, ()> {
        let s = self.consume_while(|c| c.is_alphabetic());
        if !s.is_empty() {
            Ok(vm.intern(BlockValue::Symbol("user".to_string(), s)))
        } else {
            Err(())
        }
    }

    fn parse_string(&mut self, vm: &mut VM) -> Result<Word, ()> {
        try!(self.consume_if(|c| c == '"').ok_or(()));
        let s = self.consume_while(|c| c != '"');
        try!(self.consume_if(|c| c == '"').ok_or(()));
        Ok(vm.alloc(BlockValue::String(s)))
    }

    fn parse_expr(&mut self, vm: &mut VM) -> Result<Word, ()> {
        match self.peek() {
            Some(c) if c.is_digit(10) => self.parse_isize().map(From::from),
            Some('\\') => self.parse_char().map(From::from),
            Some('#') => self.parse_bool().map(From::from),
            Some('(') => self.parse_list(vm),
            Some(c) if c.is_alphabetic() => self.parse_symbol(vm),
            Some('"') => self.parse_string(vm),
            _ => Err(())
        }
    }
}

fn read(vm: &mut VM, n: usize) {
    let s = vm.pop().unwrap();
    if let Some(BlockValue::String(s)) = s.block_val(vm) {
        let mut p = Parser { pos: 0, input: s };
        let res = p.parse_expr(vm);
        vm.push(res.unwrap());
    } else {
        let msg = vm.alloc(BlockValue::String("Not a string!".to_string()));
        vm.throw(msg);
    }
}

//// VM

struct VM {
    codeobject: Word,
    env: Word,
    ip: usize,
    framestack: Option<Word>,
    valuestack: Vec<Word>,
    memory: GcHeap,
    curr_output: Box<io::Write>
}

impl VM {
    fn new(codeobject: Word, env: Word) -> VM {
        VM {
            codeobject: codeobject,
            env: env,
            ip: 0,
            framestack: None,
            valuestack: Vec::with_capacity(20),
            memory: GcHeap::new(),
            curr_output: Box::new(stdout())
        }
    }

    fn push(&mut self, w: Word) {
        self.valuestack.push(w)
    }

    fn pop(&mut self) -> Option<Word> {
        self.valuestack.pop()
    }

    fn throw(&mut self, word: Word) -> io::Result<()> {
        self.valuestack.push(word);
        self.framestack = None;
        writeln(&self.pop().unwrap(), self)
    }
}

impl Allocate for VM {
    fn alloc(&mut self, v: BlockValue) -> Word {
        self.memory.alloc(v)
    }

    fn intern(&mut self, v: BlockValue) -> Word {
        self.memory.intern(v)
    }
}

//// Main

fn main() {
    let mut vm = VM::new(Word(FALSE), Word(FALSE));

    let input = vm.alloc(BlockValue::String("(foo 23 (bar \"fnord\"))".to_string()));
    vm.push(input);
    read(&mut vm, 1);
    let sexpr = vm.pop().unwrap();
    writeln(&sexpr, &mut vm);
}
