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

// Examine tags of block objects:
// (these only work on 64-bit architectures)
const BH_TYPE_MASK: usize = 0x0f00000000000000;
const BH_LENGTH_MASK: usize = 0x00ffffffffffffff;
const BH_SHIFT: usize = 56;

// Type tags of block objects:
const ARRAY_BITS: usize = 0;
const SYMBOL_BITS: usize = 0x0100000000000000;
const STRING_BITS: usize = 0x0200000000000000;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Word(usize);

//// Garbage-collected Heap

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

impl IndexMut<usize> for GcHeap {
    fn index_mut<'a>(&'a mut self, index: usize) -> &'a mut Word {
        &mut self.fromspace[index]
    }
}

impl Index<Range<usize>> for GcHeap {
    type Output = [Word];
    fn index<'a>(&'a self, idxs: Range<usize>) -> &'a [Word] {
        & self.fromspace[idxs]
    }
}

impl IndexMut<Range<usize>> for GcHeap {
    fn index_mut<'a>(&'a mut self, idxs: Range<usize>) -> &'a mut [Word] {
        &mut self.fromspace[idxs]
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

    fn alloc(&mut self, v: BlockValue) -> Word {
        match v {
            BlockValue::Array(ws) => {
                let res = From::from(self.fromspace.len());
                let header = Word(ws.len() & BH_LENGTH_MASK | ARRAY_BITS);
                self.fromspace.push(header);
                for w in ws {
                    self.fromspace.push(w)
                }
                res
            },
            BlockValue::Symbol(ns, name) => {
                let header = Word(2 & BH_LENGTH_MASK | SYMBOL_BITS);
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
                let header = Word(s.len() & BH_LENGTH_MASK | STRING_BITS);
                self.fromspace.push(header);
                for c in s.chars() {
                    self.fromspace.push(From::from(c))
                }
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
    Char(char)
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

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
enum BlockValue {
    Array(Vec<Word>),
    Symbol(String, String),
    String(String)
}

#[derive(Debug)]
enum BlockRef<'a> {
    Array {
        data: &'a [Word]
    },
    Symbol(&'a Word, &'a Word),
    String(String)

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

use BlockRef::{Array, Symbol};

impl Word {
    fn block_val<'a>(& self, heap: &'a GcHeap) -> Option<BlockRef<'a>> {
        let Word(w) = *self;
        if w & LOW_TAG_MASK == BLOCK_BITS {
            let hdri: usize = From::from(self);
            let Word(header) = heap[hdri];
            let len = header & BH_LENGTH_MASK;
            match header & BH_TYPE_MASK {
                ARRAY_BITS => {
                    let bdi = hdri + 1;
                    Some(Array { data: &heap[bdi..(bdi + len)] })
                },
                SYMBOL_BITS => {
                    Some(Symbol(&heap[hdri + 1], &heap[hdri + 2]))
                },
                STRING_BITS => {
                    let bdi = hdri + 1;
                    let mut s = String::new();
                    for w in heap[bdi..(bdi + len)].iter() {
                        s.push(From::from(w.clone()));
                    }
                    Some(BlockRef::String(s))
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
    fn write(&self, heap: & GcHeap, f: &mut io::Write) -> io::Result<()>;
}

impl Write for ImmediateValue {
    fn write(&self, heap: &GcHeap, f: &mut io::Write) -> io::Result<()> {
        match *self {
            Int(i)  => write!(f, "{}", i),
            Bool(b) => if b { write!(f, "#t") } else { write!(f, "#f") },
            Char(c) => write!(f, "\\{}", c),
        }
    }
}

impl<'a> Write for BlockRef<'a> {
    fn write(&self, heap: &GcHeap, f: &mut io::Write) -> io::Result<()> {
        match *self {
            Array { data: ref ws } => {
                try!(write!(f, "#.(core/Array"));
                for w in ws.iter() {
                    try!(write!(f, " "));
                    if let Some(iv) = w.imm_val() {
                        try!(iv.write(heap, f));
                    } else {
                        // unwrap() can't panic since .*_val would have already:
                        try!(w.block_val(heap).unwrap().write(heap, f))
                    }
                }
                write!(f, ")")
            },
            Symbol(nsw, nmw) => {
                if let (Some(BlockRef::String(ns)), Some(BlockRef::String(name)))
                       = (nsw.block_val(heap), nmw.block_val(heap)) {
                    write!(f, "{}/{}", ns, name)
                } else {
                    panic!()
                }
            },
            BlockRef::String(ref s) => {
                write!(f, "\"{}\"", s)
            }
        }
    }
}

fn writeln<T>(v: &T, heap: &GcHeap, f: &mut io::Write) -> io::Result<()>
    where T: Write {
    try!(v.write(heap, f));
    io::Write::write(f, b"\n").map(|_| ())
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

    // Immediate values

    fn parse_isize(&mut self) -> Result<isize, ()> {
        self.consume_while(|c| c.is_digit(10)).parse::<isize>().or(Err(()))
    }


    fn parse_char(&mut self) -> Result<char, ()> {
        if self.consume_if(|c| c == '\\').is_some() {
            self.consume_if(|c| !c.is_whitespace()).ok_or(())
        } else {
            Err(())
        }
    }

    fn parse_bool(&mut self) -> Result<bool, ()> {
        if self.consume_if(|c| c == '#').is_some() {
            match self.peek() {
                Some('t') => { self.pos += 1; Ok(true)},
                Some('f') => { self.pos += 1; Ok(false)},
                _ => { self.pos -= 1; Err(()) }
            }
        } else {
            Err(())
        }
    }

    fn parse_symbol(&mut self, heap: &mut GcHeap) -> Result<Word, ()> {
        let s = self.consume_while(|c| c.is_alphabetic());
        if !s.is_empty() {
            Ok(heap.intern(BlockValue::Symbol("user".to_string(), s)))
        } else {
            Err(())
        }
    }

    fn parse_string(&mut self, heap: &mut GcHeap) -> Result<Word, ()> {
        if self.consume_if(|c| c == '"').is_some() {
            let s = self.consume_while(|c| c != '"');
            if self.consume_if(|c| c == '"').is_some() {
                return Ok(heap.alloc(BlockValue::String(s)));
            } else {
                Err(())
            }
        } else {
            Err(())
        }
    }
}

fn read(cs: &str, heap: &mut GcHeap) -> Result<Word, ()> {
    let mut p = Parser { pos: 0, input: cs.to_string() };
    match p.peek() {
        Some(c) if c.is_digit(10) => p.parse_isize().map(From::from),
        Some('\\') => p.parse_char().map(From::from),
        Some('#') => p.parse_bool().map(From::from),
        Some(c) if c.is_alphabetic() => p.parse_symbol(heap),
        Some('"') => p.parse_string(heap),
        _ => Err(())
    }
}

//// Eval

fn eval(expr: Word, heap: & GcHeap) -> Word {
    if expr.imm_val().is_some() || expr.block_val(heap).is_some() {
        expr
    } else {
        unreachable!() // No such bit patterns
    }
}

//// Main

fn main() {
    let mut gc_heap = GcHeap::new();
    let mut out = &mut stdout();

    let w: Word = From::from(-3isize);
    writeln(&eval(w, &gc_heap).imm_val().unwrap(), &gc_heap, out);

    let arr0 = BlockValue::Array(vec![From::from(2isize)]);
    let aw0 = gc_heap.alloc(arr0);
    let arr1 = BlockValue::Array(vec![From::from(true), From::from(1isize), aw0]);
    let aw = gc_heap.alloc(arr1);
    writeln(&aw.block_val(&gc_heap).unwrap(), &gc_heap, out);

    let sym = BlockValue::Symbol("foo".to_string(), "bar".to_string());
    let sw = gc_heap.alloc(sym);
    writeln(&sw.block_val(&gc_heap).unwrap(), &gc_heap, out);

    //

    let input = String::from("#f");
    writeln(&read(&input, &mut gc_heap).unwrap().imm_val().unwrap(),
            &gc_heap, out);

    let symput = String::from("foo");
    writeln(&read(&symput, &mut gc_heap).unwrap().block_val(&gc_heap).unwrap(),
            &gc_heap, out);
}
