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

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
enum BlockValue {
    Array(Vec<Word>),
    Symbol(String, String),
    String(String),
    Pair(Word, Word)
}

#[derive(Debug)]
enum BlockRef<'a> {
    Array {
        data: &'a [Word]
    },
    Symbol(&'a Word, &'a Word),
    String(String),
    Pair(&'a Word, &'a Word)

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
                PAIR_BITS => {
                    Some(BlockRef::Pair(&heap[hdri + 1], &heap[hdri + 2]))
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
            EmptyList => write!(f, "()")
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
                    try!(w.write(heap, f));
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
            },
            BlockRef::Pair(car, cdr) => {
                let mut first = car;
                let mut rest = cdr;
                try!(write!(f, "("));
                try!(first.write(heap, f));
                loop {
                    if let Some(iv) = rest.imm_val() {
                        if rest.0 != EMPTY_LIST {
                            try!(write!(f, " . "));
                            try!(iv.write(heap, f));
                        }
                        break;
                    } else {
                        match rest.block_val(heap) {
                            Some(BlockRef::Pair(cadr, cddr)) => {
                                first = cadr;
                                rest = cddr;
                                try!(write!(f, " "));
                                try!(first.write(heap, f));
                            },
                            Some(bv) => {
                                try!(write!(f, " . "));
                                try!(bv.write(heap, f));
                                break;
                            },
                            None => unreachable!() // No such bit patterns
                        }
                    }
                }
                write!(f, ")")
            }
        }
    }
}

impl Write for Word {
    fn write(&self, heap: &GcHeap, f: &mut io::Write) -> io::Result<()> {
        if let Some(iv) = self.imm_val() {
            iv.write(heap, f)
        } else {
            // .unwrap() can't panic since .*_val() would have already:
            self.block_val(heap).unwrap().write(heap, f)
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

    fn parse_list(&mut self, heap: &mut GcHeap) -> Result<Word, ()> {
        try!(self.consume_if(|c| c == '(').ok_or(()));
        let mut vals = vec![];
        let mut tail = Word(EMPTY_LIST);
        loop {
            self.whitespace();
            match self.peek() {
                Some(')') => {self.char(); break; },
                Some('.') => {
                    // FIXME
                    tail = try!(self.parse_expr(heap));
                    self.whitespace();
                    try!(self.consume_if(|c| c == ')').ok_or(()));
                    break;
                },
                Some(c) => match self.parse_expr(heap) {
                    Ok(w) => vals.push(w),
                    err => return err
                },
                None => return Err(())
            }
            self.whitespace();
        }
        for head in vals.into_iter().rev() {
            tail = heap.alloc(BlockValue::Pair(head, tail));
        }
        Ok(tail)
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
        try!(self.consume_if(|c| c == '"').ok_or(()));
        let s = self.consume_while(|c| c != '"');
        try!(self.consume_if(|c| c == '"').ok_or(()));
        Ok(heap.alloc(BlockValue::String(s)))
    }

    fn parse_expr(&mut self, heap: &mut GcHeap) -> Result<Word, ()> {
        match self.peek() {
            Some(c) if c.is_digit(10) => self.parse_isize().map(From::from),
            Some('\\') => self.parse_char().map(From::from),
            Some('#') => self.parse_bool().map(From::from),
            Some('(') => self.parse_list(heap),
            Some(c) if c.is_alphabetic() => self.parse_symbol(heap),
            Some('"') => self.parse_string(heap),
            _ => Err(())
        }
    }
}

fn read(cs: &str, heap: &mut GcHeap) -> Result<Word, ()> {
    let mut p = Parser { pos: 0, input: cs.to_string() };
    p.parse_expr(heap)
}

//// Main

fn main() {
    let mut gc_heap = GcHeap::new();
    let mut out = &mut stdout();

    let input = String::from("(foo 23 (bar \"fnord\"))");
    writeln(&read(&input, &mut gc_heap).unwrap(), &gc_heap, out);
}
