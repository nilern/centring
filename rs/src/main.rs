use std::rc::Rc;
use std::collections::HashMap;
use std::fmt;
use std::str::FromStr;

// Types

type ValueRef = Rc<Value>;

#[derive(Debug)]
enum Value {
    Int(isize),
    Bool(bool),
    Char(char),
    Symbol(Option<String>, String),

    Tuple(Vec<ValueRef>),
    Array(Vec<ValueRef>),
    Pair { first: ValueRef, rest: ValueRef },
    EmptyList,

    Record { typ: ValueRef, vals: Vec<ValueRef> },
    Singleton { typ: ValueRef },

    AbstractType { name: String, supertyp: ValueRef },
    RecordType { name: String, supertyp: ValueRef, field_names: Vec<String> },
    SingletonType { name: String, supertyp: ValueRef },
    BuiltInType { name: String, supertyp: ValueRef },

    Fn {
        name: String,
        formal_names: Vec<String>,
        formal_types: Vec<ValueRef>,
        body: ValueRef,
        env: Rc<Environment>
    },
    NativeFn {
        name: String,
        formal_types: Vec<ValueRef>,
        code: fn(Interpreter, Vec<ValueRef>) -> ValueRef
    }
}

#[derive(Debug)]
struct Environment {
    bindings: HashMap<String, ValueRef>,
    parent: Option<Rc<Environment>>
}

struct Interpreter;

// Value Utils

fn prepend(v: ValueRef, coll: ValueRef) -> Value {
    match *coll {
        Value::EmptyList | Value::Pair { .. } => Value::Pair {
            first: v,
            rest: coll
        },
        _ => panic!()
    }
}

fn vec_to_list(vs: Vec<ValueRef>) -> Value {
    let mut res = Value::EmptyList;
    for v in vs.into_iter().rev() {
        res = Value::Pair { first: v, rest: Rc::new(res) };
    }
    res
}

// Read

#[derive(Debug)]
enum ParseError {
    EOF,
    Illegal(char),
    ExpectedPred(String),
    ExpectedConstituent(char),
    UnknownPounded,
    InvalidCharDesc
}

type ParseResult<T> = Result<(T, Parser), (ParseError, Parser)>;

#[derive(Debug, Clone)]
struct Parser {
    pos: usize,
    input: String
}

impl Parser {
    fn new(s: &str) -> Parser {
        Parser {
            pos: 0,
            input: s.to_string()
        }
    }

    fn peek(&self) -> Option<char> {
        let mut iter = self.input[self.pos..].chars();
        iter.next()
    }

    fn pop(&self) -> ParseResult<char> {
        let mut iter = self.input[self.pos..].char_indices();
        let (_, c) = try!(iter.next().ok_or((ParseError::EOF, self.clone())));
        let (pos_inc, _) = iter.next().unwrap_or((1, ' '));
        let q = Parser {
            pos: self.pos + pos_inc,
            input: self.input.clone()
        };
        Ok((c, q))
    }

    fn pop_if<P>(&self, pred: P, msg: &str) -> ParseResult<char>
        where P: Fn(char) -> bool {
        let res = try!(self.pop());
        if pred(res.0) {
            Ok(res)
        } else {
            Err((ParseError::ExpectedPred(msg.to_string()), self.clone()))
        }
    }

    fn pop_while<'a, P>(&'a self, pred: P) -> (String, Parser)
        where P: Fn(char) -> bool {
        let mut p = self.clone();
        let mut res = String::new();
        loop {
            match p.pop() {
                Ok((c, ref q)) if pred(c) => {
                    p = q.clone();
                    res.push(c);
                },
                _ => return (res, p)
            }
        }
    }

    fn parse_token(&self) -> ParseResult<Value> {
        let ccs = self.pop_while(is_constituent);
        if ccs.0.is_empty() {
            match self.peek() {
                Some(c) =>
                    Err((ParseError::ExpectedConstituent(c), self.clone())),
                None =>
                    Err((ParseError::EOF, self.clone()))
            }
        } else {
            match ccs.0.parse() {
                Ok(v) => Ok((v, ccs.1)),
                Err(msg) => Err((msg, self.clone()))
            }
        }
    }

    fn parse_char(&self) -> ParseResult<Value> {
        let (_, p) = try!(self.pop_if(|c| c == '\\', "char literal \\..."));
        let (s, q) = p.pop_while(char::is_alphabetic);
        match s.as_ref() {
            "newline" => Ok((Value::Char('\n'), q)),
            "space" => Ok((Value::Char(' '), q)),
            "tab" => Ok((Value::Char('\t'), q)),
            "return" => Ok((Value::Char('\r'), q)),
            _ if s.chars().count() == 1 =>
                Ok((Value::Char(s.chars().next().unwrap()), q)),
            _ => Err((ParseError::InvalidCharDesc, self.clone()))
        }
    }

    fn parse_pounded(&self) -> ParseResult<Value> {
        match self.peek() {
            Some('(') => {
                let (vs, q) = self.pop().unwrap().1.parse_exprs();
                let (_, r) = try!(q.pop_while(char::is_whitespace)
                                  .1.pop_if(|c| c == ')', "list terminator \\)"));
                Ok((prepend(
                    Rc::new(Value::Symbol(Some("centring.lang".to_string()),
                                          "Tuple".to_string())),
                    Rc::new(vec_to_list(vs))),
                    r))
            },
            Some('[') => {
                let (vs, q) = self.pop().unwrap().1.parse_exprs();
                let (_, r) = try!(q.pop_while(char::is_whitespace)
                                  .1.pop_if(|c| c == ']',
                                            "array terminator \\]"));
                Ok((prepend(
                    Rc::new(Value::Symbol(Some("centring.lang".to_string()),
                                          "Array".to_string())),
                    Rc::new(vec_to_list(vs))),
                    r))
            },
            Some('t') => Ok((Value::Bool(true), self.pop().unwrap().1)),
            Some('f') => Ok((Value::Bool(false), self.pop().unwrap().1)),
            _ => Err((ParseError::UnknownPounded, self.clone()))
        }
    }            

    fn parse_expr(&self) -> ParseResult<Value> {
        match self.peek() {
            Some(c) if is_constituent(c) => self.parse_token(),
            Some(c) if c.is_whitespace() => {
                self.pop_while(char::is_whitespace).1.parse_expr()
            },
            Some('(') => {
                let (_, p) = self.pop().unwrap();
                let (vs, q) = p.parse_exprs();
                let (_, r) = try!(q.pop_while(char::is_whitespace)
                                  .1.pop_if(|c| c == ')', "list terminator \\)"));
                Ok((vec_to_list(vs), r))
            },
            Some('\\') => self.parse_char(),
            Some('#') => self.pop().unwrap().1.parse_pounded(),
            Some(c) => Err((ParseError::Illegal(c), self.clone())),
            None => Err((ParseError::EOF, self.clone()))
        }
    }

    fn parse_exprs(&self) -> (Vec<ValueRef>, Parser) {
        let mut p = self.clone();
        let mut res = vec![];
        loop {
            if let Ok((v, q)) = p.parse_expr() {
                res.push(Rc::new(v));
                p = q.clone();
            } else {
                return (res, p)
            }
        }
    }
}

fn is_constituent(c: char) -> bool {
    c.is_alphanumeric() || ".!?&|<=>+-*/".contains(c)
}

impl FromStr for Value {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, ParseError> {
        if let Ok(i) = s.parse().map(Value::Int) {
            Ok(i)
        } else {
            // If the first '/' of the symbol is not the first or last char
            // the symbol is module-qualified:
            match s.chars().position(|c| c == '/') {
                Some(i) if 0 < i && i < s.len() - 1 =>
                    Ok(Value::Symbol(Some(s[0..i].to_string()),
                                     s[i+1..].to_string())),
                _ => Ok(Value::Symbol(None, s.to_string()))
            }
        }
    }
} 

// Print

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Bool(true) => write!(f, "#t"),
            Value::Bool(false) => write!(f, "#f"),
            Value::Char(c) => {
                match c {
                    '\n' => write!(f, "\\newline"),
                    ' ' => write!(f, "\\space"),
                    '\t' => write!(f, "\\tab"),
                    '\r' => write!(f, "\\return"),
                    _ => write!(f, "\\{}", c)
                }
            },  
            Value::Symbol(Some(ref mod_name), ref name) =>
                write!(f, "{}/{}", mod_name, name),
            Value::Symbol(None, ref name) => write!(f, "{}", name),

            Value::Tuple(ref vs) => {
                try!(write!(f, "#("));
                for v in vs { try!(write!(f, " {}", v)) }
                write!(f, ")")
            },
            Value::Array(ref vs) => {
                try!(write!(f, "#["));
                for v in vs { try!(write!(f, " {}", v)) }
                write!(f, "]")
            },
            Value::Pair { first: ref v, rest: ref vs } => {
                try!(write!(f, "("));
                let mut h = v;
                let mut t = vs;
                loop {
                    try!(write!(f, "{}", h));
                    match **t {
                        Value::EmptyList => break,
                        Value::Pair { first: ref v, rest: ref vs } => {
                            h = v;
                            t = vs;
                        },
                        _ => { try!(write!(f, " . {}", t)); break }
                    }
                    try!(write!(f, " "));
                }
                write!(f, ")")
            },
            Value::EmptyList => write!(f, "()"),

            Value::Singleton { ref typ } => write!(f, "#=({})", typ),
            Value::Record { ref typ, vals: ref vs } => {
                try!(write!(f, "#=({}", typ));
                for v in vs { try!(write!(f, " {}", v)) }
                write!(f, ")")
            },

            Value::AbstractType { ref name, .. } => write!(f, "{}", name),
            Value::SingletonType { ref name, .. } => write!(f, "{}", name),
            Value::RecordType { ref name, .. } => write!(f, "{}", name),
            Value::BuiltInType { ref name, .. } => write!(f, "{}", name),

            Value::Fn { ref name, formal_types: ref ftps, .. } => {
                try!(write!(f, "#<Fn {} (", name));
                for ftp in ftps { try!(write!(f, " {}", ftp)) }
                write!(f, ")>")
            },
            Value::NativeFn { ref name, formal_types: ref ftps, .. } => {
                try!(write!(f, "#<NativeFn {} (", name));
                for ftp in ftps { try!(write!(f, " {}", ftp)) }
                write!(f, ")>")
            }
        }
    }
}

// Eval

impl Interpreter {
    fn new() -> Interpreter {
        Interpreter
    }
    
    fn eval(&mut self, expr: Value) -> ValueRef {
        Rc::new(expr)
    }
}

// Main

fn main () {
    println!("{}",
             Parser::new("#(\\a \\ä \\newline)").parse_expr().unwrap().0);
}
