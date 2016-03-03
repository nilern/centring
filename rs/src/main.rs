use std::rc::Rc;
use std::collections::HashMap;
use std::fmt;

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

// Read

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
    
    fn any_char(&mut self) -> Result<char, &str> {
        let mut iter = self.input[self.pos..].char_indices();
        let (_, c) = try!(iter.next().ok_or("EOF"));
        let (pos_inc, _) = iter.next().unwrap_or((1, ' '));
        self.pos += pos_inc;
        Ok(c)
    }

    fn sat<P>(&mut self, pred: P) -> Result<char, &str>
        where P: Fn(char) -> bool {
        let c = try!(self.any_char());
        if pred(c) { Ok(c) } else { Err("pred failed") }
    }

    fn take_while<P>(&mut self, pred: P) -> String
        where P: Fn(char) -> bool {
        let mut res = String::new();
        loop {
            match self.any_char() {
                Ok(c) if pred(c) => res.push(c),
                _ => return res
            }
        }
    }

    fn parse_int(&mut self) -> Result<Value, &str> {
        let ds = self.take_while(|c| c.is_digit(10));
        match ds.parse() {
            Err(_) => Err("not an int"),
            Ok(i) => Ok(Value::Int(i))
        }
    }

    fn parse_bool(&mut self) -> Result<Value, &str> {
        match self.any_char() {
            Ok('t') => Ok(Value::Bool(true)),
            Ok('f') => Ok(Value::Bool(false)),
            Err(msg) => Err(msg),
            _ => Err("not a boolean")
        }
    }

    fn parse_coll(&mut self, term: char) -> Result<Value, &str> {
        let exprs = self.parse_exprs();
        try!(self.sat(|c| c == term).or(Err("missing terminator")));
        let mut res = Value::EmptyList;
        for v in exprs.into_iter().rev() {
            res = Value::Pair { first: v, rest: Rc::new(res) };
        }
        Ok(res)
    }

    fn parse_pounded(&mut self) -> Result<Value, &str> {
        match self.peek() {
            Some('t') | Some('f') => self.parse_bool(),
            Some('(') => {
                self.any_char();
                let vs = try!(self.parse_coll(')'));
                Ok(Value::Pair {
                    first: Rc::new(
                        Value::Symbol(Some("centring.lang".to_string()),
                                      "Tuple".to_string())),
                    rest: Rc::new(vs)
                })
            },
            Some('[') => {
                self.any_char();
                let vs = try!(self.parse_coll(']'));
                Ok(Value::Pair {
                    first: Rc::new(
                        Value::Symbol(Some("centring.lang".to_string()),
                                      "Array".to_string())),
                    rest: Rc::new(vs)
                })
            },
            None => Err("EOF"),
            _ => Err("unrecognized pounded")
        }
    }
    
    fn parse_expr(&mut self) -> Result<Value, &str> {
        loop {
            match self.peek() {
                Some(c) if c.is_digit(10) => return self.parse_int(),
                Some(c) if c.is_whitespace() => self.any_char(),
                Some('(') => {
                    self.any_char();
                    return self.parse_coll(')');
                },
                Some('#') => {
                    self.any_char();
                    return self.parse_pounded()
                },
                _ => return Err("not a valid expr")
            };
        }
    }

    fn parse_exprs(&mut self) -> Vec<ValueRef> {
        let mut res = vec![];
        loop {
            if let Ok(v) = self.parse_expr() {
                res.push(Rc::new(v))
            } else {
                return res
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
            Value::Char(c) => write!(f, "\\{}", c),
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
    let mut itp = Interpreter::new();
    println!("{}", itp.eval(Parser::new("(#t 42 )").parse_expr().unwrap()));
}
