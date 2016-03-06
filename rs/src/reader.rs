use value::{Value, ValueRef, List, prepend};
use std::str::FromStr;
use std::rc::Rc;

#[derive(Debug)]
pub enum ParseError {
    EOF,
    Illegal(char),
    ExpectedPred(String),
    ExpectedConstituent(char),
    UnknownPounded,
    InvalidCharDesc
}

pub type ParseResult<T> = Result<(T, Parser), (ParseError, Parser)>;

#[derive(Debug, Clone)]
pub struct Parser {
    pos: usize,
    input: String
}

impl Parser {
    pub fn new(s: &str) -> Parser {
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
                Ok((Value::List(prepend(
                    Rc::new(Value::Symbol(Some("centring.lang".to_string()),
                                          "Tuple".to_string())),
                    &Rc::new(vs.into_iter().collect()))),
                    r))
            },
            Some('[') => {
                let (vs, q) = self.pop().unwrap().1.parse_exprs();
                let (_, r) = try!(q.pop_while(char::is_whitespace)
                                  .1.pop_if(|c| c == ']',
                                            "array terminator \\]"));
                Ok((Value::List(prepend(
                    Rc::new(Value::Symbol(Some("centring.lang".to_string()),
                                          "Array".to_string())),
                    &Rc::new(vs.into_iter().collect()))),
                    r))
            },
            Some('t') => Ok((Value::Bool(true), self.pop().unwrap().1)),
            Some('f') => Ok((Value::Bool(false), self.pop().unwrap().1)),
            _ => Err((ParseError::UnknownPounded, self.clone()))
        }
    }            

    pub fn parse_expr(&self) -> ParseResult<Value> {
        match self.peek() {
            Some(c) if is_constituent(c) => self.parse_token(),
            Some(c) if c.is_whitespace() =>
                self.pop_while(char::is_whitespace).1.parse_expr(),
            Some('(') => {
                let (vs, q) = self.pop().unwrap().1.parse_exprs();
                let (_, r) = try!(q.pop_while(char::is_whitespace)
                                  .1.pop_if(|c| c == ')', "list terminator \\)"));
                Ok((Value::List(vs.into_iter().collect()), r))
            },
            Some('\\') => self.parse_char(),
            Some('#') => self.pop().unwrap().1.parse_pounded(),
            Some('\'') => {
                let (v, q) = try!(self.pop().unwrap().1.parse_expr());
                Ok((Value::List(prepend(
                    Rc::new(Value::Symbol(Some("centring.lang".to_string()),
                                          "quote".to_string())),
                    &Rc::new(prepend(Rc::new(v), &Rc::new(List::Empty))))),
                    q))
            },
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
