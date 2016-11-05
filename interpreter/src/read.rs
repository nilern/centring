use interpreter::Interpreter;
use value::{Any, Int, Bool, Symbol, ListPair, ListEmpty};
use refs::Root;

pub struct ParseState {
    str: String,
    pos: usize,
    line: usize,
    col: usize,
}

enum CharKind {
    Constituent(char),
    Macro { terminating: bool, char: char },
    Whitespace,
}
use self::CharKind::*;

#[derive(Debug)]
pub enum ParseErrorCode {
    /// End of input reached prematurely.
    EOF,
    /// A character failed to satisfy some condition.
    Unsatisfied,
    /// Got too many input expressions.
    Extraneous,
    /// Was expecting a terminating character (such as ')').
    NonTerminating(char),
    NonSharp(char),
    Unexpected(char)
}
use self::ParseErrorCode::*;

#[derive(Debug)]
pub struct ParseError {
    /// Description of the error.
    code: ParseErrorCode,
    /// The character index where the error occurred.
    pos: usize,
    /// The line number where the error occurred.
    line: usize,
    /// The column where the error occurred.
    col: usize,
}

pub type ParseResult<T> = Result<T, ParseError>;

pub type ReadResult<T> = ParseResult<Option<Root<T>>>;

impl ParseState {
    /// Create a new `ParseState` for parsing `str`.
    pub fn new(str: String) -> ParseState {
        ParseState {
            str: str,
            pos: 0,
            line: 1,
            col: 1,
        }
    }

    // Return the coordinates the `ParseState` is at.
    fn tell(&self) -> (usize, usize, usize) {
        (self.pos, self.line, self.col)
    }

    // Set the coordinates) of the `ParseState`.
    fn seek(&mut self, (pos, line, col): (usize, usize, usize)) {
        self.pos = pos;
        self.line = line;
        self.col = col;
    }

    // Return the character that the `ParseState` is at. Returns `None` if
    // it is past the end of the parsee.
    fn peek(&self) -> Option<char> {
        self.str[self.pos..].chars().next()
    }

    // Return the character that the `ParseState` is at. Returns
    // `Err(ParseError::EOF)` if it is past the end of the parsee.
    fn pop(&mut self) -> ParseResult<char> {
        if self.pos >= self.str.len() {
            Err(self.place_error(EOF))
        } else {
            let mut iter = self.str[self.pos..].char_indices();
            let res = iter.next().unwrap().1;
            self.pos += iter.next().unwrap_or((1, ' ')).0;
            if '\n' == res {
                // TODO: '\r'
                self.col = 1;   // at the start...
                self.line += 1; // ...of the next line
            } else {
                self.col += 1;
            }
            Ok(res)
        }
    }

    // Add the current position information to a `ParseErrorCode`.
    fn place_error(&self, code: ParseErrorCode) -> ParseError {
        ParseError {
            code: code,
            pos: self.pos,
            line: self.line,
            col: self.col,
        }
    }
}

impl CharKind {
    fn new(c: char) -> CharKind {
        if c == '(' || c == ')' || c == ';' {
            Macro {
                terminating: true,
                char: c,
            }
        } else if c == '#' {
            Macro {
                terminating: false,
                char: c,
            }
        } else if c.is_whitespace() {
            Whitespace
        } else {
            Constituent(c)
        }
    }
}

fn sat<F: Fn(char) -> bool>(f: F, st: &mut ParseState) -> ParseResult<char> {
    let oldpos = st.tell();
    match st.pop() {
        Ok(c) => {
            if f(c) {
                Ok(c)
            } else {
                let err = st.place_error(Unsatisfied);
                st.seek(oldpos);
                Err(err)
            }
        }
        err @ Err(_) => {
            st.seek(oldpos);
            err
        }
    }
}

fn comment(st: &mut ParseState) {
    while let Ok(_) = sat(|c| c != '\n' && c != '\r', st) {
    }
}

fn digit(st: &mut ParseState) -> ParseResult<usize> {
    sat(|c| c.is_digit(10), st).map(|c| c.to_digit(10).unwrap() as usize)
}

fn int(itp: &mut Interpreter, st: &mut ParseState) -> ReadResult<Int> {
    let mut n = try!(digit(st)) as isize;
    while let Ok(d) = digit(st) {
        n = n * 10 + d as isize;
    }
    Ok(Some(Int::new(itp, n)))
}

fn symbol_char(st: &mut ParseState) -> ParseResult<char> {
    match st.peek().map(CharKind::new) {
        Some(Constituent(_)) | Some(Macro { terminating: false, .. }) => st.pop(),
        Some(_) => Err(st.place_error(Unsatisfied)),
        None => Err(st.place_error(EOF)),
    }
}

fn symbol_string(st: &mut ParseState, mut chars: String) -> ParseResult<String> {
    chars.push(try!(symbol_char(st)));
    while let Ok(c) = symbol_char(st) {
        chars.push(c);
    }
    Ok(chars)
}

fn symbol(itp: &mut Interpreter, st: &mut ParseState) -> ReadResult<Symbol> {
    symbol_string(st, String::new()).map(|cs| Some(Symbol::new(itp, &cs)))
}

fn qualified_symbol(itp: &mut Interpreter, st: &mut ParseState) -> ReadResult<Symbol> {
    symbol_string(st, String::from("##")).map(|cs| Some(Symbol::new(itp, &cs)))
}

fn list(itp: &mut Interpreter, st: &mut ParseState) -> ReadResult<Any> {
    if let Some(')') = st.peek() {
        let _ = st.pop();
        Ok(Some(ListEmpty::new(itp).as_any_ref()))
    } else {
        match expr(itp, st) {
            Ok(Some(head)) => {
                match list(itp, st) {
                    Ok(Some(tail)) => {
                        let ls = ListPair::new(itp, head, tail);
                        Ok(Some(ls.as_any_ref()))
                    }
                    Ok(None) => Err(st.place_error(NonTerminating(')'))),
                    err @ Err(_) => err,
                }
            }
            Ok(None) => Err(st.place_error(NonTerminating(')'))),
            err @ Err(_) => err,
        }
    }
}

fn expr(itp: &mut Interpreter, st: &mut ParseState) -> ReadResult<Any> {
    loop {
        match st.peek().map(CharKind::new) {
            Some(Macro { char: '(', terminating: true }) => {
                let _ = st.pop();
                return list(itp, st);
            }
            Some(Macro { char: ';', terminating: true }) => {
                let _ = st.pop();
                comment(st);
            }
            Some(Macro { char: '#', terminating: false}) => {
                let _ = st.pop();
                match st.peek() {
                    Some('#') => {
                        let _ = st.pop();
                        return qualified_symbol(itp, st).map(|ov| ov.map(Root::as_any_ref))
                    },
                    Some('t') => {
                        let _ = st.pop();
                        return Ok(Some(Bool::new(itp, true).as_any_ref()));
                    },
                    Some('f') => {
                        let _ = st.pop();
                        return Ok(Some(Bool::new(itp, false).as_any_ref()));
                    },
                    Some(c) => return Err(st.place_error(NonSharp(c))),
                    None => return Err(st.place_error(EOF))
                }
            },
            Some(Macro { char: c, .. }) =>
                return Err(st.place_error(Unexpected(c))),
            Some(Whitespace) => {
                let _ = st.pop();
            }
            Some(Constituent(_)) => {
                return int(itp, st)
                    .map(|ov| ov.map(Root::as_any_ref))
                    .or_else(|_| symbol(itp, st).map(|ov| ov.map(Root::as_any_ref)))
            }
            None => return Ok(None),
        }
    }
}

/// Read an entire input string. If it contains a single expression (possibly
/// surrounded by whitespace and comments), return `Ok(Some(_))`, if it just
/// contains whitespace and comments return `Ok(None)`, else return
/// a `ParseError`.
pub fn read(itp: &mut Interpreter, st: &mut ParseState) -> ReadResult<Any> {
    let res = expr(itp, st);
    if let Ok(None) = expr(itp, st) {
        res
    } else {
        Err(st.place_error(Extraneous))
    }
}

/// # Tests
#[cfg(test)]
mod tests {
    use super::{ParseState, read};
    use interpreter::Interpreter;
    use value::{ListPair, Unbox, Int};

    #[test]
    fn int_list() {
        let mut itp = Interpreter::new();
        let mut st = ParseState::new(String::from("(235)"));
        let res = read(&mut itp, &mut st);
        let app = res.unwrap().unwrap();
        let pp = app.borrow().downcast::<ListPair>(&itp).unwrap();
        let nr = pp.first();
        let n = nr.borrow().downcast::<Int>(&itp).unwrap().unbox();
        assert_eq!(n, 235);
    }
}
