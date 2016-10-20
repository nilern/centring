use interpreter::Interpreter;
use refs::Root;

pub struct ParseState {
    str: String,
    pos: usize,
    line: usize,
    col: usize
}

#[derive(Debug)]
pub enum ParseErrorCode {
    /// End of input reached prematurely.
    EOF,
    /// A character failed to satisfy some condition.
    Unsatisfied,
    /// Got too many input expressions.
    Extraneous,
    /// Was expecting a terminating character (such as ')').
    NonTerminating(char)
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
    col: usize
}

pub type ParseResult<T> = Result<T, ParseError>;

pub type ReadResult = ParseResult<Option<Root>>;

impl ParseState {
    /// Create a new `ParseState` for parsing `str`.
    pub fn new(str: String) -> ParseState {
        ParseState {
            str: str,
            pos: 0,
            line: 1,
            col: 1
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
            if '\n' == res { // TODO: '\r'
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
            col: self.col
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
        },
        err @ Err(_) => {
            st.seek(oldpos);
            err
        }
    }
}

fn comment(st: &mut ParseState) {
    while let Ok(_) = sat(|c| c != '\n' && c != '\r', st) { }
}

fn digit(st: &mut ParseState) -> ParseResult<usize> {
    sat(|c| c.is_digit(10), st)
        .map(|c| c.to_digit(10).unwrap() as usize)
}

fn int(itp: &mut Interpreter, st: &mut ParseState) -> ReadResult {
    let mut n = try!(digit(st)) as isize;
    while let Ok(d) = digit(st) {
        n = n*10 + d as isize;
    }
    Ok(Some(itp.alloc_int(n)))
}

fn list(itp: &mut Interpreter, st: &mut ParseState) -> ReadResult {
    if let Some(')') = st.peek() {
        let _ = st.pop();
        Ok(Some(itp.alloc_nil()))
    } else {
        match expr(itp, st) {
            Ok(Some(head)) => match list(itp, st) {
                Ok(Some(tail)) => {
                    let ls = itp.alloc_pair(head.borrow(), tail.borrow());
                    //let ls = ListPair::new(itp, head.ptr(), tail.ptr());
                    Ok(Some(ls))
                },
                Ok(None) => Err(st.place_error(NonTerminating(')'))),
                err @ Err(_) => err
            },
            Ok(None) => Err(st.place_error(NonTerminating(')'))),
            err @ Err(_) => err
        }
    }
}

fn expr(itp: &mut Interpreter, st: &mut ParseState) -> ReadResult {
    loop {
        match st.peek() {
            Some('(') => {
                let _ = st.pop();
                return list(itp, st)
            },
            Some(';') => {
                let _ = st.pop();
                comment(st);
            },
            Some(c) if c.is_whitespace() => {
                let _ = st.pop();
            },
            Some(_) => return int(itp, st),
            None => return Ok(None)
        };
    }
}

/// Read an entire input string. If it contains a single expression (possibly
/// surrounded by whitespace and comments), return `Ok(Some(_))`, if it just
/// contains whitespace and comments return `Ok(None)`, else return
/// a `ParseError`.
pub fn read(itp: &mut Interpreter, st: &mut ParseState) -> ReadResult {
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
    use value::{Bits, ListPair, Downcast, Unbox};

    #[test]
    fn int_list() {
        let mut itp = Interpreter::new();
        let mut st = ParseState::new(String::from("(235)"));
        let res = read(&mut itp, &mut st);
        unsafe {
            let opp: Option<&ListPair> = (*res.unwrap().unwrap().ptr()).downcast(&mut itp);
            let pp = opp.unwrap();
            let n: Option<&Bits<isize>> = (*(*pp).head).downcast(&mut itp);
            assert_eq!(n.unwrap().unbox(), 235);
        }
    }
}
