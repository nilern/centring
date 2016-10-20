use interpreter::Interpreter;
use value::{Bits, ListPair, ListEmpty};
use refs::Root;

pub struct ParseState {
    str: String,
    pos: usize,
    line: usize,
    col: usize
}

#[derive(Debug)]
pub enum ParseError {
    EOF,
    Unsatisfied,
    Extraneous
}

pub type ParseResult<T> = Result<T, ParseError>;

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
            Err(ParseError::EOF)
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
}

fn sat<F: Fn(char) -> bool>(f: F, st: &mut ParseState) -> ParseResult<char> {
    let oldpos = st.tell();
    match st.pop() {
        Ok(c) => {
            if f(c) {
                Ok(c)
            } else {
                st.seek(oldpos);
                Err(ParseError::Unsatisfied)
            }
        },
        err @ Err(_) => {
            st.seek(oldpos);
            err
        }
    }
}

fn ws_one(st: &mut ParseState) -> ParseResult<()> {
    sat(char::is_whitespace, st)
    .map(|_| ())
    .or_else(|_| {
        if let Some(';') = st.peek() {
            let _ = st.pop();
            while let Ok(_) = sat(|c| {c != '\n' && c != '\r'}, st) { }
            Ok(())
        } else {
            Err(ParseError::Unsatisfied)
        }
    })
}

fn ws_many(st: &mut ParseState) {
    while let Ok(_) = ws_one(st) { }
}

fn digit(st: &mut ParseState) -> ParseResult<usize> {
    sat(|c| c.is_digit(10), st)
        .map(|c| c.to_digit(10).unwrap() as usize)
}

fn int(itp: &mut Interpreter, st: &mut ParseState) -> ParseResult<Root> {
    let mut n = try!(digit(st)) as isize;
    while let Ok(d) = digit(st) {
        n = n*10 + d as isize;
    }
    Ok(itp.alloc(Bits::new(n)))
}

fn list(itp: &mut Interpreter, st: &mut ParseState) -> ParseResult<Root> {
    if let Some(')') = st.peek() {
        try!(st.pop());
        let nil = ListEmpty::new(itp);
        Ok(itp.alloc(nil))
    } else {
        let v = try!(expr(itp, st));
        list(itp, st)
        .map(|l| {
            let pair = ListPair::new(itp, v.ptr(), l.ptr());
            itp.alloc(pair)
        })
    }
}

fn expr(itp: &mut Interpreter, st: &mut ParseState) -> ParseResult<Root> {
    ws_many(st);
    let res = match st.peek() {
        Some('(') => {
            let _ = st.pop();
            list(itp, st)
        },
        _ => int(itp, st)
    };
    ws_many(st);
    res
}

pub fn read(itp: &mut Interpreter, st: &mut ParseState) -> ParseResult<Root> {
    let res = expr(itp, st);
    if let Err(ParseError::EOF) = st.pop() {
        res
    } else {
        Err(ParseError::Extraneous)
    }
}

/// # Tests

#[cfg(test)]
mod tests {
    use super::{ParseState, read};
    use interpreter::Interpreter;
    use value::{Bits, ListPair, ListEmpty, Downcast, Unbox};

    #[test]
    fn int_list() {
        let mut itp = Interpreter::new();
        let mut st = ParseState::new(String::from("(235)"));
        let res = read(&mut itp, &mut st);
        assert!(res.is_ok());
        unsafe {
            let opp: Option<&ListPair> = (*res.unwrap().ptr()).downcast(&mut itp);
            assert!(opp.is_some());
            let pp = opp.unwrap();
            let n: Option<&Bits<isize>> = (*(*pp).head).downcast(&mut itp);
            let tail: Option<&ListEmpty> = (*(*pp).tail).downcast(&mut itp);
            assert!(n.is_some());
            assert!(tail.is_some());
            assert_eq!(n.unwrap().unbox(), 235);
        }
    }
}
