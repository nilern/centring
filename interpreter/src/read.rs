use interpreter::{Interpreter, ValueHandle};
use value::Bits;

pub struct ParseState {
    str: String,
    pos: usize
}

#[derive(Debug)]
pub enum ParseError {
    EOF,
    Unsatisfied
}

pub type ParseResult<T> = Result<T, ParseError>;

impl ParseState {
    /// Create a new `ParseState` for parsing `str`.
    pub fn new(str: String) -> ParseState {
        ParseState {
            str: str,
            pos: 0
        }
    }

    /// Return the position (byte index) the `ParseState` is at.
    pub fn tell(&self) -> usize {
        self.pos
    }

    /// Set the position (byte index) of the `ParseState` to `pos`.
    pub fn seek(&mut self, pos: usize) {
        self.pos = pos;
    }

    /// Return the character that the `ParseState` is at. Returns `None` if
    /// it is past the end of the parsee.
    pub fn peek(&self) -> Option<char> {
        self.str[self.pos..].chars().next()
    }

    /// Return the character that the `ParseState` is at. Returns
    /// `Err(ParseError::EOF)` if it is past the end of the parsee.
    pub fn pop(&mut self) -> ParseResult<char> {
        if self.pos >= self.str.len() {
            Err(ParseError::EOF)
        } else {
            let mut iter = self.str[self.pos..].char_indices();
            let res = iter.next().unwrap().1;
            self.pos += iter.next().unwrap_or((1, ' ')).0;
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
                Err(ParseError::Unsatisfied)
            }
        },
        err @ Err(_) => {
            st.seek(oldpos);
            err
        }
    }
}

fn digit(st: &mut ParseState) -> ParseResult<usize> {
    sat(|c| c.is_digit(10), st)
        .map(|c| c.to_digit(10).unwrap() as usize)
}

pub fn int(itp: &mut Interpreter, st: &mut ParseState)
    -> ParseResult<ValueHandle> {
    let mut n = try!(digit(st)) as isize;
    while let Ok(d) = digit(st) {
        n = n*10 + d as isize;
    }
    Ok(itp.alloc(Bits::new(n)))
}

/// # Tests

#[cfg(test)]
mod tests {
    use super::{ParseState, int};
    use interpreter::Interpreter;
    use value::{Bits, Unbox};

    #[test]
    fn read_int() {
        let mut itp = Interpreter::new();
        let mut st = ParseState::new(String::from("235"));
        let res = int(&mut itp, &mut st);
        assert!(res.is_ok());
        let pptr = res.unwrap();
        let ptr = pptr.borrow().as_ptr() as *const Bits<isize>;
        unsafe {
            assert_eq!((*ptr).unbox(), 235);
        }
    }
}
