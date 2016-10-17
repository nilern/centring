
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

pub fn int(st: &mut ParseState) -> ParseResult<isize> {
    let mut res = try!(digit(st)) as isize;
    while let Ok(d) = digit(st) {
        res = res*10 + d as isize;
    }
    Ok(res)
}

/// # Tests

#[cfg(test)]
mod tests {
    use super::{ParseState, int};

    #[test]
    fn read_int() {
        let mut st = ParseState::new(String::from("235"));
        let res = int(&mut st);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), 235);
    }
}
