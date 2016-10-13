
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
    pub fn new(str: String) -> ParseState {
        ParseState {
            str: str,
            pos: 0
        }
    }

    fn tell(&self) -> usize {
        self.pos
    }

    fn seek(&mut self, i: usize) {
        self.pos = i;
    }

    fn peek(&self) -> Option<char> {
        self.str[self.pos..].chars().next()
    }

    fn char(&mut self) -> ParseResult<char> {
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
    match st.char() {
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
        println!("{:?}", int(&mut st));
    }
}
