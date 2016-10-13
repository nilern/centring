use gc::{ValueRef, GCState};
use ops::CheckedAdd;

pub struct Interpreter {
    gc: GCState,
    stack: Vec<ValueRef>
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            gc: GCState::new(1024),
            stack: vec![]
        }
    }

    pub fn push<T: Copy>(&mut self, v: T) {
        unsafe { self.stack.push(self.gc.alloc(v)); }
    }

    pub fn pop<T: Copy>(&mut self) -> Option<T> {
        unsafe { self.stack.pop().map(|vref| vref.unbox()) }
    }

    fn add<T: Copy + CheckedAdd>(&mut self) {
        let a: T = self.pop().unwrap();
        let b: T = self.pop().unwrap();
        let c: T = a.checked_add(b).unwrap();
        self.push(c);
    }
}

/// # Tests

#[cfg(test)]
mod tests {
    use super::Interpreter;

    #[test]
    fn i64add() {
        let mut itp = Interpreter::new();
        itp.push::<i64>(2);
        itp.push::<i64>(3);
        itp.add::<i64>();
        assert_eq!(itp.pop::<i64>().unwrap(), 5);
    }
}
