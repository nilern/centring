use interpreter::Interpreter;
use refs::ValueHandle;
use value::{CtrValue, ConcreteType, Unbox, Int, Symbol, ListPair, ListEmpty};

use std::fmt;
use std::fmt::{Display, Formatter};

pub struct ContextValue<'a, T: CtrValue + 'a> {
    val: ValueHandle<'a, T>,
    itp: &'a Interpreter,
}

impl<'a, T: CtrValue> ContextValue<'a, T> {
    pub fn new(val: ValueHandle<'a, T>, itp: &'a Interpreter) -> ContextValue<'a, T> {
        ContextValue::<'a, T> {
            val: val,
            itp: itp,
        }
    }
}

impl<'a, T: CtrValue> Display for ContextValue<'a, T> {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), fmt::Error> {
        if let Some(sym) = self.val.downcast::<Symbol>(self.itp) {
            write!(fmt, "{}", String::from_utf8_lossy(&sym.clone_bytes().unwrap()))
        } else if let Some(n) = self.val.downcast::<Int>(self.itp) {
            write!(fmt, "{}", n.unbox())
        } else if let Some(pair) = self.val.downcast::<ListPair>(self.itp) {
            try!(write!(fmt, "("));
            let mut start = true;
            for v in pair.iter(self.itp) {
                if !start {
                    try!(write!(fmt, " "));
                } else {
                    start = false;
                }
                try!(ContextValue::new(v.borrow(), self.itp).fmt(fmt));
            }
            write!(fmt, ")")
        } else if self.val.instanceof(ListEmpty::typ(self.itp)) {
            write!(fmt, "()")
        } else {
            unimplemented!()
        }
    }
}
