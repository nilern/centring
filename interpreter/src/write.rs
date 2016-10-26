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
        if self.val.instanceof(ListPair::typ(self.itp))
           || self.val.instanceof(ListEmpty::typ(self.itp)) {
           try!(write!(fmt, "("));
           write_list(fmt, self, true)
        } else if let Some(n) = self.val.downcast::<Int>(self.itp) {
            write!(fmt, "{}", n.unbox())
        } else if let Some(sym) = self.val.downcast::<Symbol>(self.itp) {
            write!(fmt, "{}", String::from_utf8_lossy(&sym.clone_bytes().unwrap()))
        } else {
            unimplemented!()
        }
    }
}

fn write_list<T: CtrValue>(fmt: &mut Formatter, ls: &ContextValue<T>, start: bool)
                           -> Result<(), fmt::Error> {
    if let Some(pair) = ls.val.downcast::<ListPair>(ls.itp) {
        if !start {
            try!(write!(fmt, " "));
        }
        try!(ContextValue::new(pair.first().borrow(), ls.itp).fmt(fmt));
        write_list(fmt, &ContextValue::new(pair.rest().borrow(), ls.itp), false)
    } else if let Some(nil) = ls.val.downcast::<ListEmpty>(ls.itp) {
        write!(fmt, ")")
    } else {
        panic!()
    }
}
