use interpreter::Interpreter;
use refs::ValueHandle;
use value::{CtrValue, ConcreteType, Unbox, Int, Bool, Symbol, ListPair, ListEmpty};

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
        typecase!(self.val, self.itp; {
            sym: Symbol => { write!(fmt, "{}", sym.to_string()) },
            n: Int => { write!(fmt, "{}", n.unbox()) },
            b: Bool => { if b.unbox() { write!(fmt, "#t") } else { write!(fmt, "#t") } },
            pair: ListPair => {
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
            },
            ListEmpty => { write!(fmt, "()") },
            _ => { unimplemented!() }
        })
    }
}
