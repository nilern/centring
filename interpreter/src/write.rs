use refs::ValueHandle;
use value::{CtrValue, ConcreteType, Unbox,
            Int, Bool, Symbol, ListPair, ListEmpty, FnClosure};

use std::fmt;
use std::fmt::{Display, Formatter};

impl<'a, T: CtrValue> Display for ValueHandle<'a, T> {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), fmt::Error> {
        typecase!(self; {
            sym: Symbol => {
                write!(fmt, "{}", Symbol::to_string(&*sym))
            },
            n: Int => { write!(fmt, "{}", n.unbox()) },
            b: Bool => { if b.unbox() { write!(fmt, "#t") } else { write!(fmt, "#t") } },
            pair: ListPair => {
                try!(write!(fmt, "("));
                let mut start = true;
                for v in pair.iter() {
                    if !start {
                        try!(write!(fmt, " "));
                    } else {
                        start = false;
                    }
                    try!(v.borrow().fmt(fmt));
                }
                write!(fmt, ")")
            },
            ListEmpty => { write!(fmt, "()") },
            FnClosure => { write!(fmt, "#<Fn>") },
            _ => { unimplemented!() }
        })
    }
}
