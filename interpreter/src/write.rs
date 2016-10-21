use interpreter::Interpreter;
use refs::{Root, ValueHandle};
use value::{CtrValue, Any, Bits, ListPair, Downcast};

use std::fmt;
use std::fmt::{Display, Formatter};

pub struct ContextValue<'a, T: CtrValue + 'a> {
    val: ValueHandle<'a, T>,
    itp: &'a Interpreter
}

impl<'a, T: CtrValue> ContextValue<'a, T> {
    pub fn new(val: ValueHandle<'a, T>, itp: &'a Interpreter) -> ContextValue<'a, T> {
        ContextValue::<'a, T> {
            val: val,
            itp: itp
        }
    }
}

impl<'a, T: CtrValue> Display for ContextValue<'a, T> {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), fmt::Error> {
        let ref v = self.val;
        if v.pointy() {
            match v.alloc_len() {
                0 | 2 => {
                    try!(write!(fmt, "("));
                    write_list(fmt, self, true)
                },
                _ => unimplemented!()
            }
        } else {
            let obv: Option<&Bits<isize>> = v.downcast(&self.itp);
            if let Some(bv) = obv {
                write!(fmt, "{}", bv.data)
            } else {
                unimplemented!()
            }
        }
    }
}

fn write_list<T: CtrValue>(fmt: &mut Formatter, ls: &ContextValue<T>, start: bool)
    -> Result<(), fmt::Error> {
    let ref v = ls.val;
    if v.pointy() {
        match v.alloc_len() {
            0 => write!(fmt, ")"),
            2 => {
                let olv: Option<&ListPair> = v.downcast(&ls.itp);
                if let Some(lv) = olv {
                    let head: Root<Any> = unsafe { Root::new(lv.head) };
                    let tail: Root<Any> = unsafe { Root::new(lv.tail) };
                    if !start {
                        try!(write!(fmt, " "));
                    }
                    try!(ContextValue::new(head.borrow(), ls.itp).fmt(fmt));
                    write_list(fmt, &ContextValue::new(tail.borrow(), ls.itp),
                               false)
               } else {
                   panic!()
               }
            },
            _ => panic!()
        }
    } else {
        panic!()
    }
}
