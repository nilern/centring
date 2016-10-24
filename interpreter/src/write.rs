use interpreter::Interpreter;
use refs::{Root, ValueHandle};
use value::{CtrValue, Any, Int, Symbol, ListPair};

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
        let ref v = self.val.as_any_ref();
        if v.pointy() {
            match v.alloc_len() {
                0 | 2 => {
                    try!(write!(fmt, "("));
                    write_list(fmt, self, true)
                }
                _ => unimplemented!(),
            }
        } else {
            if let Some(bv) = v.downcast::<Int>(&self.itp) {
                write!(fmt, "{}", bv.data)
            } else if let Some(s) = v.downcast::<Symbol>(&self.itp) {
                // HACK:
                write!(fmt, "{}", String::from_utf8_lossy(&s.clone_bytes().unwrap()))
            } else {
                unimplemented!()
            }
        }
    }
}

fn write_list<T: CtrValue>(fmt: &mut Formatter,
                           ls: &ContextValue<T>,
                           start: bool)
                           -> Result<(), fmt::Error> {
    let ref v = ls.val.as_any_ref();
    if v.pointy() {
        match v.alloc_len() {
            0 => write!(fmt, ")"),
            2 => {
                if let Some(lv) = v.downcast::<ListPair>(&ls.itp) {
                    let head: Root<Any> = unsafe { Root::new(lv.head) };
                    let tail: Root<Any> = unsafe { Root::new(lv.tail) };
                    if !start {
                        try!(write!(fmt, " "));
                    }
                    try!(ContextValue::new(head.borrow(), ls.itp).fmt(fmt));
                    write_list(fmt, &ContextValue::new(tail.borrow(), ls.itp), false)
                } else {
                    panic!()
                }
            }
            _ => panic!(),
        }
    } else {
        panic!()
    }
}
