use interpreter::Interpreter;
use refs::{Root, ValueHandle};
use value::{Bits, ListPair, Downcast};

use std::fmt;
use std::fmt::{Display, Formatter};

pub struct ContextValue<'a> {
    val: ValueHandle<'a>,
    itp: &'a Interpreter
}

impl<'a> ContextValue<'a> {
    pub fn new(val: ValueHandle<'a>, itp: &'a Interpreter) -> ContextValue<'a> {
        ContextValue::<'a> {
            val: val,
            itp: itp
        }
    }
}

impl<'a> Display for ContextValue<'a> {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), fmt::Error> {
        let ref v = self.val;
        if v.pointy() {
            match v.alloc_len() {
                0 => write!(fmt, "()"),
                2 => {
                    let olv: Option<&ListPair> = v.downcast();
                    if let Some(lv) = olv {
                        let head = Root::new(lv.head);
                        let tail = Root::new(lv.tail);
                        write!(fmt, "({} . {})",
                               ContextValue {
                                   val: head.borrow(),
                                   itp: self.itp
                               },
                               ContextValue {
                                   val: tail.borrow(),
                                   itp: self.itp
                               })
                   } else {
                       unimplemented!()
                   }
                },
                _ => unimplemented!()
            }
        } else {
            let obv: Option<&Bits<isize>> = v.downcast();
            if let Some(bv) = obv {
                write!(fmt, "{}", bv.data)
            } else {
                unimplemented!()
            }
        }
    }
}
