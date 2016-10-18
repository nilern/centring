use gc::Collector;
use value::Any;

use std::rc::{Rc, Weak};
use std::cell::RefCell;
use std::slice;
use std::ops::{Deref, DerefMut};

pub type ValuePtr = *mut Any;

pub struct Root(Rc<RefCell<ValuePtr>>);
pub struct WeakRoot(Weak<RefCell<ValuePtr>>);
pub struct ValueHandle<'a>(&'a RefCell<ValuePtr>);

impl Root {
    pub fn new(v: ValuePtr) -> Root {
        Root(Rc::new(RefCell::new(v)))
    }

    pub fn downgrade(this: &Root) -> WeakRoot {
        WeakRoot(Rc::downgrade(&this.0))
    }

    pub fn ptr(&self) -> ValuePtr {
        *self.0.deref().borrow()
    }

    fn set_ptr(&mut self, ptr: ValuePtr) {
        *self.0.deref().borrow_mut() = ptr;
    }

    pub fn mark(&mut self, gc: &mut Collector) {
        let vref = unsafe { gc.mark(self.ptr()) };
        self.set_ptr(vref);
    }
}

impl Deref for Root {
    type Target = Any;

    fn deref(&self) -> &Any {
        unsafe { &**self.0.borrow() }
    }
}

impl WeakRoot {
    pub fn upgrade(&self) -> Option<Root> {
        self.0.upgrade().map(|rc| Root(rc))
    }
}

impl<'a> ValueHandle<'a> {
    pub fn ptr(&self) -> ValuePtr {
        *self.0.borrow()
    }
}
