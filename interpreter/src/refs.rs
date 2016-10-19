use gc::Collector;
use value::Any;

use std::rc::{Rc, Weak};
use std::cell::RefCell;
use std::ops::{Deref, DerefMut};

pub type ValuePtr = *mut Any;

/// An owned reference to a cell that holds a `ValuePtr`. This indirection is
/// necessary so that `Interpreter::collect()` can move the underlying value
/// and update the pointer contained by this and thus we can keep dereferencing
/// these after collections.
pub struct Root(Rc<RefCell<ValuePtr>>);

/// Like `Root`, but weak (as in `std::(a)rc::Weak`). Used by the `Interpeter`
/// to track live roots on the Rust stack.
pub struct WeakRoot(Weak<RefCell<ValuePtr>>);

/// Like `Root`, but a non-owned reference.
pub struct ValueHandle<'a>(&'a RefCell<ValuePtr>);

impl Root {
    /// Create a new `Root`.
    pub fn new(v: ValuePtr) -> Root {
        Root(Rc::new(RefCell::new(v)))
    }

    /// Create a corresponding `WeakRoot`.
    pub fn downgrade(this: &Root) -> WeakRoot {
        WeakRoot(Rc::downgrade(&this.0))
    }

    /// Create a corresponding `ValueHandle`.
    pub fn borrow(&self) -> ValueHandle {
        ValueHandle(&*self.0)
    }

    /// Get the raw pointer to the Value.
    pub fn ptr(&self) -> ValuePtr {
        *self.0.deref().borrow()
    }

    /// Mark/move the underlying value and update the internal cell with the
    /// new location.
    pub fn mark(&mut self, gc: &mut Collector) {
        let vref = unsafe { gc.mark(self.ptr()) };
        *self.0.deref().borrow_mut() = vref;
    }
}

impl Deref for Root {
    type Target = Any;

    fn deref(&self) -> &Any {
        unsafe { &**self.0.borrow() }
    }
}

impl DerefMut for Root {
    fn deref_mut(&mut self) -> &mut Any {
        unsafe { &mut **self.0.borrow_mut() }
    }
}

impl WeakRoot {
    /// Try to upgrade this to a `Root`. If the result is `Some(_)`, there were
    /// other roots remaining. If it is `None`, there were none (and the
    /// underlying Value does not need to be marked as a root).
    pub fn upgrade(&self) -> Option<Root> {
        self.0.upgrade().map(|rc| Root(rc))
    }
}

impl<'a> ValueHandle<'a> {
    /// Get the raw pointer to the Value.
    pub fn ptr(&self) -> ValuePtr {
        *self.0.borrow()
    }
}

impl<'a> Deref for ValueHandle<'a> {
    type Target = Any;

    fn deref(&self) -> &Any {
        unsafe { &**self.0.borrow() }
    }
}

impl<'a> DerefMut for ValueHandle<'a> {
    fn deref_mut(&mut self) -> &mut Any {
        unsafe { &mut **self.0.borrow_mut() }
    }
}
