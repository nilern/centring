//! Since the GC can move values, it is unsafe to hold on to &-references or  raw pointers over
//! allocations. That is why we have `Root<T>` and `ValueHandle<'a, T>`.
//!
//! At the moment the type system does not enforce this property, but as a corollary any function
//! or method that allocates must take `ValueHandle`:s or `Root`:s as arguments
//! (including `self`!). Since we need a `&mut Interpreter` for allocation, functions and methods
//! that take one of those are the ones that potentially must take `ValueHandle`:s or `Root`:s.

use interpreter::{CtrResult, CtrError};
use gc::Collector;
use value::{CtrValue, ConcreteType, Any, Type};
use ops::PtrEq;

use std::rc::{Rc, Weak};
use std::cell::RefCell;
use std::ops::{Deref, DerefMut};
use std::marker::PhantomData;
use std::ptr;
use std::fmt;

/// A pointer to a Value.
pub type ValuePtr = *mut Any;

/// An owned reference to a cell that holds a `ValuePtr`. This indirection is
/// necessary so that `Interpreter::collect()` can move the underlying value
/// and update the pointer contained by this and thus we can keep dereferencing
/// these after collections.
pub struct Root<T: CtrValue>(Rc<RefCell<ValuePtr>>, PhantomData<T>);

/// Like `Root`, but weak (as in `std::(a)rc::Weak`). Used by the `Interpreter`
/// to track live roots on the Rust stack.
pub struct WeakRoot(Weak<RefCell<ValuePtr>>);

/// Like `Root`, but a non-owned reference.
pub struct ValueHandle<'a, T: CtrValue + 'a>(&'a RefCell<ValuePtr>, PhantomData<&'a T>);

impl<T: CtrValue> Root<T> {
    /// Create a new `Root`.
    pub unsafe fn new(v: ValuePtr) -> Root<T> {
        Root(Rc::new(RefCell::new(v)), Default::default())
    }

    /// Create a corresponding `WeakRoot`.
    pub fn downgrade(this: &Root<T>) -> WeakRoot {
        WeakRoot(Rc::downgrade(&this.0))
    }

    /// Create a corresponding `ValueHandle`.
    pub fn borrow(&self) -> ValueHandle<T> {
        ValueHandle(&*self.0, Default::default())
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

    pub fn as_any_ref(self) -> Root<Any> {
        Root(self.0, Default::default())
    }

    pub fn downcast<U: ConcreteType>(self) -> CtrResult<U> {
        let typ = U::typ();
        if self.borrow().instanceof(typ.borrow()) {
            Ok(Root(self.0, Default::default()))
        } else {
            Err(CtrError::Type(typ))
        }
    }
}

impl<T: CtrValue> Clone for Root<T> {
    fn clone(&self) -> Root<T> {
        Root(self.0.clone(), self.1.clone())
    }
}

impl<T: CtrValue> PtrEq for Root<T> {
    fn identical(&self, other: &Root<T>) -> bool {
        ptr::eq(self.ptr(), other.ptr())
    }
}

impl<T: CtrValue> Deref for Root<T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { &*(*self.0.borrow() as *mut T) }
    }
}

impl<T: CtrValue> DerefMut for Root<T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { &mut *(*self.0.borrow_mut() as *mut T) }
    }
}

impl<T: CtrValue> fmt::Debug for Root<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(fmt, "#<Root<T>>")
    }
}

impl WeakRoot {
    /// Try to upgrade this to a `Root`. If the result is `Some(_)`, there were
    /// other roots remaining. If it is `None`, there were none (and the
    /// underlying Value does not need to be marked as a root).
    pub fn upgrade<T: CtrValue>(&self) -> Option<Root<T>> {
        self.0.upgrade().map(|rc| Root(rc, Default::default()))
    }
}

impl<'a, T: CtrValue> ValueHandle<'a, T> {
    /// Upcast this to a reference to `Any`.
    pub fn as_any_ref(self) -> ValueHandle<'a, Any> {
        ValueHandle(self.0, Default::default())
    }

    pub fn downcast<U: ConcreteType>(&self) -> Result<ValueHandle<'a, U>, CtrError> {
        let typ = U::typ();
        if self.instanceof(typ.borrow()) {
            Ok(ValueHandle(self.0, Default::default()))
        } else {
            Err(CtrError::Type(typ))
        }
    }

    pub fn root(&self) -> Root<T> {
        unsafe { Root::new(self.ptr()) }
    }

    /// Get the raw pointer to the Value.
    pub fn ptr(self) -> ValuePtr {
        *self.0.borrow()
    }

    /// Dynamic typecheck (`:`).
    pub fn instanceof(self, typ: ValueHandle<Type>) -> bool {
        self.as_any_ref().get_type().borrow().identical(&typ)
    }
}

impl<'a, T: CtrValue> Clone for ValueHandle<'a, T> {
    fn clone(&self) -> ValueHandle<'a, T> {
        ValueHandle(self.0, self.1)
    }
}

impl<'a, T: CtrValue> Copy for ValueHandle<'a, T> {}

impl<'a, T: CtrValue> PtrEq for ValueHandle<'a, T> {
    fn identical(&self, other: &ValueHandle<'a, T>) -> bool {
        ptr::eq(self.ptr(), other.ptr())
    }
}

impl<'a, T: CtrValue> Deref for ValueHandle<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { &*(*self.0.borrow() as *mut T) }
    }
}

impl<'a, T: CtrValue> DerefMut for ValueHandle<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { &mut *(*self.0.borrow_mut() as *mut T) }
    }
}
