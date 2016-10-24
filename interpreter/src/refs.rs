use interpreter::Interpreter;
use gc::Collector;
use value::{CtrValue, ConcreteType, Any, Symbol, Type};
use ops::PtrEq;

use std::rc::{Rc, Weak};
use std::cell::RefCell;
use std::ops::{Deref, DerefMut};
use std::marker::PhantomData;
use std::ptr;
use std::slice;

/// A pointer to a Value.
pub type ValuePtr = *mut Any;

/// An owned reference to a cell that holds a `ValuePtr`. This indirection is
/// necessary so that `Interpreter::collect()` can move the underlying value
/// and update the pointer contained by this and thus we can keep dereferencing
/// these after collections.
pub struct Root<T: CtrValue>(Rc<RefCell<ValuePtr>>, PhantomData<T>);

/// Like `Root`, but weak (as in `std::(a)rc::Weak`). Used by the `Interpeter`
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

    pub fn downcast<U: ConcreteType>(&self, itp: &Interpreter) -> Option<ValueHandle<'a, U>> {
        if self.instanceof(U::typ(itp)) {
            Some(ValueHandle(self.0, Default::default()))
        } else {
            None
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

    pub fn clone_bytes(self) -> Option<Vec<u8>> { // HACK
        let self_any = self.as_any_ref();
        if self_any.pointy() {
            None
        } else {
            let len = self_any.alloc_len();
            let bytes = unsafe { slice::from_raw_parts(self_any.ptr().offset(1) as *mut u8, len) };
            let mut res = Vec::with_capacity(len);
            res.extend_from_slice(bytes);
            Some(res)
        }
    }
}

impl<'a> ValueHandle<'a, Symbol> {
    pub fn to_string(self) -> String {
        String::from_utf8(self.clone_bytes().unwrap()).unwrap()
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
