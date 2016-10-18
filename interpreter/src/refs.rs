use gc::Collector;
use value::Any;

use std::rc::{Rc, Weak};
use std::cell::RefCell;
use std::slice;
use std::ops::{Deref, DerefMut};

#[derive(Clone, Copy)]
pub struct ValueRef(*mut Any);

pub struct Root(Rc<RefCell<ValueRef>>);
pub struct WeakRoot(Weak<RefCell<ValueRef>>);
pub struct ValueHandle<'a>(&'a RefCell<ValueRef>);

impl Root {
    pub fn new(v: ValueRef) -> Root {
        Root(Rc::new(RefCell::new(v)))
    }

    pub fn downgrade(this: &Root) -> WeakRoot {
        WeakRoot(Rc::downgrade(&this.0))
    }
}

impl Deref for Root {
    type Target = RefCell<ValueRef>;

    fn deref(&self) -> &RefCell<ValueRef> {
        self.0.deref()
    }
}

impl WeakRoot {
    pub fn upgrade(&self) -> Option<Root> {
        self.0.upgrade().map(|rc| Root(rc))
    }
}

impl ValueRef {
    #[inline(always)]
    pub fn from_raw(ptr: *mut Any) -> ValueRef {
        ValueRef(ptr)
    }

    #[inline(always)]
    pub fn as_ptr(self) -> *const Any {
        self.0 as *const Any
    }

    #[inline(always)]
    pub fn as_mut_ptr(self) -> *mut Any {
        self.0
    }

    /// Mark the value that this `ValueRef` points to. Return a new `ValueRef`
    /// that points to the new location of the value.
    ///
    /// # Safety
    /// This may move the value so calling code should overwrite the value this
    /// was called on with the returned value.
    pub unsafe fn mark(mut self, gc: &mut Collector) -> ValueRef {
        if self.marked() {
            if self.pointy() {
                return self.typ // get forward pointer
            } else {
                return self;
            }
        } else {
            if self.pointy() {
                // we have to do the copy first so that only the fromspace
                // version becomes a broken heart:
                let res =
                    gc.move_rec_slice( // move data
                        slice::from_raw_parts(self.as_mut_ptr() as *mut ValueRef,
                            2 + self.alloc_len()));
                self.typ = res; // set forward pointer
                self.set_mark_bit();
                return res;
            } else {
                // on the other hand here we need to set the bit first to avoid
                // infinite recursive loops:
                self.set_mark_bit();
                self.typ = self.typ.mark(gc);
                return self;
            }
        }
    }
}

impl Deref for ValueRef {
    type Target = Any;

    #[inline(always)]
    fn deref(&self) -> &Any {
        unsafe { &*self.0 }
    }
}

impl DerefMut for ValueRef {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Any {
        unsafe { &mut *self.0 }
    }
}
