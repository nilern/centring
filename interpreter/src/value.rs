use gc::Collector;

use std::slice;
use std::ops::{Deref, DerefMut};
use std::mem;
use std::fmt::Debug;
use std::ptr;
use std::mem::size_of;

#[repr(C)]
pub struct Any {
    pub header: usize,
    pub typ: ValueRef
}

#[repr(C)]
pub struct Bits<T> {
    pub header: usize,
    pub typ: ValueRef,
    pub data: T
}

#[derive(Clone, Copy)]
pub struct ValueRef(*mut Any);

pub trait CtrValue {
    fn as_any(&self) -> &Any;
}

pub trait Unbox {
    type Prim;

    fn unbox(&self) -> Self::Prim;
}

impl Any {
    fn header(alloc_len: usize, pointy: bool, marked: bool) -> usize {
        alloc_len << 2
        | (pointy as usize) << 1
        | marked as usize
    }

    pub fn alloc_len(&self) -> usize {
        self.header >> 2
    }

    pub fn pointy(&self) -> bool {
        self.header >> 1 & 1 == 1
    }

    pub fn marked(&self) -> bool {
        self.header & 1 == 1
    }

    /// Set the mark bit on.
    #[inline(always)]
    pub fn set_mark_bit(&mut self) {
        self.header = self.header | 1;
    }

    /// Turn the mark bit off.
    #[inline(always)]
    pub fn unset_mark_bit(&mut self) {
        self.header = self.header & !1;
    }
}

impl CtrValue for Any {
    fn as_any(&self) -> &Any {
        self
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
                let res = unsafe {
                    gc.move_rec_slice( // move data
                        slice::from_raw_parts(self.as_mut_ptr() as *mut ValueRef,
                            2 + self.alloc_len()))
                };
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

impl<T> Bits<T> {
    pub fn new(v: T) -> Bits<T> {
        Bits::<T> {
            header: Any::header(size_of::<T>(), false, false),
            typ: ValueRef::from_raw(ptr::null::<Any>() as *mut Any), // FIXME
            data: v
        }
    }
}

impl<T> CtrValue for Bits<T> {
    fn as_any(&self) -> &Any {
        unsafe { mem::transmute(self) }
    }
}

impl<T: Copy> Unbox for Bits<T> {
    type Prim = T;

    fn unbox(&self) -> T {
        self.data
    }
}
