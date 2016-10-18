use refs::ValueRef;

use std::mem;
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

#[repr(C)]
pub struct ListPair {
    pub header: usize,
    pub typ: ValueRef,
    pub head: ValueRef,
    pub tail: ValueRef
}

pub trait CtrValue {
    fn as_any(&self) -> &Any;
}

pub trait Unbox {
    type Prim;

    fn unbox(&self) -> Self::Prim;
}

impl Any {
    pub unsafe fn header(alloc_len: usize, pointy: bool, marked: bool) -> usize {
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

    pub fn get_type(&self) -> ValueRef {
        self.typ
    }

    pub fn set_type(&mut self, t: ValueRef) {
        self.typ = t
    }
}

impl CtrValue for Any {
    fn as_any(&self) -> &Any {
        self
    }
}

impl<T> Bits<T> {
    pub fn new(v: T) -> Bits<T> {
        Bits::<T> {
            header: unsafe {Any::header(size_of::<T>(), false, false) },
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

impl CtrValue for ListPair {
    fn as_any(&self) -> &Any {
        unsafe { mem::transmute(self) }
    }
}
