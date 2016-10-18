use refs::ValueRef;

use std::mem;
use std::ptr;
use std::mem::size_of;

/// The layout of every Centring Value on the GC heap starts with the fields
/// of this struct.
#[repr(C)]
pub struct Any {
    pub header: usize,
    pub typ: ValueRef
}

/// Wraps some data that has no inner structure, for example `Int64` wraps
/// an i64.
#[repr(C)]
pub struct Bits<T: Copy> {
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
    /// Return a reference to the Any part of a Value.
    fn as_any(&self) -> &Any;
}

/// A trait for getting the raw data out of 'boxes' like `Int` etc.
pub trait Unbox {
    type Prim: Copy;

    /// Get a copy of the wrapped value.
    fn unbox(&self) -> Self::Prim;
}

impl Any {
    /// Construct a Value header for a Value that has a payload of `alloc_len`
    /// words if `pointy` or `alloc_len` bytes otherwise. The header will have
    /// the mark bit turned off.
    ///
    /// # Safety
    /// Everything will go bad if you give incorrect inputs to this and
    /// actually use the header on a GC-managed Value.
    pub unsafe fn header(alloc_len: usize, pointy: bool) -> usize {
        alloc_len << 2
        | (pointy as usize) << 1
    }

    /// Get the size of the payload in words if `self.pointy()` and in bytes
    /// otherwise.
    pub fn alloc_len(&self) -> usize {
        self.header >> 2
    }

    /// Does this Value contain references to other values (besides the type)?
    pub fn pointy(&self) -> bool {
        self.header >> 1 & 1 == 1
    }

    /// Is the mark bit of this Value on?
    pub fn marked(&self) -> bool {
        self.header & 1 == 1
    }

    /// Set the mark bit on.
    pub fn set_mark_bit(&mut self) {
        self.header = self.header | 1;
    }

    /// Turn the mark bit off.
    pub fn unset_mark_bit(&mut self) {
        self.header = self.header & !1;
    }

    /// Get the type reference of this Value.
    pub fn get_type(&self) -> ValueRef {
        self.typ
    }

    /// Set the type reference of this Value.
    pub fn set_type(&mut self, t: ValueRef) {
        self.typ = t
    }
}

impl CtrValue for Any {
    fn as_any(&self) -> &Any {
        self
    }
}

impl<T: Copy> Bits<T> {
    /// Construct a new `Bits<T>`.
    pub fn new(v: T) -> Bits<T> {
        Bits::<T> {
            header: unsafe {Any::header(size_of::<T>(), false) },
            typ: ValueRef::from_raw(ptr::null::<Any>() as *mut Any), // FIXME
            data: v
        }
    }
}

impl<T: Copy> CtrValue for Bits<T> {
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
