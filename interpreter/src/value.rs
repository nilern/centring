use refs::ValuePtr;
use interpreter::Interpreter;

use std::mem;
use std::mem::size_of;

/// The layout of every Centring Value on the GC heap starts with the fields
/// of this struct.
#[repr(C)]
pub struct Any {
    pub header: usize,
    pub typ: ValuePtr
}

/// Wraps some data that has no inner structure, for example `Int64` wraps
/// an i64.
#[repr(C)]
pub struct Bits<T: Copy> {
    header: usize,
    typ: ValuePtr,
    pub data: T
}

/// The good ol' cons cell.
#[repr(C)]
pub struct ListPair {
    header: usize,
    typ: ValuePtr,
    pub head: ValuePtr,
    pub tail: ValuePtr
}

/// Plain old `'()`
#[repr(C)]
pub struct ListEmpty {
    header: usize,
    typ: ValuePtr
}

pub trait CtrValue {
    /// Return a reference to the Any part of a Value.
    fn as_any(&self) -> &Any;
}

// FIXME: impls should take type pointers into account
pub trait Downcast<SubType> {
    fn downcast(&self, itp: &Interpreter) -> Option<&SubType>;
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
        alloc_len << 2 | (pointy as usize) << 1
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
    pub fn get_type(&self) -> ValuePtr {
        self.typ
    }

    /// Set the type reference of this Value.
    pub fn set_type(&mut self, t: ValuePtr) {
        self.typ = t
    }
}

impl CtrValue for Any {
    fn as_any(&self) -> &Any {
        self
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

impl CtrValue for ListEmpty {
    fn as_any(&self) -> &Any {
        unsafe { mem::transmute(self) }
    }
}

impl<T: Copy> Downcast<Bits<T>> for Any {
    fn downcast(&self, itp: &Interpreter) -> Option<&Bits<T>> {
        if !self.pointy() && self.alloc_len() == size_of::<T>() {
            Some(unsafe { mem::transmute(self) })
        } else {
            None
        }
    }
}

impl Downcast<ListPair> for Any {
    fn downcast(&self, itp: &Interpreter) -> Option<&ListPair> {
        if self.pointy() && self.alloc_len() == 2 {
            Some(unsafe { mem::transmute(self) })
        } else {
            None
        }
    }
}

impl Downcast<ListEmpty> for Any {
    fn downcast(&self, itp: &Interpreter) -> Option<&ListEmpty> {
        if self.pointy() && self.alloc_len() == 0 {
            Some(unsafe { mem::transmute(self) })
        } else {
            None
        }
    }
}
