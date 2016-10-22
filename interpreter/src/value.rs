use refs::{ValuePtr, Root, ValueHandle};
use interpreter::Interpreter;

use std::mem;

/* Traits ********************************************************************/

pub trait CtrValue {
    /// Return a reference to the Any part of a Value.
    fn as_any(&self) -> &Any;
}

// FIXME: impls should take type pointers into account
pub trait Downcast<SubType> {
    /// Try to downcast this Value reference to `SubType`. If this is not
    /// possible, return `None`.
    fn downcast(&self, itp: &Interpreter) -> Option<SubType>;
}

/// A trait for getting the raw data out of 'boxes' like `Int` etc.
pub trait Unbox: CtrValue {
    type Prim: Copy;

    /// Get a copy of the wrapped value.
    fn unbox(&self) -> Self::Prim;
}

pub trait ConcreteType: CtrValue {
    /// Get a reference to the corresponding runtime type.
    fn typ(itp: &Interpreter) -> ValueHandle<Type>;
}

/* Any ***********************************************************************/

/// The layout of every Centring Value on the GC heap starts with the fields
/// of this struct.
#[repr(C)]
pub struct Any {
    pub header: usize,
    pub typ: ValuePtr
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
    pub fn get_type(&self) -> Root<Type> {
        unsafe { Root::new(self.typ) }
    }

    /// Set the type reference of this Value.
    pub fn set_type(&mut self, t: ValueHandle<Type>) {
        self.typ = t.ptr()
    }
}

impl CtrValue for Any {
    fn as_any(&self) -> &Any {
        self
    }
}

/* Bits **********************************************************************/

/// Wraps some data that has no inner structure, for example `Int64` wraps
/// an i64.
#[repr(C)]
pub struct Bits<T: Copy> {
    header: usize,
    typ: ValuePtr,
    pub data: T
}

/// A 'fixnum'.
pub type Int = Bits<isize>;

impl<T: Copy> CtrValue for Bits<T> {
    fn as_any(&self) -> &Any {
        unsafe { mem::transmute(self) }
    }
}

impl ConcreteType for Int {
    fn typ(itp: &Interpreter) -> ValueHandle<Type> {
        itp.int_t.borrow()
    }
}

impl<T: Copy> Unbox for Bits<T> {
    type Prim = T;

    fn unbox(&self) -> T {
        self.data
    }
}

/* Pair **********************************************************************/

/// The good ol' cons cell.
#[repr(C)]
pub struct ListPair {
    header: usize,
    typ: ValuePtr,
    pub head: ValuePtr,
    pub tail: ValuePtr
}

impl CtrValue for ListPair {
    fn as_any(&self) -> &Any {
        unsafe { mem::transmute(self) }
    }
}

impl ConcreteType for ListPair {
    fn typ(itp: &Interpreter) -> ValueHandle<Type> {
        itp.pair_t.borrow()
    }
}

/* Nil ***********************************************************************/

/// Plain old `'()`
#[repr(C)]
pub struct ListEmpty {
    header: usize,
    typ: ValuePtr
}

impl CtrValue for ListEmpty {
    fn as_any(&self) -> &Any {
        unsafe { mem::transmute(self) }
    }
}

impl ConcreteType for ListEmpty {
    fn typ(itp: &Interpreter) -> ValueHandle<Type> {
        itp.nil_t.borrow()
    }
}

/* Type **********************************************************************/

/// A type.
#[repr(C)]
pub struct Type {
    header: usize,
    typ: ValuePtr
}

impl CtrValue for Type {
    fn as_any(&self) -> &Any {
        unsafe { mem::transmute(self) }
    }
}

impl ConcreteType for Type {
    fn typ(itp: &Interpreter) -> ValueHandle<Type> {
        itp.type_t.borrow()
    }
}

/* Const **********************************************************************/

/// An AST node representing a constant.
#[repr(C)]
pub struct Const {
    header: usize,
    typ: ValuePtr,
    pub val: ValuePtr
}

impl CtrValue for Const {
    fn as_any(&self) -> &Any {
        unsafe { mem::transmute(self) }
    }
}

impl ConcreteType for Const {
    fn typ(itp: &Interpreter) -> ValueHandle<Type> {
        itp.const_t.borrow()
    }
}

/* Halt **********************************************************************/

/// The halt continuation.
#[repr(C)]
pub struct Halt {
    header: usize,
    typ: ValuePtr
}

impl CtrValue for Halt {
    fn as_any(&self) -> &Any {
        unsafe { mem::transmute(self) }
    }
}

impl ConcreteType for Halt {
    fn typ(itp: &Interpreter) -> ValueHandle<Type> {
        itp.halt_t.borrow()
    }
}
