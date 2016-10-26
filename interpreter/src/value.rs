use refs::{ValuePtr, Root, ValueHandle};
use interpreter::Interpreter;

use std::mem;

// Traits *******************************************************************

/// A marker trait for Values (structs that are extensions of `Any`).
pub trait CtrValue {}

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

macro_rules! impl_typ {
    { $rs_typ: ident, $itp_field: ident } => {
        impl ConcreteType for $rs_typ {
            fn typ(itp: &Interpreter) -> ValueHandle<Type> {
                itp.$itp_field.borrow()
            }
        }
    }
}

// Any **********************************************************************

/// The layout of every Centring Value on the GC heap starts with the fields
/// of this struct.
#[repr(C)]
pub struct Any {
    pub header: usize,
    pub typ: ValuePtr,
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

impl CtrValue for Any {}

// Bits *********************************************************************

/// Wraps some data that has no inner structure, for example `Int64` wraps
/// an i64.
#[repr(C)]
pub struct Bits<T: Copy> {
    header: usize,
    typ: ValuePtr,
    data: T,
}

impl<T: Copy> CtrValue for Bits<T> {}

impl<T: Copy> Unbox for Bits<T> {
    type Prim = T;

    fn unbox(&self) -> T {
        self.data
    }
}

/// A 'fixnum'.
pub type Int = Bits<isize>;

impl_typ! { Int, int_t }

/// An unsigned 'fixnum'.
pub type UInt = Bits<usize>;

impl_typ! { UInt, uint_t }

// Symbol *******************************************************************

#[repr(C)]
pub struct Symbol {
    header: usize,
    typ: ValuePtr,
}

impl CtrValue for Symbol {}

impl_typ! { Symbol, symbol_t }

// Pair *********************************************************************

/// The good ol' cons cell.
#[repr(C)]
pub struct ListPair {
    header: usize,
    typ: ValuePtr,
    head: ValuePtr,
    tail: ValuePtr,
}

impl CtrValue for ListPair {}

impl_typ! { ListPair, pair_t }

impl ListPair {
    pub fn first(&self) -> Root<Any> {
        unsafe { Root::new(self.head) }
    }
    
    pub fn rest(&self) -> Root<Any> {
        unsafe { Root::new(self.tail) }
    }
}

// Nil **********************************************************************

/// Plain old `'()`
#[repr(C)]
pub struct ListEmpty {
    header: usize,
    typ: ValuePtr,
}

impl CtrValue for ListEmpty {}

impl_typ! { ListEmpty, nil_t }

// Type *********************************************************************

/// A type.
#[repr(C)]
pub struct Type {
    header: usize,
    typ: ValuePtr,
}

impl CtrValue for Type {}

impl_typ! { Type, type_t }

// Do *********************************************************************

/// An AST node for `$do`.
#[repr(C)]
pub struct Do {
    header: usize,
    typ: ValuePtr
}

impl CtrValue for Do {}

impl_typ! { Do, do_t }

impl Do {
    pub fn stmts(&self, i: usize) -> Option<Root<Any>> {
        unsafe {
            let self_ptr: ValuePtr = mem::transmute(self);
            if i < (*self_ptr).alloc_len() {
                let rfields = self_ptr.offset(1) as *mut ValuePtr;
                Some(Root::new(*rfields.offset(i as isize)))
            } else {
                None
            }
        }
    }
}

// Const *********************************************************************

/// An AST node representing a constant.
#[repr(C)]
pub struct Const {
    header: usize,
    typ: ValuePtr,
    val: ValuePtr,
}

impl CtrValue for Const {}

impl_typ! { Const, const_t }

impl Const {
    pub fn val(&self) -> Root<Any> {
        unsafe { Root::new(self.val) }
    }
}

// DoCont *********************************************************************

/// The `$do` continuation.
#[repr(C)]
pub struct DoCont {
    header: usize,
    typ: ValuePtr,
    parent: ValuePtr,
    do_ast: ValuePtr,
    index: ValuePtr
}

impl CtrValue for DoCont {}

impl_typ! { DoCont, docont_t }

impl DoCont {
    pub fn parent(&self) -> Root<Any> {
        unsafe { Root::new(self.parent) }
    }

    pub fn do_ast(&self, itp: &mut Interpreter) -> Option<Root<Do>> {
        unsafe {
            let res = Root::<Do>::new(self.do_ast);
            if res.borrow().instanceof(Do::typ(itp)) {
                Some(res)
            } else {
                None
            }
        }
    }

    pub fn index(&self, itp: &mut Interpreter) -> Option<usize> {
        unsafe {
            let res = Root::<UInt>::new(self.index);
            if res.borrow().instanceof(UInt::typ(itp)) {
                Some(res.unbox())
            } else {
                None
            }
        }
    }
}

// Halt *********************************************************************

/// The halt continuation.
#[repr(C)]
pub struct Halt {
    header: usize,
    typ: ValuePtr,
}

impl CtrValue for Halt {}

impl_typ! { Halt, halt_t }
