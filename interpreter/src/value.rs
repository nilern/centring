use refs::{ValuePtr, Root, ValueHandle};
use interpreter::{Interpreter, CtrError};

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

// General

unsafe fn get_indexed<T: CtrValue>(rec: &T, i: usize) -> Option<Root<Any>> {
    let rec_ptr: *mut T = mem::transmute(rec);
    if i < (*(rec_ptr as *mut Any)).alloc_len() {
        let fields = rec_ptr.offset(1) as *mut ValuePtr;
        Some(Root::new(*fields.offset(i as isize)))
    } else {
        None
    }
}

unsafe fn set_indexed<T: CtrValue>(rec: &T, i: usize, v: ValueHandle<Any>) -> Result<(), CtrError> {
    let rec_ptr: *mut T = mem::transmute(rec);
    let len = (*(rec_ptr as *mut Any)).alloc_len();
    if i < len {
        let fields = rec_ptr.offset(1) as *mut ValuePtr;
        *fields.offset(i as isize) = v.ptr();
        Ok(())
    } else {
        Err(CtrError::Index(i, len))
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

/// A void pointer.                                                                                 
pub type VoidPtr = Bits<*mut ()>;                                                                   
                                                                                                    
impl_typ! { VoidPtr, voidptr_t }  

// Symbol *******************************************************************

#[repr(C)]
pub struct Symbol {
    header: usize,
    typ: ValuePtr,
}

impl CtrValue for Symbol {}

impl_typ! { Symbol, symbol_t }

// String **********************************************************************

#[repr(C)]
pub struct String {
    header: usize,
    typ: ValuePtr
}

impl CtrValue for String {}

impl_typ! { String, string_t }

impl String {
    fn new(itp: &mut Interpreter, chars: &str) -> Root<String> {
        let typ = itp.string_t.clone();
        itp.alloc_bytes(typ.borrow(), chars.as_bytes())
    }
}

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
    
    pub fn iter<'a>(&self, itp: &'a Interpreter) -> ListIter<'a> {
        ListIter {
            list: unsafe { Root::new(mem::transmute::<&ListPair, *mut Any>(self)) },
            itp: itp
        }
    }
}

pub struct ListIter<'a> {
    list: Root<Any>,
    itp: &'a Interpreter
}

impl<'a> Iterator for ListIter<'a> {
    type Item = Root<Any>;
    
    fn next(&mut self) -> Option<Root<Any>> {
        let ls = self.list.clone();
        if let Some(pair) = ls.borrow().downcast::<ListPair>(self.itp) {
            self.list = pair.rest();
            Some(pair.first())
        } else if ls.borrow().instanceof(ListEmpty::typ(self.itp)) {
            None
        } else {
            panic!()
        }
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

// ArrayMut ***********************************************************************

/// A mutable array
#[repr(C)]
pub struct ArrayMut {
    header: usize,
    typ: ValuePtr,
}

impl CtrValue for ArrayMut {}

impl_typ! { ArrayMut, array_mut_t }

impl ArrayMut {
    pub fn get(&self, i: usize) -> Option<Root<Any>> {
        unsafe { get_indexed(self, i) }
    }

    pub fn set(&self, i: usize, v: ValueHandle<Any>) -> Result<(), CtrError> {
        unsafe { set_indexed(self, i, v) }
    }
}

// Type *********************************************************************

/// A type.
#[repr(C)]
pub struct Type {
    header: usize,
    typ: ValuePtr,
}

impl CtrValue for Type {}

impl_typ! { Type, type_t }

// Expr *******************************************************************                         
                                                                                                
/// An AST node for expressions (such as `(%eq? a b)`).                                             
#[repr(C)]                                                                                          
pub struct Expr {                                                                                   
    header: usize,                                                                                  
    typ: ValuePtr,                                                                                  
    op: ValuePtr
}

impl CtrValue for Expr {}

impl_typ! { Expr, expr_t }

impl Expr {
    pub fn op(&self, itp: &Interpreter) -> fn(&mut Interpreter, &[Root<Any>]) -> Root<Any> {
        let op = unsafe { Root::<Any>::new(self.op) };
        if let Some(f) = op.borrow().downcast::<VoidPtr>(itp) {
            unsafe { mem::transmute(f.unbox()) }
        } else {
            panic!()
        }
    }
    
    pub fn args(&self, i: usize) -> Option<Root<Any>> {
        unsafe { get_indexed(self, i) }
    }
}

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
        unsafe { get_indexed(self, i) }
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
