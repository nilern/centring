use refs::{ValuePtr, Root, ValueHandle};
use interpreter::{Interpreter, CtrError, CtrResult};
use primops::{ExprFn, StmtFn, CtrlFn};

use std::iter;
use std::slice;
use std::string;
use std::mem;
use std::hash::{Hash, Hasher};
use std::collections::hash_map::DefaultHasher;

// Traits *****************************************************************************************

/// A marker trait for Values (structs that are extensions of `Any`).
pub trait CtrValue: Sized {
    fn as_any(&self) -> &Any {
        unsafe { mem::transmute(self) }
    }
}

pub trait UnsizedCtrValue: CtrValue {
    type Item;
    type Storage;

    fn flex_len(&self) -> usize {
        let self_any = self.as_any();
        let nonflex_size = mem::size_of::<Self>() - mem::size_of::<Any>();
        if self_any.pointy() {
            self_any.alloc_len() - nonflex_size / mem::size_of::<ValuePtr>()
        } else {
            (self_any.alloc_len() - nonflex_size) / mem::size_of::<Self::Storage>()
        }
    }

    fn flex_get(&self, i: usize) -> Option<Self::Item>;

    fn flex_iter(&self) -> IndexedFields<Self> {
        IndexedFields {
            rec: unsafe { Root::new(mem::transmute::<&Self, ValuePtr>(self)) },
            index: 0,
            len: self.flex_len()
        }
    }
}

pub trait UnsizedCtrValueMut: UnsizedCtrValue {
    fn flex_set(&self, i: usize, v: Self::Item) -> Result<(), CtrError>;
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

// Macros *****************************************************************************************

macro_rules! impl_typ {
    { $rs_typ: ident, $itp_field: ident } => {
        impl ConcreteType for $rs_typ {
            fn typ(itp: &Interpreter) -> ValueHandle<Type> {
                itp.$itp_field.borrow()
            }
        }
    }
}

macro_rules! ctr_struct {
    { struct $name:ident () = $itp_field:ident { $($field_name:ident : $field_type:ty),* } } => {
        ctr_struct!{
            struct $name = $itp_field {
                $($field_name : $field_type),*
            }
        }

        impl $name {
            constructor!{ ($($field_name: $field_type),*) -> $name = $itp_field; rec }
        }
    };

    { struct $name:ident = $itp_field:ident { $($field_name:ident : $field_type:ty),* } } => {
        #[repr(C)]
        pub struct $name {
            header: usize,
            typ: ValuePtr,
            $($field_name: ValuePtr),*
        }

        impl CtrValue for $name {}

        impl_typ! { $name, $itp_field }
    }
}

macro_rules! constructor {
    { () -> $t:ty = $itp_field:ident; rec } => {
        pub fn new(itp: &mut Interpreter,) -> Root<$t> {
            let typ = itp.$itp_field.clone();
            itp.alloc_rec(typ.borrow(), iter::empty())
        }
    };

    { ( $($field_name:ident : $field_type:ty),* ) -> $t:ty = $itp_field:ident; rec } => {
        pub fn new(itp: &mut Interpreter, $($field_name: Root<$field_type>),*) -> Root<$t> {
            let typ = itp.$itp_field.clone();
            let fields = [$($field_name.as_any_ref()),*];
            itp.alloc_rec(typ.borrow(), fields.into_iter().cloned())
        }
    }
}

macro_rules! getter {
    { $field:ident : Any } => {
        pub fn $field(&self) -> Root<Any> {
            unsafe { Root::new(self.$field) }
        }
    };

    { $field:ident : $t:ty } => {
        pub fn $field(&self, itp: &Interpreter) -> CtrResult<$t> {
            let res = unsafe { Root::<Any>::new(self.$field) };
            res.downcast::<$t>(itp)
        }
    };

    { $field:ident : $t:ty ; unbox } => {
        pub fn $field(&self, itp: &Interpreter) -> Result<$t, CtrError> {
            let res = unsafe { Root::<Any>::new(self.$field) };
            res.downcast::<Bits<$t>>(itp).map(|res| res.unbox())
        }
    };
}

macro_rules! impl_unboxed_flex_get {
    {} => {
        fn flex_get(&self, i: usize) -> Option<Self::Item> {
            unsafe {
                let ptr: *mut Self = mem::transmute(self);
                if i < self.flex_len() {
                    let fields = ptr.offset(1) as *mut Self::Storage;
                    Some(*fields.offset(i as isize))
                } else {
                    None
                }
            }
        }
    }
}

macro_rules! impl_boxed_flex_get {
    {} => {
        fn flex_get(&self, i: usize) -> Option<Root<Any>> {
            unsafe {
                let ptr: *mut Self = mem::transmute(self);
                if i < self.flex_len() {
                    let fields = ptr.offset(1) as *mut Self::Storage;
                    Some(Root::new(*fields.offset(i as isize)))
                } else {
                    None
                }
            }
        }
    }
}

macro_rules! typecase {
    // 'else' case:
    ( $e:expr, $itp:expr; { _ => $body:block } ) => {
        $body
    };

    // don't need a casted value so `instanceof` suffices:
    ( $e:expr, $itp:expr; { $typ:ty => $body:block , $($rest:tt)* } ) => {
        if $e.instanceof(<$typ as ConcreteType>::typ($itp)) {
            $body
        } else {
            typecase!($e, $itp; {
                $($rest)*
            })
        }
    };

    // the normal case (try to cast):
    ( $e:expr, $itp:expr; { $alias:ident : $typ:ty => $body:block , $($rest:tt)* } ) => {
        if let Ok($alias) = $e.downcast::<$typ>($itp) {
            $body
        } else {
            typecase!($e, $itp; {
                $($rest)*
            })
        }
    };

    // logic-duplicating versions:
    ( $e:expr, $itp:expr; { $($typ:ty)|+ => $body:block , $($rest:tt)* } ) => {
        typecase!($e, $itp; {
            $($typ => $body),+,
            $($rest)*
        })
    };
    ( $e:expr, $itp:expr; { $alias:ident : $($typ:ty)|+ => $body:block , $($rest:tt)* } ) => {
        typecase!($e, $itp; {
            $($alias: $typ => $body),+,
            $($rest)*
        })
    };
}

// IndexedFields **********************************************************************************

pub struct IndexedFields<T: UnsizedCtrValue> {
    rec: Root<T>,
    index: usize,
    len: usize
}

impl<T: UnsizedCtrValue> Iterator for IndexedFields<T> {
    type Item = T::Item;

    fn next(&mut self) -> Option<T::Item> {
        if self.index < self.len {
            let res = self.rec.flex_get(self.index);
            self.index += 1;
            res
        } else {
            None
        }
    }
}

impl<T: UnsizedCtrValue> ExactSizeIterator for IndexedFields<T> {
    fn len(&self) -> usize {
        self.len
    }
}

// Any ********************************************************************************************

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

// Bits *******************************************************************************************

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

impl Int {
    pub fn new(itp: &mut Interpreter, n: isize) -> Root<Int> {
        let typ = itp.int_t.clone();
        itp.alloc_bits(typ.borrow(), n)
    }
}

/// An unsigned 'fixnum'.
pub type UInt = Bits<usize>;

impl_typ! { UInt, uint_t }

impl UInt {
    pub fn new(itp: &mut Interpreter, n: usize) -> Root<UInt> {
        let typ = itp.uint_t.clone();
        itp.alloc_bits(typ.borrow(), n)
    }
}

/// An unsigned 'fixnum'.
pub type Bool = Bits<bool>;

impl_typ! { Bool, bool_t }

impl Bool {
    pub fn new(itp: &mut Interpreter, v: bool) -> Root<Bool> {
        let typ = itp.bool_t.clone();
        itp.alloc_bits(typ.borrow(), v)
    }
}

/// A void pointer.
pub type VoidPtr = Bits<*mut ()>;

impl_typ! { VoidPtr, voidptr_t }

impl VoidPtr {
    pub fn new(itp: &mut Interpreter, ptr: *mut ()) -> Root<VoidPtr> {
        let typ = itp.voidptr_t.clone();
        itp.alloc_bits(typ.borrow(), ptr)
    }
}

// Symbol *****************************************************************************************

#[repr(C)]
pub struct Symbol {
    header: usize,
    typ: ValuePtr,
    hash: u64
}

impl CtrValue for Symbol {}

impl_typ! { Symbol, symbol_t }

impl UnsizedCtrValue for Symbol {
    type Item = u8;
    type Storage = u8;

    impl_unboxed_flex_get! { }
}

impl Symbol {
    // TODO: hash-cons
    pub fn new(itp: &mut Interpreter, chars: &str) -> Root<Symbol> {
        let typ = itp.symbol_t.clone();

        // Precompute hash:
        let mut hasher = DefaultHasher::new();
        chars.hash(&mut hasher);
        let hash = hasher.finish();

        // (Awkwardly) join the bytes of the hash and the chars:
        let mut bytes = Vec::new();
        let hash_bytes = unsafe { slice::from_raw_parts(mem::transmute::<&u64, *mut u8>(&hash),
                                                        mem::size_of::<u64>()) };
        bytes.extend_from_slice(hash_bytes);
        bytes.extend_from_slice(chars.as_bytes());

        itp.alloc_bytes(typ.borrow(), &bytes)
    }

    pub fn hash(&self) -> u64 {
        self.hash
    }

    pub fn to_string(&self) -> string::String {
        string::String::from_utf8(self.flex_iter().collect()).unwrap()
    }

    // TODO: remove this when hash-consing is implemented:
    pub fn equal(&self, other: ValueHandle<Symbol>) -> bool {
        // ATM this is just for Env so comparing the hash fields would be redundant
        self.flex_iter().eq(other.flex_iter())
    }
}

// String *****************************************************************************************

ctr_struct!{
    struct String = string_t { }
}

impl String {
    fn new(itp: &mut Interpreter, chars: &str) -> Root<String> {
        let typ = itp.string_t.clone();
        itp.alloc_bytes(typ.borrow(), chars.as_bytes())
    }
}

// Pair *******************************************************************************************

/// The good ol' cons cell.
ctr_struct!{
    struct ListPair() = pair_t {
        first: Any,
        rest: Any
    }
}

impl ListPair {
    getter!{ first: Any }

    getter!{ rest: Any }

    pub fn iter<'a>(&self, itp: &'a Interpreter) -> ListIter<'a> {
        ListIter {
            list: unsafe { Root::new(mem::transmute(self)) },
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
        typecase!(ls.borrow(), self.itp; {
            pair: ListPair => {
                self.list = pair.rest();
                Some(pair.first())
            },
            ListEmpty => { None },
            _ => { panic!() }
        })
    }
}

// Nil ********************************************************************************************

/// Plain old `'()`
ctr_struct!{
    struct ListEmpty() = nil_t { }
}

// ArrayMut ***************************************************************************************

/// A mutable array
ctr_struct!{
    struct ArrayMut = array_mut_t { }
}

impl UnsizedCtrValue for ArrayMut {
    type Item = Root<Any>;
    type Storage = ValuePtr;

    impl_boxed_flex_get! { }
}

impl UnsizedCtrValueMut for ArrayMut {
    fn flex_set(&self, i: usize, v: Self::Item) -> Result<(), CtrError> {
        unsafe {
            let ptr: *mut Self = mem::transmute(self);
            if i < self.flex_len() {
                let fields = ptr.offset(1) as *mut Self::Storage;
                *fields.offset(i as isize) = v.ptr();
                Ok(())
            } else {
                Err(CtrError::Index(i, self.flex_len()))
            }
        }
    }
}

impl ArrayMut {
    pub fn new<I>(itp: &mut Interpreter, elems: I) -> Root<ArrayMut>
        where I: Iterator<Item=Root<Any>> + ExactSizeIterator {
        let typ = itp.array_mut_t.clone();
        itp.alloc_rec(typ.borrow(), elems)
    }

    pub fn len(&self) -> usize {
        self.flex_len()
    }

    pub fn get(&self, i: usize) -> Option<Root<Any>> {
        self.flex_get(i)
    }

    pub fn set(&self, i: usize, v: Root<Any>) -> Result<(), CtrError> {
        self.flex_set(i, v)
    }

    pub fn iter(&self) -> IndexedFields<ArrayMut> {
        self.flex_iter()
    }
}

// Type *******************************************************************************************

// TODO: hash-cons
/// A type.
ctr_struct!{
    struct Type() = type_t { }
}

// Env ********************************************************************************************

/// An environment frame.
ctr_struct!{
    struct Env = env_t {
        parent: Env,
        count: UInt,
        buckets: ArrayMut
    }
}

impl Env {
    /// Create a fresh environment frame, optionally prepending it to a pre-existing frame chain.
    pub fn new(itp: &mut Interpreter, parent: Option<Root<Env>>) -> Root<Env> {
        let typ = itp.env_t.clone();
        let fields = [parent.map(Root::as_any_ref)
                      .unwrap_or_else(|| Bool::new(itp, false).as_any_ref()),
                      UInt::new(itp, 0).as_any_ref(),
                      Env::new_bucket_array(itp, 4).as_any_ref()];
        itp.alloc_rec(typ.borrow(), fields.into_iter().cloned())
    }

    fn new_bucket_array(itp: &mut Interpreter, len: usize) -> Root<ArrayMut> {
        let elems: Vec<_> = iter::repeat(Bool::new(itp, false).as_any_ref()).take(len).collect();
        ArrayMut::new(itp, elems.into_iter())
    }

    // TODO: these shouldn't be public
    getter!{ parent: Env }
    getter!{ count: usize; unbox }
    getter!{ buckets: ArrayMut }

    fn set_buckets(&mut self, new_buckets: ValueHandle<ArrayMut>) {
        self.buckets = new_buckets.ptr()
    }

    /// Look up the value of a variable by going up the environment frame stack.
    pub fn lookup(&self, itp: &Interpreter, key: ValueHandle<Symbol>) -> Option<Root<Any>> {
        self.lookup_bucket(itp, key).map(|b| b.value())
    }

    /// Set the value of a (pre-existing) variable by going up the environment frame stack.
    pub fn set(&self, itp: &Interpreter, key: ValueHandle<Symbol>, value: ValueHandle<Any>)
        -> Result<(), CtrError> {
        if let Some(mut b) = self.lookup_bucket(itp, key) {
            Ok(b.set_value(value))
        } else {
            Err(CtrError::SetUnbound(key.root()))
        }
    }

    fn lookup_bucket(&self, itp: &Interpreter, key: ValueHandle<Symbol>)
                     -> Option<Root<EnvBucket>> {
        let mut frame = Ok(self);
        while let Ok(env) = frame {
            let buckets = self.buckets(itp).unwrap();
            let index = key.hash() as usize % buckets.len();
            if let Ok(start_bucket) = buckets.get(index).unwrap().downcast::<EnvBucket>(itp) {
                if let Some(bucket) = start_bucket.lookup(itp, key) {
                    return Some(bucket);
                }
            }
            frame = env.parent(itp).map(|env| unsafe { mem::transmute(env.ptr()) });
        }
        None
    }

    fn load_factor(&self, itp: &Interpreter) -> f64 {
        self.count(itp).unwrap() as f64 / self.buckets(itp).unwrap().len() as f64
    }
}

impl<'a> ValueHandle<'a, Env> {
    /// Initialize (or overwrite) a variable in the topmost environment frame.
    pub fn def(self, itp: &mut Interpreter,
               key: ValueHandle<Symbol>, value: ValueHandle<Any>) {
        // first we need to see whether the variable already exists in this frame:
        let buckets = self.buckets(itp).unwrap();
        let index = key.hash() as usize % buckets.len();
        if let Ok(start_bucket) = buckets.get(index).unwrap().downcast::<EnvBucket>(itp) {
            // there was a bucket chain at the index
            if let Some(mut bucket) = start_bucket.lookup(itp, key) {
                // the chain has a bucket with the key; replace the value
                return bucket.set_value(value);
            }
        }
        // didn't exist, so we create it here:
        self.assoc(itp, key, value);
    }

    fn assoc(self, itp: &mut Interpreter, key: ValueHandle<Symbol>, value: ValueHandle<Any>) {
        if self.load_factor(itp) >= 0.75 {
            self.rehash(itp);
        }
        self._assoc(itp, key, value);
    }

    fn _assoc(self, itp: &mut Interpreter, key: ValueHandle<Symbol>, value: ValueHandle<Any>) {
        let buckets = self.buckets(itp).unwrap();
        let index = key.hash() as usize % buckets.len();
        let old_buckets = buckets.get(index).unwrap().downcast::<EnvBucket>(itp).ok();
        let new_bucket = EnvBucket::new(itp, old_buckets, key.root(), value.root());
        buckets.set(index, new_bucket.as_any_ref()).unwrap();
        self.inc_count(itp);
    }

    fn rehash(mut self, itp: &mut Interpreter) {
        let old_buckets = self.buckets(itp).unwrap();
        self.set_buckets(Env::new_bucket_array(itp, 2 * old_buckets.len()).borrow());
        self.set_count(itp, 0);
        for bucket_list in old_buckets.iter() {
            let mut bucket = bucket_list.downcast::<EnvBucket>(itp);
            while let Ok(b) = bucket {
                let k = b.key(itp).unwrap();
                let v = b.value();
                self._assoc(itp, k.borrow(), v.borrow());
                bucket = b.next(itp).and_then(|bucket| bucket.downcast::<EnvBucket>(itp));
            }
        }
    }

    fn set_count(mut self, itp: &mut Interpreter, count: usize) {
        let count = UInt::new(itp, count);
        self.count = count.ptr();
    }

    fn inc_count(self, itp: &mut Interpreter) {
        let old_count = self.count(itp).unwrap();
        self.set_count(itp, old_count + 1);
    }
}

/// A hash bucket for Env.
ctr_struct!{
    struct EnvBucket = env_bucket_t {
        next: EnvBucket,
        key: Symbol,
        value: Any
    }
}

impl EnvBucket {
    fn new(itp: &mut Interpreter, next: Option<Root<EnvBucket>>, key: Root<Symbol>,
               value: Root<Any>) -> Root<EnvBucket> {
        let typ = itp.env_bucket_t.clone();
        let next = next.map(Root::as_any_ref)
                       .unwrap_or_else(|| Bool::new(itp, false).as_any_ref());
        let fields = [next, key.as_any_ref(), value];
        itp.alloc_rec(typ.borrow(), fields.into_iter().cloned())
    }

    getter!{ next: EnvBucket }

    getter!{ key: Symbol }

    getter!{ value: Any }

    fn set_value(&mut self, value: ValueHandle<Any>) {
        self.value = value.ptr()
    }

    fn lookup(&self, itp: &Interpreter, key: ValueHandle<Symbol>) -> Option<Root<EnvBucket>> {
        let mut bucket = Ok(self);
        while let Ok(b) = bucket {
            // OPTIMIZE: when hash consing gets implemented, turn this into .identical()
            if b.key(itp).unwrap().borrow().equal(key) {
                return Some(unsafe { Root::new(mem::transmute(b)) });
            }
            bucket = b.next(itp).map(|b| unsafe{ mem::transmute(b.ptr()) });
        }
        None
    }
}

// Def ********************************************************************************************

ctr_struct!{
    struct Def() = def_t {
        name: Symbol,
        value: Any
    }
}

impl Def {
    getter!{ name: Symbol }

    getter!{ value: Any }
}

// Expr *******************************************************************************************

/// An AST node for expressions (such as `(%eq? a b)`).
ctr_struct!{
    struct Expr = expr_t {
        op: VoidPtr
    }
}

impl UnsizedCtrValue for Expr {
    type Item = Root<Any>;
    type Storage = ValuePtr;

    impl_boxed_flex_get! { }
}

impl Expr {
    pub fn new<I>(itp: &mut Interpreter, op: ExprFn, args: I) -> Root<Expr>
        where I: Iterator<Item=Root<Any>> + ExactSizeIterator {
        let typ = itp.expr_t.clone();
        let mut fields = vec![VoidPtr::new(itp, op as *mut ()).as_any_ref()];
        fields.extend(args);
        itp.alloc_rec(typ.borrow(), fields.into_iter())
    }

    pub fn op(&self, itp: &Interpreter) -> ExprFn {
        let op = unsafe { Root::<Any>::new(self.op) };
        typecase!(op.borrow(), itp; {
            f: VoidPtr => { unsafe { mem::transmute(f.unbox()) } },
            _ => { panic!() }
        })
    }

    pub fn argc(&self) -> usize {
        self.flex_len()
    }

    pub fn args(&self, i: usize) -> Option<Root<Any>> {
        self.flex_get(i)
    }

    pub fn args_iter(&self) -> IndexedFields<Expr> {
        self.flex_iter()
    }
}

// Stmt *******************************************************************************************

/// An AST node for statements (such as `(%rset! arr 0 3#t)`).
ctr_struct!{
    struct Stmt = stmt_t {
        op: VoidPtr
    }
}

impl UnsizedCtrValue for Stmt {
    type Item = Root<Any>;
    type Storage = ValuePtr;

    impl_boxed_flex_get! { }
}

impl Stmt {
    pub fn new<I>(itp: &mut Interpreter, op: StmtFn, args: I) -> Root<Stmt>
        where I: Iterator<Item=Root<Any>> + ExactSizeIterator {
        let typ = itp.stmt_t.clone();
        let mut fields = vec![VoidPtr::new(itp, op as *mut ()).as_any_ref()];
        fields.extend(args);
        itp.alloc_rec(typ.borrow(), fields.into_iter())
    }

    pub fn op(&self, itp: &Interpreter) -> StmtFn {
        let op = unsafe { Root::<Any>::new(self.op) };
        typecase!(op.borrow(), itp; {
            f: VoidPtr => { unsafe { mem::transmute(f.unbox()) } },
            _ => { panic!() }
        })
    }

    pub fn argc(&self) -> usize {
        self.flex_len()
    }

    pub fn args(&self, i: usize) -> Option<Root<Any>> {
        self.flex_get(i)
    }

    pub fn args_iter(&self) -> IndexedFields<Stmt> {
        self.flex_iter()
    }
}

// Ctrl *******************************************************************************************

/// An AST node for control flow (such as `(%brf foo 3 4)`).
ctr_struct!{
    struct Ctrl = ctrl_t {
        op: VoidPtr,
        determinant: Any
    }
}

impl UnsizedCtrValue for Ctrl {
    type Item = Root<Any>;
    type Storage = ValuePtr;

    impl_boxed_flex_get! { }
}

impl Ctrl {
    pub fn new<I>(itp: &mut Interpreter, op: CtrlFn, determinant: Root<Any>, args: I) -> Root<Ctrl>
        where I: Iterator<Item=Root<Any>> + ExactSizeIterator {
        let typ = itp.ctrl_t.clone();
        let mut fields = vec![VoidPtr::new(itp, op as *mut ()).as_any_ref(), determinant];
        fields.extend(args);
        itp.alloc_rec(typ.borrow(), fields.into_iter())
    }

    pub fn op(&self, itp: &Interpreter) -> CtrlFn {
        let op = unsafe { Root::<Any>::new(self.op) };
        typecase!(op.borrow(), itp; {
            f: VoidPtr => { unsafe { mem::transmute(f.unbox()) } },
            _ => { panic!() }
        })
    }

    getter!{ determinant: Any }

    pub fn argc(&self) -> usize {
        self.flex_len()
    }

    pub fn args(&self, i: usize) -> Option<Root<Any>> {
        self.flex_get(i)
    }

    pub fn args_iter(&self) -> IndexedFields<Ctrl> {
        self.flex_iter()
    }
}

// Do *********************************************************************************************

/// An AST node for `$do`.
ctr_struct!{
    struct Do = do_t { }
}

impl UnsizedCtrValue for Do {
    type Item = Root<Any>;
    type Storage = ValuePtr;

    impl_boxed_flex_get! { }
}

impl Do {
    pub fn new(itp: &mut Interpreter, stmts: &[Root<Any>]) -> Root<Do> {
        let typ = itp.do_t.clone();
        itp.alloc_rec(typ.borrow(), stmts.into_iter().cloned())
    }

    pub fn stmts(&self, i: usize) -> Option<Root<Any>> {
        self.flex_get(i)
    }
}

// Var ********************************************************************************************

/// An AST node representing a variable reference.
ctr_struct!{
    struct Var() = var_t {
        name: Symbol
    }
}

impl Var {
    getter!{ name: Symbol }
}

// Const ******************************************************************************************

/// An AST node representing a constant.
ctr_struct!{
    struct Const() = const_t {
        val: Any
    }
}

impl Const {
    getter!{ val: Any }
}

// DoCont *****************************************************************************************

/// The `$do` continuation.
ctr_struct!{
    struct DoCont = docont_t {
        parent: Any,
        do_ast: Do,
        index: UInt,
        env: Env
    }
}

impl DoCont {
    pub fn new(itp: &mut Interpreter, parent: Root<Any>, do_ast: Root<Do>, i: usize,
               env: Root<Env>) -> Root<DoCont> {
        let typ = itp.docont_t.clone();
        let i = UInt::new(itp, i);
        let fields = [parent, do_ast.as_any_ref(), i.as_any_ref(), env.as_any_ref()];
        itp.alloc_rec(typ.borrow(), fields.into_iter().cloned())
    }

    getter!{ parent: Any }

    getter!{ do_ast: Do }

    getter!{ index: usize; unbox }

    getter!{ env: Env }
}

// DefCont ****************************************************************************************

ctr_struct!{
    struct DefCont() = defcont_t {
        parent: Any,
        name: Symbol,
        env: Env
    }
}

impl DefCont {
    getter!{ parent: Any }

    getter!{ name: Symbol }

    getter!{ env: Env }
}

// ExprCont ***************************************************************************************

/// A continuation for `Expr`.
ctr_struct!{
    struct ExprCont = exprcont_t {
        parent: Any,
        ast: Expr,
        index: UInt,
        env: Env
    }
}

impl UnsizedCtrValue for ExprCont {
    type Item = Root<Any>;
    type Storage = ValuePtr;

    impl_boxed_flex_get! { }
}

impl UnsizedCtrValueMut for ExprCont {
    fn flex_set(&self, i: usize, v: Self::Item) -> Result<(), CtrError> {
        unsafe {
            let ptr: *mut Self = mem::transmute(self);
            if i < self.flex_len() {
                let fields = ptr.offset(1) as *mut Self::Storage;
                *fields.offset(i as isize) = v.ptr();
                Ok(())
            } else {
                Err(CtrError::Index(i, self.flex_len()))
            }
        }
    }
}

impl ExprCont {
    pub fn new<I>(itp: &mut Interpreter, parent: Root<Any>, expr_ast: Root<Expr>, index: usize,
                  env: Root<Env>, args: I)
                  -> Root<ExprCont> where I: Iterator<Item=Root<Any>> + ExactSizeIterator {
        let typ = itp.exprcont_t.clone();
        let mut fields = vec![parent, expr_ast.clone().as_any_ref(),
                              UInt::new(itp, index).as_any_ref(), env.as_any_ref()];
        fields.extend(args);
        itp.alloc_rec(typ.borrow(), fields.into_iter())
    }

    getter!{ parent: Any }

    getter!{ ast: Expr }

    getter!{ index: usize; unbox }

    getter!{ env: Env }

    pub fn argc(&self) -> usize {
        self.flex_len()
    }

    pub fn set_arg(&self, i: usize, arg: Root<Any>) -> Result<(), CtrError> {
        self.flex_set(i, arg)
    }

    pub fn args_iter(&self) -> IndexedFields<ExprCont> {
        self.flex_iter()
    }
}

// StmtCont ***************************************************************************************

/// A continuation for `Stmt`.
ctr_struct!{
    struct StmtCont = stmtcont_t {
        parent: Any,
        ast: Stmt,
        index: UInt,
        env: Env
    }
}

impl UnsizedCtrValue for StmtCont {
    type Item = Root<Any>;
    type Storage = ValuePtr;

    impl_boxed_flex_get! { }
}

impl UnsizedCtrValueMut for StmtCont {
    fn flex_set(&self, i: usize, v: Self::Item) -> Result<(), CtrError> {
        unsafe {
            let ptr: *mut Self = mem::transmute(self);
            if i < self.flex_len() {
                let fields = ptr.offset(1) as *mut Self::Storage;
                *fields.offset(i as isize) = v.ptr();
                Ok(())
            } else {
                Err(CtrError::Index(i, self.flex_len()))
            }
        }
    }
}

impl StmtCont {
    pub fn new<I>(itp: &mut Interpreter, parent: Root<Any>, expr_ast: Root<Stmt>, index: usize,
                  env: Root<Env>, args: I)
                  -> Root<StmtCont> where I: Iterator<Item=Root<Any>> + ExactSizeIterator {
        let typ = itp.stmtcont_t.clone();
        let mut fields = vec![parent, expr_ast.clone().as_any_ref(),
                              UInt::new(itp, index).as_any_ref(), env.as_any_ref()];
        fields.extend(args);
        itp.alloc_rec(typ.borrow(), fields.into_iter())
    }

    getter!{ parent: Any }

    getter!{ ast: Stmt }

    getter!{ index: usize; unbox }

    getter!{ env: Env }

    pub fn argc(&self) -> usize {
        self.flex_len()
    }

    pub fn set_arg(&self, i: usize, arg: Root<Any>) -> Result<(), CtrError> {
        self.flex_set(i, arg)
    }

    pub fn args_iter(&self) -> IndexedFields<StmtCont> {
        self.flex_iter()
    }
}

// CtrlCont ***************************************************************************************

/// A continuation for `Ctrl`.
ctr_struct!{
    struct CtrlCont() = ctrlcont_t {
        parent: Any,
        ast: Ctrl,
        env: Env
    }
}

impl CtrlCont {
    getter!{ parent: Any }

    getter!{ ast: Ctrl }

    getter!{ env: Env }
}

// Halt *******************************************************************************************

/// The halt continuation.
ctr_struct!{
    struct Halt() = halt_t { }
}
