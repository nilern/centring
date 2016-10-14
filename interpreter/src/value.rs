use gc::Collector;

use std::slice;
use std::mem;

pub trait ValueRefT: Sized + Copy {
    /// Wrap a raw pointer and initialize the value header with the other
    /// arguments.
    ///
    /// # Safety
    /// `ptr` should point to a sufficient amount of memory. Does not
    /// initialize the contained data so you should do that ASAP or bad things
    /// will happen.
    unsafe fn from_raw_parts(ptr: *mut usize,
        size: usize, is_rec: bool, marked: bool) -> Self;

    /// Wrap a raw pointer.
    ///
    /// # Safety
    /// Subsequent operations may do arbitrary things if `ptr` doesn't point to
    /// a valid Value.
    unsafe fn from_raw(ptr: *mut usize) -> Self;

    /// Get the underlying raw pointer for reading.
    fn as_ptr(self) -> *const usize;

    /// Get the underlying raw pointer for reading and writing.
    fn as_mut_ptr(self) -> *mut usize;

    /// Returns `true` if the mark bit is on, else `false`
    #[inline(always)]
    fn marked(&self) -> bool {
        unsafe { *self.as_ptr() & 1 == 1 }
    }

    /// Set the mark bit on.
    #[inline(always)]
    fn set_mark_bit(self) {
        unsafe { *self.as_mut_ptr() = *self.as_ptr() | 1; }
    }

    /// Turn the mark bit off.
    #[inline(always)]
    fn unset_mark_bit(self) {
        unsafe { *self.as_mut_ptr() = *self.as_ptr() & !1; }
    }

    /// Does the Value contain any pointers beside the type pointer?
    #[inline(always)]
    fn is_rec(self) -> bool {
        unsafe { *self.as_ptr() >> 1 & 1 == 1 }
    }

    /// Get the reference to the type value.
    #[inline(always)]
    fn get_type(self) -> ValueRef {
        unsafe { ValueRef(*self.as_ptr().offset(1) as *mut usize) }
    }

    /// Set the reference to the type value.
    #[inline(always)]
    fn set_type(self, typeref: ValueRef) {
        unsafe { *self.as_mut_ptr().offset(1) = typeref.as_ptr() as usize; }
    }
}

pub trait FlatValueRef {
    fn size_of(self) -> usize;
}

pub trait Unbox {
    /// Unbox the data behind this Value reference type.
    ///
    /// # Panics
    /// Will `panic!()` if the sizes of T and the Value data differ.
    fn unbox<T: Copy>(self) -> T;
}

#[derive(Clone, Copy)]
pub struct ValueRef(*mut usize);

impl ValueRef {
    #[inline(always)]
    pub fn field_count(self) -> usize {
        unsafe { *self.as_ptr() >> 2 }
    }

    #[inline(always)]
    fn fwd(self) -> ValueRef {
        self.get_type()
    }

    #[inline(always)]
    fn set_fwd(self, fwd: ValueRef) {
        self.set_type(fwd);
    }

    /// Mark the value that this `ValueRef` points to. Return a new `ValueRef`
    /// that points to the new location of the value.
    ///
    /// # Safety
    /// This may move the value so calling code should overwrite the value this
    /// was called on with the returned value.
    pub unsafe fn mark(self, ctx: &mut Collector) -> ValueRef {
        if self.marked() {
            if self.is_rec() {
                return self.fwd();
            } else {
                return self;
            }
        } else {
            if self.is_rec() {
                // we have to do the copy first so that only the fromspace
                // version becomes a broken heart:
                let res = unsafe {
                    ctx.move_rec_slice( // move data
                        slice::from_raw_parts(self.as_ptr(),
                        2 + self.field_count()))
                };
                self.set_fwd(res);
                self.set_mark_bit();
                return res;
            } else {
                // on the other hand here we need to set the bit first to avoid
                // infinite recursive loops:
                self.set_mark_bit();
                self.set_type(self.get_type().mark(ctx));
                return self;
            }
        }
    }
}

impl ValueRefT for ValueRef {
    #[inline(always)]
    unsafe fn from_raw_parts(ptr: *mut usize, size: usize, is_rec: bool,
                                 marked: bool) -> ValueRef {
        *ptr = size << 2 | (is_rec as usize) << 1 | marked as usize;
        ValueRef(ptr)
    }

    #[inline(always)]
    unsafe fn from_raw(ptr: *mut usize) -> ValueRef { ValueRef(ptr) }

    #[inline(always)]
    fn as_ptr(self) -> *const usize { self.0 as *const usize }

    #[inline(always)]
    fn as_mut_ptr(self) -> *mut usize { self.0 }
}

#[derive(Clone, Copy)]
pub struct BitsRef(*mut usize);

impl BitsRef {
    #[inline(always)]
    fn data(self) -> *mut usize {
        unsafe { self.as_mut_ptr().offset(2) }
    }
}

impl ValueRefT for BitsRef {
    #[inline(always)]
    unsafe fn from_raw_parts(ptr: *mut usize, size: usize, is_rec: bool,
                                 marked: bool) -> BitsRef {
        assert!(!is_rec);
        *ptr = size << 2 | (is_rec as usize) << 1 | marked as usize;
        BitsRef(ptr)
    }

    #[inline(always)]
    unsafe fn from_raw(ptr: *mut usize) -> BitsRef { BitsRef(ptr) }

    #[inline(always)]
    fn as_ptr(self) -> *const usize { self.0 as *const usize }

    #[inline(always)]
    fn as_mut_ptr(self) -> *mut usize { self.0 }
}

impl FlatValueRef for BitsRef {
    fn size_of(self) -> usize {
        unsafe { *self.as_ptr() >> 2 }
    }
}

impl Unbox for BitsRef {
    fn unbox<T: Copy>(self) -> T {
        assert_eq!(self.size_of(), mem::size_of::<T>());
        unsafe { *(self.data() as *const T) }
    }
}
