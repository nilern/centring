use gc::Collector;

use std::slice;

pub trait ValueRefT {
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
    fn marked(&self) -> bool;

    /// Set the mark bit on.
    fn set_mark_bit(self);

    /// Turn the mark bit off.
    fn unset_mark_bit(self);

    /// Does the Value contain any pointers beside the type pointer?
    fn is_rec(self) -> bool;

    /// Get the reference to the type value.
    fn get_type(self) -> ValueRef;

    /// Set the reference to the type value.
    fn set_type(self, typeref: ValueRef);
}

#[derive(Clone, Copy)]
pub struct ValueRef(*mut usize);

impl ValueRef {
    #[inline(always)]
    fn data(self) -> *mut usize {
        unsafe { self.0.offset(2) }
    }

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

    /// Get the underlying bits of a bits type instance as a `T`, for example
    /// an `i64`.
    ///
    /// # Safety
    /// Doesn't check that the underlying value actually has enough bits.
    pub unsafe fn unbox<T: Copy>(self) -> T {
        *(self.data() as *const T)
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
    unsafe fn from_raw(ptr: *mut usize) -> ValueRef {
        ValueRef(ptr)
    }

    #[inline(always)]
    fn as_ptr(self) -> *const usize {
        self.0 as *const usize
    }

    #[inline(always)]
    fn as_mut_ptr(self) -> *mut usize {
        self.0
    }

    #[inline(always)]
    fn marked(&self) -> bool {
        unsafe { *self.as_ptr() & 1 == 1 }
    }

    #[inline(always)]
    fn set_mark_bit(self) {
        unsafe { *self.as_mut_ptr() = *self.as_ptr() | 1; }
    }

    #[inline(always)]
    fn unset_mark_bit(self) {
        unsafe { *self.as_mut_ptr() = *self.as_ptr() & !1; }
    }

    #[inline(always)]
    fn is_rec(self) -> bool {
        unsafe { *self.as_ptr() >> 1 & 1 == 1 }
    }

    #[inline(always)]
    fn get_type(self) -> ValueRef {
        unsafe { ValueRef(*self.as_ptr().offset(1) as *mut usize) }
    }

    #[inline(always)]
    fn set_type(self, typeref: ValueRef) {
        unsafe { *self.as_mut_ptr().offset(1) = typeref.0 as usize; }
    }
}
