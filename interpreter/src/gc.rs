use std::slice;
use std::mem::{size_of, align_of, transmute, swap};
use std::ptr;
use alloc::heap;

// TODO: also consider blobs when polling and triggering collections

#[derive(Clone, Copy)]
pub struct ValueRef(*mut usize);

impl ValueRef {
    #[inline(always)]
    unsafe fn new(ptr: *mut usize, size: usize, is_rec: bool, marked: bool)
        -> ValueRef {
        *ptr = size << 2 | (is_rec as usize) << 1 | marked as usize;
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
    fn data(self) -> *mut usize {
        unsafe { self.0.offset(2) }
    }

    #[inline(always)]
    fn marked(self) -> bool {
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
    fn field_count(self) -> usize {
        unsafe { *self.as_ptr() >> 2 }
    }

    #[inline(always)]
    pub fn typeref(self) -> ValueRef {
        unsafe { ValueRef(*self.as_ptr().offset(1) as *mut usize) }
    }

    #[inline(always)]
    pub fn set_typeref(self, tref: ValueRef) {
        unsafe { *self.as_mut_ptr().offset(1) = tref.0 as usize; }
    }

    #[inline(always)]
    fn fwd(self) -> ValueRef {
        self.typeref()
    }

    #[inline(always)]
    fn set_fwd(self, fwd: ValueRef) {
        self.set_typeref(fwd);
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
                self.set_typeref(self.typeref().mark(ctx));
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

pub struct Collector {
    fromspace: Vec<usize>,
    tospace: Vec<usize>,
    blobspace: *mut usize
}

impl Collector {
    /// Make a new garbage collector to allocate memory and collect garbage with.
    #[inline(always)]
    pub fn new(heapsize: usize) -> Collector {
        Collector {
            fromspace: Vec::with_capacity(heapsize),
            tospace: Vec::with_capacity(heapsize),
            blobspace: ptr::null::<usize>() as *mut usize
        }
    }

    #[inline(always)]
    pub fn rec_poll(&self, word_count: usize) -> bool {
        self.fromspace.capacity() - self.fromspace.len() >= word_count
    }

    /// Allocate an empty blob value that can store a type and `size` bytes of
    /// data. The blob will not be moved or have its data bytes scanned for
    /// pointers by the GC.
    ///
    /// # Safety
    /// This does not initialize the type or data. You must do that before you
    /// do anything else with the allocated Value.
    pub unsafe fn alloc_blob(&mut self, size: usize) -> ValueRef {
        let block: *mut usize = transmute(
            heap::allocate(3 * size_of::<usize>() + size,
                           align_of::<usize>()));
        *block = self.blobspace as usize;
        self.blobspace = block;
        ValueRef::new(block.offset(1), size, false, false)
    }

    /// Allocate an empty record that can store a type and `field_count` other
    /// `ValueRef`:s. The record will be scanned for pointers by the GC and
    /// possibly even moved.
    ///
    /// # Safety
    /// This does not initialize the type or data. You must do that before you
    /// do anything else with the allocated Value.
    pub unsafe fn alloc_rec(&mut self, field_count: usize) -> ValueRef {
        let len = self.fromspace.len();
        let size = 2 + field_count;
        self.fromspace.set_len(len + size);
        let dest = self.fromspace.as_mut_ptr().offset(len as isize);

        ValueRef::new(dest, field_count, true, false)
    }

    fn move_rec_slice(&mut self, src: &[usize]) -> ValueRef {
        unsafe {
            let dest = self.tospace.as_mut_ptr().offset(self.tospace.len() as isize);
            self.tospace.extend_from_slice(src);
            ValueRef(dest)
        }
    }

    /// Allocate an instance of a bits type that can hold the bits of `v` and
    /// initialize its data with a copy of `v`.
    ///
    /// # Safety
    /// This does not initialize the type. You must do that before you do
    /// anything else with the allocated Value.
    pub unsafe fn alloc<T: Clone>(&mut self, v: T) -> ValueRef {
        let vref = self.alloc_blob(size_of::<T>());
        *(vref.as_mut_ptr().offset(2) as *mut T) = v;
        vref
    }

    /// Collect garbage. Assumes that all roots have already been `mark()`:ed.
    ///
    /// # Safety
    /// You must mark all roots and let go of all raw ValueRef:s before calling
    /// this. Otherwise you will get segfaults or at the very least pointers to
    /// garbage data.
    pub unsafe fn collect(&mut self) {
        let mut scan = self.tospace.as_mut_ptr();
        while scan < self.tospace.as_mut_ptr().offset(self.tospace.len() as isize) {
            let size = ValueRef(scan).field_count();
            scan = scan.offset(1);
            *scan = transmute(ValueRef(*scan as *mut usize).mark(self));
            scan = scan.offset(1);
            for _ in 0..size {
                *scan = transmute(ValueRef(*scan as *mut usize).mark(self));
                scan = scan.offset(1);
            }
        }
        self.sweep();
        swap(&mut self.fromspace, &mut self.tospace);
        self.tospace.clear();
        if self.fromspace.len() > 8*self.fromspace.capacity()/10 {
            self.fromspace.reserve(size_of::<usize>());
            self.tospace.reserve(size_of::<usize>());
        }
    }

    fn sweep(&mut self) {
        unsafe {
            let mut blob = &mut self.blobspace as *mut *mut usize;
            while !(*blob).is_null() {
                let val = ValueRef((*blob).offset(1));
                if val.marked() {
                    val.unset_mark_bit();
                    blob = *blob as *mut *mut usize;
                } else {
                    let unreached = *blob;
                    *blob = *unreached as *mut usize;
                    heap::deallocate(unreached as *mut u8,
                                     3 * size_of::<usize>() + val.field_count(),
                                     align_of::<usize>());
                }
            }
        }
    }
}

/// # Tests

#[cfg(test)]
mod tests {
    use super::{Collector, ValueRef};
    use std::mem::{size_of, transmute};
    use std::slice;

    #[test]
    fn it_works() {
        unsafe {
            let mut gc = Collector::new(1024);

            let (tup, a) = alloc(&mut gc);

            print_heaps(&gc);

            println!("\ncollecting...\n");
            tup.mark(&mut gc);
            gc.collect();

            print_heaps(&gc);

            println!("\ncollecting...\n");
            a.mark(&mut gc);
            gc.collect();

            print_heaps(&gc);
        }
    }

    #[test]
    fn stress() {
        unsafe {
            let mut gc = Collector::new(1024);
            let (_, mut a) = alloc(&mut gc);
            for _ in 0..10_000 {
                if gc.rec_poll(10) {
                    unsafe { a.mark(&mut gc); }
                    gc.collect();
                }
                a = alloc(&mut gc).1;
            }
        }
    }

    fn alloc(gc: &mut Collector) -> (ValueRef, ValueRef) {
        unsafe {
            let a = gc.alloc_blob(size_of::<usize>());
            let b = gc.alloc_blob(size_of::<usize>());
            let tup = gc.alloc_rec(2);
            let int_t = gc.alloc_rec(0);
            let tuple_t = gc.alloc_rec(0);
            let type_t = gc.alloc_rec(0);

            a.set_typeref(int_t);
            b.set_typeref(int_t);
            tup.set_typeref(tuple_t);
            int_t.set_typeref(type_t);
            tuple_t.set_typeref(type_t);
            type_t.set_typeref(type_t);

            *a.as_mut_ptr().offset(2) = 3;
            *b.as_mut_ptr().offset(2) = 5;
            *tup.as_mut_ptr().offset(2) = transmute(a);
            *tup.as_mut_ptr().offset(3) = transmute(b);

            return (tup, a);
        }
    }

    fn print_heaps(gc: &Collector) {
        unsafe {
            println!("# Blobspace");
            let mut blob = gc.blobspace;
            while !blob.is_null() {
                let val = ValueRef(blob.offset(1));
                let size = val.field_count();
                let typ = val.typeref();
                let data = slice::from_raw_parts(blob.offset(3) as *const u8, size);

                print!("  {:p} -> blob<{}, {:p}>[",
                       val.as_ptr(), size, typ.as_ptr());
                for n in data {
                    print!(" {}", n);
                }
                println!("]");

                blob = *blob as *mut usize;
            }

            println!("# Fromspace");
            let mut rec = gc.fromspace.as_ptr();
            let end = rec.offset(gc.fromspace.len() as isize);
            while rec < end {
                let val = ValueRef(rec as *mut usize);
                let size = val.field_count();
                let typ = val.typeref();
                let data = slice::from_raw_parts(rec.offset(2), size);

                print!("  {:p} -> rec<{}, {:p}>[", rec, size, typ.as_ptr());
                for v in data {
                    print!(" {:p}", v as *const usize);
                }
                println!("]");

                rec = rec.offset(2 + size as isize);
            }
        }
    }
}