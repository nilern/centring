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
    pub fn as_ptr(self) -> *const usize {
        self.0 as *const usize
    }

    #[inline(always)]
    pub fn as_mut_ptr(self) -> *mut usize {
        self.0
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
    fn typeref(self) -> ValueRef {
        unsafe { ValueRef(*self.as_ptr().offset(1) as *mut usize) }
    }

    #[inline(always)]
    fn set_typeref(self, tref: ValueRef) {
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

    fn mark(self, ctx: &mut GCState) -> ValueRef {
        if self.marked() {
            if self.is_rec() {
                return self.fwd();
            } else {
                return self;
            }
        } else {
            if self.is_rec() {
                let res = unsafe {
                    ctx.move_rec_slice( // move data
                        slice::from_raw_parts(self.as_ptr(),
                        2 + self.field_count()))
                };
                self.set_fwd(res);
                self.set_mark_bit();
                return res;
            } else {
                self.set_typeref(self.typeref().mark(ctx));
                self.set_mark_bit();
                return self;
            }
        }
    }

    pub unsafe fn unbox<T: Copy>(self) -> T {
        *(self.as_ptr().offset(1) as *const T)
    }
}

pub struct GCState {
    fromspace: Vec<usize>,
    tospace: Vec<usize>,
    blobspace: *mut usize
}

impl GCState {
    #[inline(always)]
    pub fn new(heapsize: usize) -> GCState {
        GCState {
            fromspace: Vec::with_capacity(heapsize),
            tospace: Vec::with_capacity(heapsize),
            blobspace: ptr::null::<usize>() as *mut usize
        }
    }

    #[inline(always)]
    pub fn rec_poll(&self, word_count: usize) -> bool {
        self.fromspace.capacity() - self.fromspace.len() >= word_count
    }

    pub fn alloc_blob(&mut self, byte_count: usize) -> ValueRef {
        unsafe {
            let block: *mut usize = transmute(
                heap::allocate(3 * size_of::<usize>() + byte_count,
                               align_of::<usize>()));
            *block = self.blobspace as usize;
            self.blobspace = block;
            ValueRef::new(block.offset(1), byte_count, false, false)
        }
    }

    pub fn alloc_rec(&mut self, field_count: usize) -> ValueRef {
        unsafe {
            let len = self.fromspace.len();
            let size = 2 + field_count;
            self.fromspace.set_len(len + size);
            let dest = self.fromspace.as_mut_ptr().offset(len as isize);

            ValueRef::new(dest, field_count, true, false)
        }
    }

    fn move_rec_slice(&mut self, src: &[usize]) -> ValueRef {
        unsafe {
            let dest = self.tospace.as_mut_ptr().offset(self.tospace.len() as isize);
            self.tospace.extend_from_slice(src);
            ValueRef(dest)
        }
    }

    pub unsafe fn alloc<T: Clone>(&mut self, v: T) -> ValueRef {
        let vref = self.alloc_blob(size_of::<T>());
        *(vref.as_mut_ptr().offset(1) as *mut T) = v;
        vref
    }

    pub fn collect(&mut self) {
        unsafe {
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
    use super::{GCState, ValueRef};
    use std::mem::{size_of, transmute};
    use std::slice;

    #[test]
    fn it_works() {
        let mut gc = GCState::new(1024);

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

    #[test]
    fn stress() {
        let mut gc = GCState::new(1024);
        let (_, mut a) = alloc(&mut gc);
        for _ in 0..1000_000 {
            if gc.rec_poll(10) {
                a.mark(&mut gc);
                gc.collect();
            }
            a = alloc(&mut gc).1;
        }
    }

    fn alloc(gc: &mut GCState) -> (ValueRef, ValueRef) {
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

        unsafe {
            *a.as_mut_ptr().offset(2) = 3;
            *b.as_mut_ptr().offset(2) = 5;
            *tup.as_mut_ptr().offset(2) = transmute(a);
            *tup.as_mut_ptr().offset(3) = transmute(b);
        }

        return (tup, a);
    }

    fn print_heaps(gc: &GCState) {
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
