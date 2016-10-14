use value::{ValueRef, ValueRefT};

use std::slice;
use std::mem::{size_of, align_of, transmute, swap};
use std::ptr;
use alloc::heap;

// TODO: also consider blobs when polling and triggering collections

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
        ValueRef::from_raw_parts(block.offset(1), size, false, false)
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

        ValueRef::from_raw_parts(dest, field_count, true, false)
    }

    pub fn move_rec_slice(&mut self, src: &[usize]) -> ValueRef {
        unsafe {
            let dest = self.tospace.as_mut_ptr().offset(self.tospace.len() as isize);
            self.tospace.extend_from_slice(src);
            ValueRef::from_raw(dest)
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
            let size = ValueRef::from_raw(scan).field_count();
            scan = scan.offset(1);
            *scan = transmute(ValueRef::from_raw(*scan as *mut usize).mark(self));
            scan = scan.offset(1);
            for _ in 0..size {
                *scan = transmute(ValueRef::from_raw(*scan as *mut usize).mark(self));
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
                let val = ValueRef::from_raw((*blob).offset(1));
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
    use super::Collector;
    use value::{ValueRef, ValueRefT};

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

            a.set_type(int_t);
            b.set_type(int_t);
            tup.set_type(tuple_t);
            int_t.set_type(type_t);
            tuple_t.set_type(type_t);
            type_t.set_type(type_t);

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
                let val = ValueRef::from_raw(blob.offset(1));
                let size = val.field_count();
                let typ = val.get_type();
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
                let val = ValueRef::from_raw(rec as *mut usize);
                let size = val.field_count();
                let typ = val.get_type();
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
