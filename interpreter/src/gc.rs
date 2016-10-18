use value::{Any, CtrValue};
use refs::ValueRef;

use std::ptr;
use std::mem::{size_of, align_of, transmute, swap};
use alloc::heap;

// TODO: also consider blobs when polling and triggering collections

/// A garbage collector (holds the memory areas and other GC state).
pub struct Collector {
    fromspace: Vec<ValueRef>,
    tospace: Vec<ValueRef>,
    blobspace: *mut Blob
}

#[repr(C)]
struct Blob {
    next: *mut Blob,
    data: Any
}

impl Collector {
    /// Create a new `Collector`.
    #[inline(always)]
    pub fn new(heapsize: usize) -> Collector {
        Collector {
            fromspace: Vec::with_capacity(heapsize),
            tospace: Vec::with_capacity(heapsize),
            blobspace: ptr::null::<Blob>() as *mut Blob
        }
    }


    #[inline(always)]
    pub fn rec_poll(&self, word_count: usize) -> bool {
        self.fromspace.capacity() - self.fromspace.len() >= word_count
    }

    unsafe fn alloc_pointy<T: CtrValue>(&mut self, v: T) -> ValueRef {
        let len = self.fromspace.len();
        let alloc_len = v.as_any().alloc_len();
        let size = 2 + alloc_len;
        self.fromspace.set_len(len + size);
        let dest = self.fromspace.as_mut_ptr().offset(len as isize) as *mut T;

        *dest = v;
        ValueRef::from_raw(dest as *mut Any)
    }

    unsafe fn alloc_flat<T: CtrValue>(&mut self, v: T) -> ValueRef {
        let data_size = v.as_any().alloc_len();
        let block: *mut Blob = transmute(
            heap::allocate(size_of::<Blob>() + data_size,
                           align_of::<Blob>()));
        (*block).next = self.blobspace;
        self.blobspace = block;

        let dest = (block as *mut usize).offset(1) as *mut T;
        *dest = v;
        ValueRef::from_raw(dest as *mut Any)
    }

    /// Move a Centring value into the GC heap and return a ValueRef to its new
    /// position. Comparable to `Box::new()` and `Rc::new()`.
    pub unsafe fn alloc<T: CtrValue>(&mut self, v: T) -> ValueRef {
        if v.as_any().pointy() {
            self.alloc_pointy(v)
        } else {
            self.alloc_flat(v)
        }
    }

    pub fn move_rec_slice(&mut self, src: &[ValueRef]) -> ValueRef {
        unsafe {
            let dest = self.tospace
                .as_mut_ptr().offset(self.tospace.len() as isize) as *mut Any;
            self.tospace.extend_from_slice(src);
            ValueRef::from_raw(dest)
        }
    }

    /// Collect garbage. Assumes that all roots have already been `mark()`:ed.
    ///
    /// # Safety
    /// You must mark all roots and let go of all raw ValueRef:s before calling
    /// this. Otherwise you will get segfaults or at the very least pointers to
    /// garbage data.
    pub unsafe fn collect(&mut self) {
        let mut scan: *mut usize = self.tospace.as_mut_ptr() as *mut usize;
        let mut free: *mut usize = scan.offset(self.tospace.len() as isize);
        while scan < (self.tospace.as_mut_ptr() as *mut usize)
                          .offset(self.tospace.len() as isize) {
            let len = (*(scan as *mut Any)).alloc_len();
            scan = scan.offset(1);
            *scan = transmute(ValueRef::from_raw(*scan as *mut Any).mark(self));
            scan = scan.offset(1);

            for _ in 0..len {
                *scan = transmute(ValueRef::from_raw(*scan as *mut Any).mark(self));
                scan = scan.offset(1);
            }

            free = free.offset(2 + len as isize);
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
            let mut blob: *mut *mut usize = transmute(&mut self.blobspace);
            while !(*blob).is_null() {
                let mut val = ValueRef::from_raw((*blob).offset(1) as *mut Any);
                if val.marked() {
                    val.unset_mark_bit();
                    blob = *blob as *mut *mut usize;
                } else {
                    let unreached = *blob;
                    *blob = *unreached as *mut usize;
                    heap::deallocate(unreached as *mut u8,
                                     3 * size_of::<usize>() + val.alloc_len(),
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
    use value::{Any, Bits, ListPair};
    use refs::ValueRef;

    use std::mem::transmute;
    use std::slice;
    use std::borrow::{Borrow, BorrowMut};
    use std::ptr;

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
                    a.mark(&mut gc);
                    gc.collect();
                }
                a = alloc(&mut gc).1;
            }
        }
    }

    fn alloc(gc: &mut Collector) -> (ValueRef, ValueRef) {
        unsafe {
            let mut a = gc.alloc(Bits::new(3isize));
            let mut b = gc.alloc(Bits::new(5isize));
            let mut tup = gc.alloc(ListPair {
                header: Any::header(2, true),
                typ: ValueRef::from_raw(ptr::null::<Any>() as *mut Any),
                head: a,
                tail: b
            });

            // HACK:
            let ptr = *a.borrow();
            (*a.borrow_mut()).typ = ptr;
            (*b.borrow_mut()).typ = ptr;
            (*tup.borrow_mut()).typ = ptr;

            return (tup, b)
        }
    }

    fn print_heaps(gc: &Collector) {
        unsafe {
            println!("# Blobspace");
            let mut blob = gc.blobspace;
            while !blob.is_null() {
                let val = &(*blob).data;
                let size = val.alloc_len();
                let typ = val.get_type();
                let datap = transmute::<&Any, *mut Any>(val).offset(1);
                let data = slice::from_raw_parts(datap as *const u8, size);

                print!("  {:p} -> blob<{}, {:p}>[",
                       val, size, typ.as_ptr());
                for n in data {
                    print!(" {}", n);
                }
                println!("]");

                blob = (*blob).next;
            }

            println!("# Fromspace");
            let mut rec = gc.fromspace.as_ptr();
            let end = rec.offset(gc.fromspace.len() as isize);
            while rec < end {
                let val = ValueRef::from_raw(rec as *mut Any);
                let size = val.alloc_len();
                let typ = val.get_type();
                let data = slice::from_raw_parts(rec.offset(2), size);

                print!("  {:p} -> rec<{}, {:p}>[", rec, size, typ.as_ptr());
                for v in data {
                    print!(" {:p}", v.as_ptr());
                }
                println!("]");

                rec = rec.offset(2 + size as isize);
            }
        }
    }
}
