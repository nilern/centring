use value::Any;
use refs::ValuePtr;

use std::ptr;
use std::mem::{size_of, align_of, transmute, swap};
use std::slice;

use alloc::heap;

// TODO: also consider blobs when polling and triggering collections

/// A garbage collector (holds the memory areas and other GC state).
pub struct Collector {
    fromspace: Vec<ValuePtr>,
    tospace: Vec<ValuePtr>,
    blobspace: *mut Blob
}

#[repr(C)]
struct Blob {
    next: *mut Blob,
    data: Any
}

impl Collector {
    /// Create a new `Collector`.
    pub fn new(heapsize: usize) -> Collector {
        Collector {
            fromspace: Vec::with_capacity(heapsize),
            tospace: Vec::with_capacity(heapsize),
            blobspace: ptr::null::<Blob>() as *mut Blob
        }
    }

    pub fn rec_poll(&self, word_count: usize) -> bool {
        self.fromspace.capacity() - self.fromspace.len() >= word_count
    }

    pub unsafe fn alloc_rec<I>(&mut self, alloc_len: usize, typ: ValuePtr, fields: I)
        -> ValuePtr where I: Iterator<Item=ValuePtr> {
        let len = self.fromspace.len();
        let size = size_of::<Any>()/size_of::<ValuePtr>() + alloc_len;
        self.fromspace.set_len(len + size);
        let mut dest = self.fromspace.as_mut_ptr().offset(len as isize) as *mut ValuePtr;
        let start = dest;

        *dest = Any::header(alloc_len, true) as ValuePtr;
        dest = dest.offset(1);
        *dest = typ;
        for field in fields {
            dest = dest.offset(1);
            *dest = field;
        }
        start as ValuePtr
    }

    pub unsafe fn alloc_bits<T: Clone>(&mut self, typ: ValuePtr, v: T) -> ValuePtr {
        let data_size = size_of::<T>();
        let block: *mut Blob = transmute(
            heap::allocate(size_of::<Blob>() + data_size,
                           align_of::<Blob>()));
        (*block).next = self.blobspace;
        self.blobspace = block;

        let mut dest = (block as *mut ValuePtr).offset(1);
        let start = dest as ValuePtr;
        *dest = Any::header(data_size, false) as ValuePtr;
        dest = dest.offset(1);
        *dest = typ;
        *(dest.offset(1) as *mut T) = v;
        start
    }

    fn move_rec_slice(&mut self, src: &[ValuePtr]) -> ValuePtr {
        unsafe {
            let dest = self.tospace
                .as_mut_ptr().offset(self.tospace.len() as isize) as *mut Any;
            self.tospace.extend_from_slice(src);
            dest
        }
    }

    pub unsafe fn mark(&mut self, ptr: ValuePtr) -> ValuePtr {
        if (*ptr).marked() {
            if (*ptr).pointy() {
                (*ptr).typ // get forward pointer
            } else {
                ptr
            }
        } else {
            if (*ptr).pointy() {
                // we have to do the copy first so that only the fromspace
                // version becomes a broken heart:
                let res =
                    self.move_rec_slice( // move data
                        slice::from_raw_parts(ptr as *mut ValuePtr,
                            2 + (*ptr).alloc_len()));
                (*ptr).typ = res; // set forward pointer
                (*ptr).set_mark_bit();
                res
            } else {
                // on the other hand here we need to set the bit first to avoid
                // infinite recursive loops:
                (*ptr).set_mark_bit();
                (*ptr).typ = self.mark((*ptr).typ);
                ptr
            }
        }
    }

    /// Collect garbage. Assumes that all roots have already been `mark()`:ed.
    ///
    /// # Safety
    /// You must mark all roots and let go of all raw ValuePtr:s before calling
    /// this. Otherwise you will get segfaults or at the very least pointers to
    /// garbage data.
    pub unsafe fn collect(&mut self) {
        let mut scan: *mut usize = self.tospace.as_mut_ptr() as *mut usize;
        let mut free: *mut usize = scan.offset(self.tospace.len() as isize);
        while scan < (self.tospace.as_mut_ptr() as *mut usize)
                          .offset(self.tospace.len() as isize) {
            let len = (*(scan as *mut Any)).alloc_len();
            scan = scan.offset(1);
            *scan = transmute(self.mark(*scan as ValuePtr));
            scan = scan.offset(1);

            for _ in 0..len {
                *scan = transmute(self.mark(*scan as ValuePtr));
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
                let val = (*blob).offset(1) as ValuePtr;
                if (*val).marked() {
                    (*val).unset_mark_bit();
                    blob = *blob as *mut *mut usize;
                } else {
                    let unreached = *blob;
                    *blob = *unreached as *mut usize;
                    heap::deallocate(unreached as *mut u8,
                                     size_of::<Blob>() + (*val).alloc_len(),
                                     align_of::<usize>());
                }
            }
        }
    }
}

// /// # Tests
//
// // NOTE: segfaulting ATM since all type pointers are null
//
// #[cfg(test)]
// mod tests {
//     use super::Collector;
//     use value::Any;
//     use refs::ValuePtr;
//     use interpreter::Interpreter;
//
//     use std::mem::transmute;
//     use std::slice;
//
//     #[test]
//     fn it_works() {
//         let mut itp = Interpreter::new();
//         unsafe {
//             let mut gc = Collector::new(1024);
//
//             let (tup, a) = alloc(&mut itp);
//
//             print_heaps(&gc);
//
//             println!("\ncollecting...\n");
//             gc.mark(tup);
//             gc.collect();
//
//             print_heaps(&gc);
//
//             println!("\ncollecting...\n");
//             gc.mark(a);
//             gc.collect();
//
//             print_heaps(&gc);
//         }
//     }
//
//     // #[test]
//     // fn stress() {
//     //     let mut itp = Interpreter::new();
//     //     unsafe {
//     //         let mut gc = Collector::new(1024);
//     //         let (_, mut a) = alloc(&mut itp, &mut gc);
//     //         for _ in 0..10_000 {
//     //             if gc.rec_poll(10) {
//     //                 gc.mark(a);
//     //                 gc.collect();
//     //             }
//     //             a = alloc(&mut itp, &mut gc).1;
//     //         }
//     //     }
//     // }
//
//     fn alloc(itp: &mut Interpreter) -> (ValuePtr, ValuePtr) {
//         unsafe {
//             let a = itp.alloc_int(3);
//             let b = itp.alloc_int(5);
//             let tup = itp.alloc_pair(a.borrow(), b.borrow());
//
//             return (tup.ptr(), b.ptr())
//         }
//     }
//
//     fn print_heaps(gc: &Collector) {
//         unsafe {
//             println!("# Blobspace");
//             let mut blob = gc.blobspace;
//             while !blob.is_null() {
//                 let val = &(*blob).data;
//                 let size = val.alloc_len();
//                 let typ = val.get_type();
//                 let datap = transmute::<&Any, ValuePtr>(val).offset(1);
//                 let data = slice::from_raw_parts(datap as *const u8, size);
//
//                 print!("  {:p} -> blob<{}, {:p}>[",
//                        val, size, typ);
//                 for n in data {
//                     print!(" {}", n);
//                 }
//                 println!("]");
//
//                 blob = (*blob).next;
//             }
//
//             println!("# Fromspace");
//             let mut rec = gc.fromspace.as_ptr();
//             let end = rec.offset(gc.fromspace.len() as isize);
//             while rec < end {
//                 let val = &*(rec as ValuePtr);
//                 let size = val.alloc_len();
//                 let typ = val.get_type();
//                 let data = slice::from_raw_parts(rec.offset(2), size);
//
//                 print!("  {:p} -> rec<{}, {:p}>[", rec, size, typ);
//                 for v in data {
//                     print!(" {:p}", v);
//                 }
//                 println!("]");
//
//                 rec = rec.offset(2 + size as isize);
//             }
//         }
//     }
// }
