use value::{CtrValue, Any, Type};
use refs::{ValuePtr, Root, ValueHandle};

use std::ptr;
use std::mem::{size_of, align_of, transmute, swap};
use std::slice;

use alloc::heap;

/// A garbage collector (holds the memory areas and other GC state).
pub struct Collector {
    fromspace: Vec<ValuePtr>,
    tospace: Vec<ValuePtr>,
    free_rec: ValuePtr,

    blobspace: *mut Blob,
    blob_bytes_allocated: usize,
}

#[repr(C)]
struct Blob {
    next: *mut Blob,
    data: Any,
}

const BYTE_INTERVAL: usize = 1048_576; // 1 MiB

impl Collector {
    /// Create a new `Collector`.
    pub fn new(heapsize: usize) -> Collector {
        let fromspace = Vec::with_capacity(heapsize);
        let free_rec = fromspace.as_ptr() as ValuePtr;
        Collector {
            fromspace: fromspace,
            tospace: Vec::with_capacity(heapsize),
            free_rec: free_rec,
            blobspace: ptr::null::<Blob>() as *mut Blob,
            blob_bytes_allocated: 0,
        }
    }

    /// Can we allocate a record with `field_count` fields or do we need to run
    /// a garbage collection first?
    pub fn rec_poll(&self, field_count: usize) -> bool {
        self.fromspace.capacity() - self.fromspace.len() >=
        size_of::<Any>() / size_of::<ValuePtr>() + field_count
    }

    /// Can we allocate a blob with `byte_count` bytes of data or do we need to
    /// run a garbage collection first?
    pub fn bytes_poll(&self, byte_count: usize) -> bool {
        self.blob_bytes_allocated + byte_count <= BYTE_INTERVAL
    }

    /// Construct a record with a type of `typ` and field values of `fields`.
    ///
    /// # Safety
    /// You must first use `bytes_poll` and if that is false, you need to mark
    /// the GC roots and call `collect` to free/allocate more space in
    /// the GC heap.
    pub unsafe fn alloc_rec(&mut self,
                            typ: ValueHandle<Type>,
                            fields: &[ValueHandle<Any>])
                            -> ValuePtr {
        let len = self.fromspace.len();
        self.fromspace.set_len(len + size_of::<Any>() / size_of::<ValuePtr>() + fields.len());

        let res = self.free_rec;
        (*res).header = Any::header(fields.len(), true);
        (*res).typ = typ.ptr();

        let mut field = res.offset(1) as *mut ValuePtr;
        for i in 0..fields.len() {
            *field = fields[i].ptr();
            field = field.offset(1);
        }
        self.free_rec = field as ValuePtr;
        res
    }

    /// Construct a record with a type of `typ` and `field_count` field values of `fields`.
    ///
    /// # Safety
    /// You must first use `bytes_poll` and if that is false, you need to mark
    /// the GC roots and call `collect` to free/allocate more space in
    /// the GC heap.
    ///
    /// `fields` should have exactly `fields_count` items.
    pub unsafe fn alloc_rec_iter<I>(&mut self, typ: ValueHandle<Type>, field_count: usize,
                                       fields: I) -> ValuePtr
        where I: Iterator<Item=Root<Any>> {
        let len = self.fromspace.len();
        self.fromspace.set_len(len + size_of::<Any>() / size_of::<ValuePtr>() + field_count);

        let res = self.free_rec;
        (*res).header = Any::header(field_count, true);
        (*res).typ = typ.ptr();

        let mut field = res.offset(1) as *mut ValuePtr;
        for fv in fields {
            *field = fv.ptr();
            field = field.offset(1);
        }
        self.free_rec = field as ValuePtr;
        res
    }

    /// Construct a flat value with a type of `typ` and `data` as the payload.
    ///
    /// # Safety
    /// You must first use `rec_poll` and if that is false, you need to mark
    /// the GC roots and call `collect` to free/allocate more space in
    /// the GC heap.
    pub unsafe fn alloc_bytes(&mut self, typ: ValueHandle<Type>, data: &[u8]) -> ValuePtr {
        let blob: *mut Blob = transmute(heap::allocate(size_of::<Blob>() + data.len(),
                                                       align_of::<Blob>()));
        (*blob).next = self.blobspace;
        self.blobspace = blob;
        self.blob_bytes_allocated += data.len();

        let res = transmute::<&Any, ValuePtr>(&(*blob).data);
        (*res).header = Any::header(data.len(), false);
        (*res).typ = typ.ptr();

        let data_dest = slice::from_raw_parts_mut(res.offset(1) as *mut u8, data.len());
        data_dest.copy_from_slice(data);
        res
    }

    fn move_rec_slice(&mut self, src: &[ValuePtr]) -> ValuePtr {
        unsafe {
            let dest = self.tospace
                .as_mut_ptr()
                .offset(self.tospace.len() as isize) as *mut Any;
            self.tospace.extend_from_slice(src);
            dest
        }
    }

    /// Mark/move a Value. Return a pointer to its new location.
    ///
    /// # Safety
    /// `ptr` must actually point into the GC heap.
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
                let res = self.move_rec_slice(// move data
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
        while scan < (self.tospace.as_mut_ptr() as *mut usize).offset(self.tospace.len() as isize) {
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
        if self.fromspace.len() > 8 * self.fromspace.capacity() / 10 {
            self.fromspace.reserve(size_of::<usize>());
            self.tospace.reserve(size_of::<usize>());
        }
        self.blob_bytes_allocated = 0;
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
