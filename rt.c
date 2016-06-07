#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#include "rt.h"

/* Allocation */

ctr_blob* ctr_alloc_blob(size_t byte_count) {
  ctr_blob* res = malloc(sizeof(ctr_blob) + byte_count*sizeof(ctr_byte));

  res->box.size = byte_count;
  res->box.memkind = CTR_BLOB;
  res->box.marked = false;
  
  res->next = ctr_blobspace;
  ctr_blobspace = res;
  return res;
}

ctr_rec* ctr_alloc_rec(size_t field_count) {
  ctr_rec* res = (ctr_rec*)ctr_next_rec;

  res->box.size = field_count;
  res->box.memkind = CTR_REC;
  res->box.marked = false;

  ctr_next_rec += sizeof(ctr_rec)/sizeof(ctr_word) + field_count;
  return res;
}

/* Collection */

void ctr_collect(ctr_rec* cont) {
  static bool grew = false;
  
  // Switch allocation space:
  ctr_next_rec = ctr_tospace;

  // Mark roots:
  ctr_mark((ctr_value*)cont);

  // Copy live records:
  for (ctr_word* scan = ctr_tospace; scan < ctr_next_rec; ) {
    ctr_word size = ((ctr_value*)scan)->size;
    ((ctr_value*)scan)->type = ctr_mark(((ctr_value*)scan)->type);
    scan += sizeof(ctr_rec)/sizeof(ctr_word);
    for (ctr_word i = 0; i < size; i++, scan++) {
      *scan = (ctr_word)ctr_mark((ctr_value*)(*scan));
    }
  }
    
  // Sweep blobspace:
  ctr_sweep();

  // Swap semispaces:
  ctr_word* tmp = ctr_fromspace;
  ctr_fromspace = ctr_tospace;
  ctr_tospace = tmp;

  // Grow tospace if necessary:
  if (grew) {
    // Catch up with the other semispace:
    ctr_tospace = realloc(ctr_tospace, ctr_heapsize);
    grew = false;
  } else if ((ctr_next_rec - ctr_fromspace)*sizeof(ctr_word)
             > (size_t)(0.8*ctr_heapsize)) {
    // Low on memory, grow tospace:
    ctr_heapsize *= 2;
    ctr_tospace = realloc(ctr_tospace, ctr_heapsize);
    grew = true; // remember that tospace grew
  }
}

ctr_value* ctr_mark(ctr_value* val) {
  if (val->memkind == CTR_BLOB) {
    if (val->marked) {
      return val; // blobs don't move
    } else {
      /* Mark the blob and its type: */
      val->marked = true;
      val->type = ctr_mark(val->type);
      return val;
    }
  } else {
    if (val->marked) {
      return val->type; // get forward pointer
    } else {
      /* Copy to tospace: */
      ctr_value* res = (ctr_value*)ctr_next_rec;
      ctr_word size = val->size;
      memcpy((void*)res, (void*)val, sizeof(ctr_rec) + size*sizeof(ctr_value*));
      ctr_next_rec += sizeof(ctr_rec)/sizeof(ctr_word) + size;
      /* Set mark and forward pointer: */
      val->marked = true;
      val->type = res;
      return res;
    }
  }
}

void ctr_sweep(void) {
  ctr_blob** blob = &ctr_blobspace;
  while (*blob) {
    if ((*blob)->box.marked) {
      (*blob)->box.marked = false; // unmark
      blob = &(*blob)->next; // move on
    } else {
      ctr_blob* unreached = *blob;
      *blob = unreached->next; // drop from blobspace, move on
      free(unreached); // free
    }
  }
}

/* Initialization */

void ctr_init(void) {
  ctr_heapsize = 1024*sizeof(ctr_byte);
  ctr_fromspace = malloc(ctr_heapsize);
  ctr_tospace = malloc(ctr_heapsize);
  ctr_next_rec = ctr_fromspace;
  ctr_blobspace = NULL;
}

/* Testing */

void print_heaps(void) {
  puts("# Blobspace");
  for (ctr_blob* blob = ctr_blobspace; blob != NULL; blob = blob->next) {
    ctr_word size = ((ctr_value*)blob)->size;
    printf("  %p -> blob<%lu, %p>[", blob, size, blob->box.type);
    for (ctr_word i = 0; i < size; i++) {
      printf(" %d", blob->bytes[i]);
    }
    puts(" ]");
  }
  
  printf("# Fromspace (%lu bytes)\n", ctr_heapsize);
  ctr_word size;
  for (ctr_word* recp = (ctr_word*)ctr_fromspace;
       recp < (ctr_word*)ctr_next_rec;
       recp += sizeof(ctr_rec)/sizeof(ctr_word) + size) {
    ctr_rec* rec = (ctr_rec*)recp;
    size = ((ctr_value*)rec)->size;
    printf("  %p -> rec<%lu, %p>{", rec, size, rec->box.type);
    for (ctr_word i = 0; i < size; i++) {
      printf(" %p", rec->fields[i]);
    }
    puts(" }");
  }
}

int main() {
  ctr_init();
  
  ctr_blob* a = ctr_alloc_blob(8);
  ctr_blob* b = ctr_alloc_blob(8);
  ctr_rec* tup = ctr_alloc_rec(2);
  ctr_rec* IntType = ctr_alloc_rec(0);
  ctr_rec* TupleType = ctr_alloc_rec(0);
  ctr_rec* TypeType = ctr_alloc_rec(0);

  a->box.type = (ctr_value*)IntType;
  b->box.type = (ctr_value*)IntType;
  tup->box.type = (ctr_value*)TupleType;
  IntType->box.type = (ctr_value*)TypeType;
  TupleType->box.type = (ctr_value*)TypeType;
  TypeType->box.type = (ctr_value*)TypeType;

  a->bytes[7] = 3;
  b->bytes[7] = 5;
  tup->fields[0] = (ctr_value*)a;
  tup->fields[1] = (ctr_value*)b;
  
  print_heaps();

  puts("\ncollecting...\n");
  ctr_collect(tup);
  
  print_heaps();
}
