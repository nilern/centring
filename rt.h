#include <stdint.h>

/* General Typedefs */

typedef uintptr_t ctr_word;
typedef unsigned char ctr_byte;

typedef enum { CTR_BLOB, CTR_REC } ctr_memkind;

/* Value */

typedef struct ctr_svalue {
  ctr_word size : sizeof(ctr_word) - 2;
  bool memkind : 1;
  ctr_memkind marked : 1; 
  struct ctr_svalue* type;
} ctr_value;

typedef struct ctr_sblob {
  ctr_value box;
  struct ctr_sblob* next;
  ctr_byte bytes[];
} ctr_blob;

typedef struct {
  ctr_value box;
  ctr_value* fields[];
} ctr_rec;

/* Storage */

ctr_word* ctr_fromspace;
ctr_word* ctr_tospace;
ctr_word* ctr_next_rec;
size_t ctr_heapsize;
ctr_blob* ctr_blobspace;

/* Allocation */

ctr_rec* ctr_alloc_rec(size_t field_count);
ctr_blob* ctr_alloc_blob(size_t byte_count);

/* Collection */

void ctr_collect(ctr_rec* cont);
ctr_value* ctr_mark(ctr_value* val);
void ctr_sweep(void);

/* Initialization */

void ctr_init(void);
