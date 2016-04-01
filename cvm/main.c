#include<stdlib.h>
#include<stdio.h>
#include<inttypes.h>

// Typedefs

typedef uint32_t bytecode;
typedef uintptr_t word;
typedef uint8_t u8;
typedef uint16_t u16;
typedef intptr_t isize;
typedef uintptr_t usize;
typedef uintptr_t value_ref;

// Value

struct Value {
  enum {
    INT
  } type;
  union {
    isize i;
  } data;
};

#define INT_SHIFT 1
#define INT_TAG 1

// GC

struct Semispace {
  word* start;
  word* free;
  word size;
};

struct GcHeap {
  struct Semispace fromspace;
  struct Semispace tospace;
};

value_ref alloc(struct GcHeap* heap, struct Value* val) {
  switch (val->type) {
  case INT:
    return val->data.i << INT_SHIFT | INT_TAG;
  }
}

// VM

struct Stack {
  value_ref* start;
  usize len;
  usize capacity;
};

struct Stack Stack_new() {
  struct Stack stack = {0, 0, 0};
  return stack;
}

struct Stack Stack_with_capacity(usize cap) {
  struct Stack stack = {0, 0, cap};
  stack.start = malloc(sizeof(word)*cap);
  return stack;
}

void push(struct Stack* stack, value_ref w) {
  stack->start[stack->len++] = w;
}

struct VM { };

struct VMProcess {
  struct VM* vm;
};

struct VMProcess* spawn(struct VM* vm) {
  struct VMProcess* proc = (struct VMProcess*) malloc(sizeof(struct VMProcess));
  proc->vm = vm;
  return proc;
}

value_ref run(struct VMProcess* proc) {
  return 0;
}

// Bytecode

enum opcode {
  CONST,
  ADDI,
  SUBI,
  MULI,
  DIVI
};

#define OPCODE_BC 8
#define ARG0_SHIFT 8
#define ARG1_SHIFT 16

bytecode instr1(enum opcode op, u16 arg) {
  return arg << ARG0_SHIFT | op;
}

bytecode instr2(enum opcode op, u8 arg0, u8 arg1) {
  return arg1 << ARG1_SHIFT | arg0 << ARG0_SHIFT | op;
}

// Main

int main(int argc, char** argv) {
  struct Stack stack = Stack_with_capacity(5);
  push(&stack, 3);
  push(&stack, 5);
  push(&stack, -24);
  push(&stack, 3);
  push(&stack, 5);
  for (usize i = 0; i < stack.len; i++) {
    printf("%li, ", stack.start[i]);
  }
}
