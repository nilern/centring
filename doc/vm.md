# The Centring Virtual Machine

## VM

The VM maintains pools of

* VMProcesses
* channels
* IO ports

VMProcesses are pre-emptive green threads. They can communicate through channels
in a CSP paradigm. The VM communicates with the operating system through IO
ports in a similar manner.

## VMProcess

These actually run code. A VMProcess consists of

* a reference to the VM that spawned this VMProcess 
* an operand stack
* a garbage-collected heap
* a program counter

At any given time the VMProcess is either

* Executing a bytecode function
* Executing a native function
* Waiting for a value from a channel or IO port
* Waiting for other VMProcesses to run

## Data Representation

### Immediate Values

* Int
* Char
* Bool
* Void

### Block Values

* Tuple
* Array
* Buffer
* Closure
* Procedure
* NativeProcedure

## Instruction Set

* Instruction operands
  - i is just an unsigned number, an index into something
  - a and b are also indices but use 2 bits to encode what they index:
    * Locals
    * Globals
    * Closed-overs
    * Constants
* Instructions with an i- -prefix assume the arguments will be Ints
* Explanations in Rust-flavored pseudocode and C-like operators.

### Loads

```
local  i // stack.push(stack[i])
global i // stack.push(curr_module[global_names[i]])
clover i // stack.push(clovers[i])
const  i // stack.push(consts[i])
fn     i // let proc = codeobjs[i]
         // fn_clovers = stack.split_off(stack.len() - proc.clover_count)
         // stack.push(Closure(proc, fn_clovers))
```

### Arithmetic

```
iadd a b // stack.push(fetch(a).iadd(fetch(b))) // +
isub a b // stack.push(fetch(a).isub(fetch(b))) // -
imul a b // stack.push(fetch(a).imul(fetch(b))) // *
idiv a b // stack.push(fetch(a).idiv(fetch(b))) // /
ineg a   // stack.push(fetch(a).ineg())         // -
irem a b // stack.push(fetch(a).irem(fetch(b))) // remainder (signed)
imod a b // stack.push(fetch(a).imod(fetch(b))) // modulus (always positive)
```

### Bit Operations

```
iand  a b // stack.push(fetch(a).iand(fetch(b)))  // &
ior   a b // stack.push(fetch(a).ior(fetch(b)))   // |
ixor  a b // stack.push(fetch(a).ixor(fetch(b)))  // ^
inot  a   // stack.push(fetch(a).inot())          // ~
iashr a b // stack.push(fetch(a).iashr(fetch(b))) // >> (arithmetic)
iashl a b // stack.push(fetch(a).iashl(fetch(b))) // << (arithmetic)
ilshr a b // stack.push(fetch(a).ilshr(fetch(b))) // >> (logical)
ilshl a b // stack.push(fetch(a).ilshl(fetch(b))) // << (logical)
```

### Comparisons

```
eq   a b // stack.push(fetch(a).iand(fetch(b))) // same bit pattern?
ne   a b // stack.push(fetch(a).iand(fetch(b))) // different bit patterns?
igt  a b // stack.push(fetch(a).igt(fetch(b)))  // >
ilt  a b // stack.push(fetch(a).ilt(fetch(b)))  // <
ige  a b // stack.push(fetch(a).ige(fetch(b)))  // >=
ile  a b // stack.push(fetch(a).ile(fetch(b)))  // <=
```

### Boolean Operations

```
and a b // stack.push(fetch(a).and(fetch(b))) // &&
or  a b // stack.push(fetch(a).or(fetch(b)))  // ||
not a   // stack.push(fetch(a).not(fetch(b))) // !
```

### Control Flow

```
brf  a i // if fetch(a) { pc = i }
halt a   // a -> ret_chan; terminate VMProcess
```

### Calls

```
call i
```
