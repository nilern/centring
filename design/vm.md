# The Centring Virtual Machine

## VM

The VM maintains pools of

* Fibers
* channels
* IO ports

VMProcesses are pre-emptive green threads. They can communicate through channels
in a CSP paradigm. The VM communicates with the operating system through IO
ports in a similar manner.

## Fiber

These actually run code. A Fiber consists of

* a reference to the VM that spawned this Fiber
* an operand stack
* a garbage-collected heap
* a program counter

At any given time the Fiber is either

* Executing a bytecode function
* Executing a native function
* Waiting for a value from a channel or IO port
* Waiting for other Fibers to run

## Semantics Components

1. Control String (code & constants)
2. Lexical environment (locals & clovers)
    * Immutable
3. Top-level environment (= the current namespace)
    * (Potentially) mutable

# Abstract machine Components

1. Executing Procedure
2. Namespace registry
3. Current namespace
4. GP Registers

## Conceptual Execution Model

* Control array contains primop functions and argument fetch descriptors.
* We basically `control[pc++](fiber)` and then the primop function fetches its
  conceptual args using `control[pc++]`. Then the primop does its actual thing.
  After this, `control[pc++]` will yield another primop.
* Primops are defined in the VM implementation language.
* The control array is derived from the Centring source program. It is obvious
  that Centring programs are nothing more than compositions of these primitives.
* `call` is an extensible primitive and allows function definition, method extension.

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
* Port

## Instruction Set

### Loads

\((\varphi: Any) \rightarrow^\kappa (Any)\)

    load d v

### Constructors

\((\varphi_0: Type, \varphi^*_i: Any...) \rightarrow^\kappa (\varphi_0)\)

    new t n vs...

### Arithmetic

\((\varphi_1: Int, \varphi_2: Int) \rightarrow \kappa(Int) | \bot(Overflow...)\)

    iadd d a b
    isub d a b
    imul d a b
    idiv d a b
    irem d a b
    imod d a b

\((\varphi: Int) \rightarrow \kappa(Int) | \bot(Overflow...)\)

    ineg d a

### Bit operations

\((\varphi_1: Int, \varphi_2: Int) \rightarrow \kappa(Int)\)

    iand d a b
    iior d a b
    ixor d a b
    iash d a b
    ilsh d a b

\((\varphi: Int) \rightarrow \kappa(Int)\)

    inot d a

### Control Flow

\(() \rightarrow \kappa()\)

    br i

\((\varphi: Bool) \rightarrow \kappa_1() | \kappa_2()\)

    brf c ilong

\((\varphi_1: Any) \rightarrow \bot (Any)\)

    halt v

### Calls

\((\varphi_0:Callable, \varphi_i^*:Any...) \rightarrow 
  \top(Any)|\bot(Type|Args...)\)

    call f n as...

<!-- Comparisons (??) -->
