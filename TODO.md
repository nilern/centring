# TODO

## ctri

* [ ] Primops
* [ ] deftype & data layout
* [ ] FFI
* [ ] Stack traces
* [ ] Concurrent ML
* [ ] IO
* [ ] Basic macros
* [ ] modules
* [ ] PUC Lua source (https://m.reddit.com/r/programming/comments/63hth/ask_reddit_which_oss_codebases_out_there_are_so/)
* [ ] Ctx module, hide implementation
* [ ] Java/Go/Julia packed struct GC (?)

## ctrc

* [ ] Bootstrap Compiler
    - method fusion
    - eta contraction
    - beta contraction
    - closure conversion
    - linearization of closure constructions
* [ ] Optimizations
    - Beta contraction (needed to remove overhead of do, let etc.)
    - Eta contraction (needed for TCO)
    - Constant folding & propagation
    - Inlining
    - Partial evaluation and supercompilation
* [ ] Compiler
* [ ] VM
    - Value
    - GC
    - Bytecodes
    - Execution loop
    - IO
* [ ] GC
* [ ] Reader (in ctr)
* [ ] Macros
    * Hygiene
* [ ] Dispatch compilation
* [ ] Environments & modules
    - import (import every var from some other ns)
* [ ] CSP-lets/actors
* [ ] FFI
    - Conversion of immediate types (Int, Bool, Char...)
    - Other 'bits types' (UInt8...)
    - FFIBuffer for arrays, strings, structs, pointees (managed by non-moving GC)