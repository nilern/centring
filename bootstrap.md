
1. Build crappy interpreter, no macros, bytecode etc.
   * fn
   * def
     - plain
     - Fn
     - MultiFn
   * deftype
   * defrecord
   * defenum
   * let
   * match
   * module
   * List
   * String
   * Bool
2. Use that to make a compiler
   * Reader
   * Macroexpander
   * CPS
   * Optimization
   * Closure conversion
   * Code generation
3. Compile the compiler
4. Write a new interpreter, compile it