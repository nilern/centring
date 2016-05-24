(module centring.emit
  *

  (import scheme chicken)
  (use centring.cps
       centring.schring)

  (defrecord (Procedure name instrs subprocs consts global-names local-names)))
