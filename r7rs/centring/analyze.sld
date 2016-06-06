(define-library (centring analyze)
  (export)

  (import (scheme base))

  (begin
    (define-record-type <AMod>
      (AMod decls)
      AMod?
      (decls AMod-decls))
    
    (define-record-type <AFn>
      (AFn formals types body)
      AFn?
      (formals AFn-formals)
      (types AFn-types)
      (body AFn-body))
    
    (define-record-type <AFix>
      (AFix bindings body)
      AFix?
      (bindings AFix-bindings)
      (body AFix-body))
    
    (define-record-type <APrimop>
      (APrimop op args)
      APrimop?
      (op APrimop-op)
      (args APrimop-args))
    
    (define-record-type <ASplat>
      (ASplat val)
      ASplat?
      (val ASplat-val))
    
    (define-record-type <AGlobal>
      (AGlobal ns name)
      AGlobal?
      (ns AGlobal-ns)
      (name AGlobal-name))
    
    (define-record-type <ALocal>
      (ALocal name)
      ALocal?
      (name ALocal-name))
    
    (define-record-type <AConst>
      (AConst val)
      AConst?
      (val AConst-val))))
