(module centring.value
  *

  (import scheme chicken)
  (use (only (srfi 13) string-index)

       centring.schring)
    
  (define (ns-name sym)
    (let* ((symstr (symbol->string sym))
           (i (string-index symstr #\/)))
      (if (or (not i) (= i 0) (= i (sub1 (string-length symstr))))
        (values (None) sym)
        (values (Some (string->symbol (substring symstr 0 i)))
                (string->symbol (substring symstr (add1 i)))))))

  (define (name sym)
    (receive (_ name) (ns-name sym) name))

  (define (ns sym)
    (receive (ns _) (ns-name sym) ns))

  (define-record Proc
    name
    instrs
    consts
    global-names
    local-names))
