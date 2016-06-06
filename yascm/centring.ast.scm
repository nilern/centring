(define-library (centring ast)
  (export <ast>
          <global> <clover> <local> <const>
          .ann .res-ns .ns .name .index .val
          <fn> <primop> <fix> <do>
          .formals .body .op .args .bindings .stmts)

  (import (scheme base)
          (coops))

  (begin
    ;; Superclass of all AST nodes:
    (define-class <ast> ()
      ((ann accessor: .ann)))

    ;; Trivial Expressions:
    (define-class <global> (<ast>)
      ((res-ns accessor: .res-ns)
       (ns accessor: .ns)
       (name accessor: .name)))
    (define-class <clover> (<ast>)
      ((index accessor: .index)))
    (define-class <local> (<ast>)
      ((name accessor: .name)))
    (define-class <const> (<ast>)
      ((val accessor: .val)))

    ;; Nontrivial Expressions:
    (define-class <fn> (<ast>)
      ((formals accessor: .formals)
       (body accessor: .body)))
    (define-class <primop> (<ast>)
      ((op accessor: .op)
       (args accessor: .args)))
    (define-class <fix> (<ast>)
      ((bindings accessor: .bindings)
       (body accessor: .body)))
    (define-class <do> (<ast>)
      ((stmts accessor: .stmts)))))

