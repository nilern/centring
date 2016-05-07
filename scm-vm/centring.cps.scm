(module centring.cps
  *

  (import scheme chicken)
  (use typed-records)

  (define-record Block
    (label : symbol)
    formals ; ::vector<symbol U Splat<symbol>>
    types   ; ::vector<symbol U Splat<symbol>>
    body)   ; ::CPS
  (define-record Fix
    (defns : (vector-of (struct Block)))
    body) ; ::CPS
  (define-record If
    cond   ; ::fetch-descr
    tcont  ; ::CPS
    fcont) ; ::CPS
  (define-record Primop
    (op : symbol)
    args  ; ::vector<fetch-descr U Splat>
    (res : (or (list symbol) (list)))
    cont) ; ::CPS
  (define-record App
    callee ; ::fetch-descr
    args)  ; ::vector<fetch-descr U Splat>

  (define-record Splat
    fd) ; ::fetch-descr

  ;; fetch-descr
  (define-record Local
    (name : symbol))
  (define-record Clover
    (index : fixnum))
  (define-record Const
    val) ; ::dvalue
  (define-record Global
    (name : symbol))
  (define-record Label
    (name : symbol)))
