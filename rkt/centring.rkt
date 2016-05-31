#lang racket
(require redex)

(define-language centring
  (AST PVal
       (fn (Id ...) AST)
       Id
       (AST ...))
  (Id variable-not-otherwise-mentioned)
  (Val number
       boolean
       (fn (Id ...) AST))
  (PVal number
        boolean))

(define-extended-language centring-CESK
  centring
  (State (Ctrl Env Store Cont))
  (Ctrl (AST Env))
  (Env ((Id Addr) ...))
  (Store ((Addr Ctrl) ...))
  (Cont Halt
        (App (Val ...) (Ctrl ...) Cont))
  (Addr variable-not-otherwise-mentioned))

(define (inject prog)
  (term ((,prog ()) () ())))

(define CESK
  (reduction-relation
   centring-CESK
   (--> ((Id Env) Store Cont)
        (((store-lookup Store (env-lookup Env Id)) Env) Store Cont)
        id)
   (--> ((Val_m Env) Store (App (Val_m-1 ... (λ (Id ...) AST)) () Cont))
        ())))

(define-metafunction centring-CESK
  [(extend-env ((Id Store) ...) Id_1 Store_1)
   ((Id_1 Store_1) (Id Store) ...)])

(define-metafunction centring-CESK
  [(extend-store ((Store Ctrl) ...) Store_1 Ctrl_1)
   ((Store_1 Ctrl_1) (Store Ctrl) ...)])

(define-metafunction centring-CESK
  [(env-lookup ((Id_1 Addr_1) (Id Addr) ...) Id_1)
   Addr_1]
  [(env-lookup ((Id_2 Addr_2) (Id Addr) ...) Id_1)
   (env-lookup ((Id Addr) ...) Id_1)])

(define-metafunction centring-CESK
  [(store-lookup ((Addr_1 Ctrl_1) (Addr Ctrl) ...) Addr_1)
   Ctrl_1]
  [(store-lookup ((Addr_2 Ctrl_2) (Addr Ctrl) ...) Addr_1)
   (store-lookup ((Addr Ctrl) ...) Addr_1)])
