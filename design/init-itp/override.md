# Resolving Overrides

## dispatch.ml

1. Generate an override matrix : [[Option<AST>]]
    * m1 == m2
      => `overrides[i1, i2] = None`
    * Can statically determine that m1 overrides m2
      => `overrides[i1, i2] = Some(Const(True))`
    * m1 overrides m2 if a guard is true at runtime
      => `overrides[i1, i2] = Some(guardExpr)`
    * Can statically determine that m2 overrides m1
      => `overrides[i1, i2] = Some(Const(False))`
    * What about the case when we can determine nothing? Is it `Const(False)`?
2. Generate something like the following:

        ($def mmis (max-method-indices
                    (fn
                      (#(i1 i2) #t
                       (%switch i1
                         (%switch i2
                           ...)
                         ...)))
                    ,l '() 0))
        (%brf (%identical? (%type (%cdr mmis)) List.Empty)
          (%switch (%car mmis) ($closure ,env ,body) ...)
          (%err 'AmbiguousMethodError ...))

    where

    1. the first ellipses are filled in based on the override matrix
    2. `l` is the number of methods whose overrides are being resolved
    3. The third ellipsis is filled in with the method bodies
    4. The fourth ellipsis should have some useful info like method name and
       source position

(Seems that generating the intermediate matrix is not compulsory...)

## bootstrap0.ctr

    (def max-method-indices
      (fn max-method-indices
        (#(>method method-count ais i) #t
         (def loop
           (fn loop
            (#(ais i) #t
             (if (< i method-count)
               (if (empty? ais)
                 (loop (prepend i ais) (inc i))
                 (do
                   (def ai (first ais))
                   (loop
                     (let/cc k
                       (def cmp-all
                         (fn cmp-all
                           (#(ord ais*) #t
                            (if (empty? ais*) ; no acc-methods left?
                              ord             ; return result.
                              (cmp-all
                                (%switch ord
                                 (if (>method (first ais*) i)
                                   1                    ; m overridden by yet another!
                                   (k (prepend i ais))) ; became inconclusive...
                                 (if (>method (first ais*) am)
                                   2                     ; m overrides yet another!
                                   (k (prepend i ais)))) ; became inconclusive...
                                (rest ais*))))))
                         (%switch (cmp-all
                                    (if (>method ai i)
                                      (if (>method i ai)
                                        (k (prepend i ais)) ; <=> override => inconclusive
                                        1)                  ; am > m => am
                                      (if (>method i ai)
                                        2                     ; m > am => m
                                        (k (prepend i ais)))) ; no override => inconclusive
                                    (rest ais))
                         (prepend i ais) ; inconclusive => add m to ams
                         ais             ; m was overridden by every method in ams => discard m
                         #{m}))          ; m overrode every method in ams => discard ams
                     (inc i))))
               ais)))))
        (loop ais i)))
