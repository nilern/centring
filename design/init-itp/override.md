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

### High Level

    (defn max-method-indices
      (#(method-cmp n) #t
       (defn cmp-all
         (#(mmis i) (empty? mmis) ; nothing to compare i against?
          #{i})                   ; put i in the set.
         (#(mmis i) #t
          (defn step
            (#(ord mmi) (= (method-cmp i mmi) ord) ; still the same result?
             ord)                                  ; keep it.
            (#(_ _) #t                             ; results disagree?
             (Unknown)))                           ; inconclusive.
          (match (foldl step (method-cmp i (first mmis)) (rest mmis))
            ((Left) #t mmis)             ; ignore i, it can't win
            ((Unknown) #t (conj mmis i)) ; put i in the set
            ((Right) #t #{i}))))         ; i overrode the whole set, discard those
       (foldl cmp-all #{} (range n))))
