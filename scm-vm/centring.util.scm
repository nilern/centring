(module centring.util
  *

  (import scheme chicken)
  (use persistent-hash-map
       vector-lib
       (only clojurian-syntax -> doto)
       (only anaphora aif)
       (only (srfi 13) string-index))

  (define (keyword->symbol kw)
    (-> kw keyword->string string->symbol))

  (define (mapv f ls)
    (define (mv ls n)
      (if (null? ls)
        (make-vector n)
        (doto (mv (cdr ls) (add1 n))
          (vector-set! n (f (car ls))))))
    (mv ls 0))

  (define (mapl f vec)
    (let ((res '()))
      (do ((i (sub1 (vector-length vec)) (sub1 i))) ((= i -1))
        (set! res (cons (f (vector-ref vec i)) res)))
      res))

  (define (map-merge-with f map1 map2)
    (define (merge k v map)
      (map-add map k (aif (map-ref map k) (f it v) v)))
    (map-reduce merge map1 map2))

  (define (zipmap ks vs)
    (let ((m (map->transient-map (persistent-map))))
      (vector-for-each (lambda (i k v) (map-add! m k v)) ks vs)
      (persist-map! m)))
    
  (define (ns-name sym)
    (let* ((symstr (symbol->string sym))
           (i (string-index symstr #\/)))
      (if (or (not i) (= i 0) (= i (sub1 (string-length symstr))))
        (values #f sym)
        (values (string->symbol (substring symstr 0 i))
                (string->symbol (substring symstr (add1 i)))))))

  (define (name sym)
    (receive (_ name) (ns-name sym) name))

  (define (ns sym)
    (receive (ns _) (ns-name sym) ns))

  (define-record-type stack
    (raw-stack stack start end buffer)
    stack?
    
    (start stack-start stack-start-set!)
    (end stack-end stack-end-set!)
    (buffer stack-buffer stack-buffer-set!))

  (define (subtype? t subt)
    (or (equal? t subt) (eq? t 'centring.lang/Any)))

  (define (stack-has-space? st)
    (< (stack-end st) (vector-length (stack-buffer st))))

  (define (stack-push! st v)
    (if (stack-has-space? st)
      (begin
        (vector-set! st (stack-end st) v)
        (stack-end-set! st (add1 (stack-start st))))
      (error "stack not finished!"))))
