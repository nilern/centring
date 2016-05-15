(module centring.util
  *

  (import scheme chicken)
  (use persistent-hash-map
       vector-lib
       sequences
       (only clojurian-syntax -> doto)
       (only anaphora aif)
       (only (srfi 13) string-index))

  (define (keyword->symbol kw)
    (-> kw keyword->string string->symbol))

  (define (mapv f ls)
    (smap #() f ls))

  (define (mapl f vec)
    (smap '() f vec))

  (define (map-merge-with f map1 map2)
    (define (merge k v map)
      (map-add! map k (aif (map-ref map k) (f it v) v)))
    (persist-map! (map-reduce merge (map->transient-map map1) map2)))

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
