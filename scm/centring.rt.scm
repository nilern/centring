(module centring.rt
  *

  (import scheme chicken)
  (use (only extras read-file)
       (only files make-pathname)
       irregex)

  (define (read-ns full-path ns-name)
    (let recur ((path full-path))
      (if (pair? path)
        (let* ((ns-components (irregex-split #\. (symbol->string ns-name)))
               (filename
                (make-pathname
                 (car path) (foldl make-pathname "" ns-components) ".ctr")))
          (if (file-exists? filename)
            `(do ,@(read-file filename))
            (recur (cdr path))))
        (error "unable to locate ns with path" ns-name full-path)))))
