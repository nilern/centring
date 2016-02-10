(include "bootstrap.scm")
(import centring.bootstrap)

(def (make-prepend front back)
  (cond 
    ((not (pair? front))                 (list 'cons (qq-expand front) back))
    ((eq? (car front) 'unquote)          (list 'cons (cadr front) back))
    ((eq? (car front) 'unquote-splicing) (list 'concat (cadr front) back))
    (else                                (list 'cons (qq-expand front) back))))

(def (qq-expand-list form)
  (cond
    ((null? (cdr form)) (make-prepend (car form) (list 'quote '())))
    ((pair? (cdr form))
     (case (cadr form)
       ((unquote)          (make-prepend (car form) (caddr form)))
       ((unquote-splicing) (error "can't splice tail."))
       (else               (make-prepend (car form) (qq-expand-list (cdr form))))))
    (else               (make-prepend (car form) (list 'quote (cdr form))))))

(def (qq-expand form)
  (if (not (pair? form))
    (list 'quote form)
    (case (car form)
      ((unquote)          (cadr form))
      ((unquote-splicing) (error "quasiquote cannot splice into its surroundings."))
      (else               (qq-expand-list form)))))
