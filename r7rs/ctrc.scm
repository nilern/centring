(import (scheme base)
        (scheme cxr)
        (scheme read)
        (scheme write)
        
        (centring expand)
        (only (centring util) comp))

(define (main argv)
  (let* ((read-string (comp read open-input-string)))
    (write (expand-all (read-string (caddr argv))))
    0))
