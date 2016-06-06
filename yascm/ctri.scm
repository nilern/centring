(import (centring ast)
        (coops))

(define (main argv)
  (write (read (open-input-string (car argv)))))

#+compiling
(main (command-line-arguments))
