#lang scheme

(define ns (make-base-namespace))
(apply (eval '+ ns) '(1 2 3))

(define (foo . params)
    (display "bar")(newline)
)
; http://www.ccs.neu.edu/home/ryanc/macro-patterns/error_unbound-in-transformer-env.html
(apply (eval 'foo) '(1 2 3))
