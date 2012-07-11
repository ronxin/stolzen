#lang scheme

(define ns (make-base-namespace))
(apply (eval '+ ns) '(1 2 3))

(define (foo . params)
    (display "bar")(newline)
)

(apply (eval 'foo) '(1 2 3))
