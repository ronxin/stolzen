#lang scheme

(define (paired? seq)
    true
)

(define input '(1 2 3))
(if (paired? input)
    (map (lambda (x) (* x x x)) input)
    (foldr + 1 input)
)