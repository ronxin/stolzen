#lang scheme

(define (fib n)
    (fib-iter 0 1 n)
)

(define (fib-iter a b n)
    (if (= n 0)
        b
        (fib-iter (+ a b) a (- n 1))
    )
)

(define res (fib 6))
(print res) (newline)