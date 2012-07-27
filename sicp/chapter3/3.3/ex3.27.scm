#lang scheme

(require "table.scm")


(define (memoize f)
    (let 
        ((table (make-table)))
    
        (lambda (x)
            (let 
                ((previously-computed-result (lookup x table)))
                (or 
                    previously-computed-result
                    (let 
                        ((result (f x)))
                        (insert! x result table)
                        result
                    )
                )
            )
        )
    )
)

(define memo-fib (memoize 
    (lambda (n)
        (cond 
            ((= n 0) 0)
            ((= n 1) 1)
            (else 
                (+ (memo-fib (- n 1)) (memo-fib (- n 2))))
        )
    )
))


(memo-fib 10)

; Q: Explain why memo-fib computes the nth Fibonacci number in a number of steps proportional to n. 
; A: Because it stores already computed elements in a table

; Q: Would the scheme still work if we had simply defined memo-fib to be (memoize fib)? 
; A: Not exactly. Would, if (set! fib (memoize fib))