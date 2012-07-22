#lang scheme

(require rackunit)

(define (make-accumulator sum)
    (lambda (audent) 
        (begin 
            (set! sum (+ sum audent))
            sum
        )
    )
)

(define A (make-accumulator 5))

(check-equal? 
    (A 10)
    15)

(check-equal? 
    (A 10)
    25)