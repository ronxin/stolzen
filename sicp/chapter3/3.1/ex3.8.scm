#lang scheme 

(require rackunit)

(define var-set false)

(define (f p)
    (if var-set
        0 
        (begin (set! var-set true) p)
    )
)


(+ (f 0) (f 1))
(+ (f 1) (f 0))


