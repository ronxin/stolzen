#lang scheme


(define (filter items el)
    (cond 
        ((null? items) null)
        ((= (car items) el)
            (filter (cdr items) el)
        )
        (else 
            (cons (car items) (filter (cdr items) el))
        )
    )
)

(filter (list 1 2 3 4) 2)
(filter (list 1 2 3 4) 1)

; 2.32

; http://en.wikipedia.org/wiki/Powerset#Algorithms

(define (powerset s)
    (if (null? s)
        (list null)
        (let
            ((head (car s))
             (oldpowerset (powerset (cdr s))))
            (define newitems (map (lambda (subset) (cons head subset)) oldpowerset))
            (append oldpowerset newitems)
        )
    )
)

(powerset (list 1 2 3))