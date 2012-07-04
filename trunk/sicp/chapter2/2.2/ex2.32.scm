#lang scheme


(define (remove-all el sequence)
    (cond 
        ((null? sequence) null)
        ((= (car sequence) el)
            (remove-all el (cdr sequence)))
        (else 
            (cons (car sequence) (remove-all el (cdr sequence))))
    )
)

(remove-all 2 (list 1 2 3 4 2))
(remove-all 1 (list 1 2 3 4 1))

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