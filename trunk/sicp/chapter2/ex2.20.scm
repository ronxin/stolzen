#lang scheme

; 2.20

(define (println . vars)
    (define (println-in items)
        (cond
            ((null? items) (newline))
            (else 
                (display (car items))
                (display " ")
                (println-in (cdr items)))
        )
    )
    (println-in vars)
)

(println "a" 1 2 3)

;

(define (same-parity . items)
    (define (filter predicate items)
        (cond
            ((null? items) null)
            ((predicate (car items))
                (cons (car items) (filter predicate (cdr items))))
            (else
                (filter predicate (cdr items)))
        )
    )
    (if (odd? (car items))
        (filter odd? items)
        (filter even? items)
    )
)

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)