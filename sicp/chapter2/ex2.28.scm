#lang scheme

; 2.28

(define (fringe tree)
    (cond
        ((null? tree) null)
        ((not (pair? tree)) 
            (list tree))
        (else 
            (append (fringe (car tree)) (fringe (cdr tree))))
    )
)

(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))


(define (fringe2 tree)
    (define (fringe-iter tree result)
        tree
    )
    (fringe-iter tree null)
)

(fringe2 x)
(fringe2 (list x x))
