#lang scheme 

(require racket/trace)

(define (element-of-set? x set)
    (cond
        ((null? set) false)
        ((equal? x (car set)) true)
        (else
            (element-of-set? x (cdr set)))
    )
)

(element-of-set? 1 '(2 3 4))
(element-of-set? 1 '(2 3 1))


(define (adjoint-set x set)
    (if (element-of-set? x set)
        set
        (cons x set)
    )
)

(adjoint-set 1 '(2 3 1))
(adjoint-set 4 '(2 3 1))

(define (intersection-set set1 set2)
    (cond
        ((or (null? set1) (null? set2)) null)
        ((element-of-set? (car set1) set2)
            (cons (car set1) (intersection-set (cdr set1) set2)))
        (else
            (intersection-set (cdr set1) set2))
    )
)

(intersection-set '(1 2 3) '(2 3 4))

