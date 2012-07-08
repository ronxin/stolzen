#lang scheme 

(define (difference set1 set2)
    (cond
        ((null? set1) null)
        ((element-of-set? (car set1) set2)
            (difference (cdr set1) set2))
        (else
            (cons (car set1) (difference (cdr set1) set2)))
    )
)


(define (element-of-set? x set)
    (cond
        ((null? set) false)
        ((equal? x (car set)) true)
        (else
            (element-of-set? x (cdr set)))
    )
)

(define (adjoint-set x set)
    (cons x set)
)

(define (union-set set1 set2)
    (append set1 set2)
)

(define (intersection-set set1 set2)
    (define intersection-left (difference set1 (difference set1 set2)))
    (define intersection-right (difference set2 (difference set2 set1)))
    (union-set intersection-left intersection-right)
)

(define a '(1 2 3 1))
(define b '(2 3 3 3 4 2))

(intersection-set a b)

; Q: How does the efficiency of each compare with the corresponding procedure
; for the non-duplicate representation? A: union-set and adjoint-set are more
; efficientbut not intersection-set

; Q: Are there applications for which you would use this representation in
; preference to the non-duplicate one? A: There are. In cases where there may
; be more then one element in set (bags, multisets)
