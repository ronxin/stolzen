#lang scheme 

(define (element-of-set? x set)
    (cond
        ((null? set) false)
        ((equal? x (car set)) true)
        (else
            (element-of-set? x (cdr set)))
    )
)

; 2.59
(define (difference set1 set2)
    (cond
        ((null? set1) null)
        ((element-of-set? (car set1) set2)
            (difference (cdr set1) set2))
        (else
            (cons (car set1) (difference (cdr set1) set2)))
    )
)

(difference '(1 2 3) '(2 3 4))

(define (union-set set1 set2)
    (append set1 (difference set2 set1))
)

(union-set '(1 2 3) '(2 3 4)) ; 1 2 3 4
