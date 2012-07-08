#lang scheme

(define (key tree)
    (car tree)
)

(define (entry tree)
    (cadr tree)
)

(define (left-branch tree)
    (caddr tree)
)

(define (right-branch tree)
    (cadddr tree)
)

(define (make-tree key entry left right)
    (list key entry left right)
)

(define a (make-tree 10 200 null null))
(= 10 (key a))
(= 200 (entry a))

(define b (make-tree 20 134 null null))
(define root (make-tree 15 555 a b))

(equal? a (left-branch root))
(equal? b (right-branch root))

(define (lookup k set)
    (cond
        ((null? set) false)
        ((= k (key set)) (entry set))
        ((< k (key set)) 
            (lookup k (left-branch set)))
        ((> k (key set)) 
            (lookup k (right-branch set)))
    )
)

(= 200 (lookup 10 root))
(= 200 (lookup 10 root))
(= 555 (lookup 15 root))
(not (lookup 34 root))
