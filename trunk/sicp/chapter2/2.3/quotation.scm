#lang scheme

(define a 1)
(define b 2)

(list a b)
(list 'a 'b)
(list 'a b)

; Quotation also allows us to type in compound objects, using the conventional
; printed representation for lists

(car '(a b c))
(cdr '(a b c))


; empty list which in racket is null, not nil

(define nil '())
nil


(define (memq item seq)
    (cond 
        ((null? seq) false)
        ((eq? item (car seq)) seq)
        (else 
            (memq item (cdr seq)))
    )
)

(memq 'apple '(pear banana prune))
(memq 'apple '(x (apple sauce) y apple pear))