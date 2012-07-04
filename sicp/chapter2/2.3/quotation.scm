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


(display "(eq? 'apple 'apple) ")
(eq? 'apple 'apple)

(display "(eq? 'apple1 'apple2) ")
(eq? 'apple1 'apple2)

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



; 2.53

(newline)
(display "2.53: ")
(newline)

(list 'a 'b 'c) ; (a b c)

(list (list 'george)) ; ((george))
(cdr '((x1 x2) (y1 y2))) ; ((y1 y2))

(cadr '((x1 x2) (y1 y2))) ; (y1 y2)
(pair? (car '(a short list))) ; false
(memq 'red '((red shoes) (blue socks))) ; false

(memq 'red '(red shoes blue socks)) ; whole list

; 2.54

(newline)
(display "2.54: ")
(newline)

(define (equal? list1 list2)
    (if (and (null? list1) (null? list2) )
        true
        (and (eq? (car list1) (car list2))
             (equal? (cdr list1) (cdr list2)))
    )
)

; equal? exists as a primitive
(equal? '(this is a list) '(this is a list)) ; true
(equal? '(this is a list) '(this (is a) list)) ; false


; 2.55

; http://wiki.drewhess.com/wiki/SICP_exercise_2.55
(car ''abracadabra)
; same as (car (quote (quote abracadabra)))

