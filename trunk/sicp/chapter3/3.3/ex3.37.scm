#lang scheme

(require rackunit)

(require "constraints.scm")


(define (c+ x y)
    (let 
        ((z (make-connector)))
        (adder x y z)
        z
    )
)

(define (c* x y)
    (let 
        ((z (make-connector)))
        (multiplier x y z)
        z
    )
)

(define (c/ z y)
    (let 
        ((x (make-connector)))
        (multiplier x y z)
        x
    )
)

(define (cv c)
    (let 
        ((z (make-connector)))
        (constant c z)
        z
    )
)

(define (celsius-fahrenheit-converter x)
    (c+ (c* (c/ (cv 9) (cv 5)) x)
        (cv 32))
)


(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(set-value! C 25 'user)
(check-equal? (get-value F) 77) 

(set-value! C 25 'user)

(forget-value! C 'user)
(check-false (has-value? F)) 
(check-false (has-value? C)) 


(set-value! F 212 'user)
(check-equal? (get-value C) 100) 

