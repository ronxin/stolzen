#lang scheme

(define (variable? e)
    (symbol? e)
)

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2))
)

(define (sum? e)
    (and (pair? e) (eq? (car e) '+))
)

; first term of addition
(define (addend e)
    (cadr e)
)

; second term of addition
(define (augend e)
    (caddr e)
)

(define (make-sum a1 a2)
    (list '+ a1 a2)
)

(define (product? e)
    (and (pair? e) (eq? (car e) '*))
)

; first term of multiplication
(define (multiplier e)
    (cadr e)
)

; second term of multiplication
(define (multiplicand e)
    (caddr e)
)

(define (make-product m1 m2)
    (list '* m1 m2)
)

(define (deriv exp var)
    (cond 
        ((number? exp) 0)
        ((variable? exp)
            (if (same-variable? exp var) 1 0))
        ((sum? exp) 
            (make-sum 
                (deriv (addend exp) var)
                (deriv (augend exp) var))
        )
        ((product? exp)
            (make-sum
                (make-product (multiplier exp)
                              (deriv (multiplicand exp) var))
                (make-product (deriv (multiplier exp) var)
                              (multiplicand exp))
            )
        )
        (else 
            (error "unknown expression tupe -- DERIV" exp))
    )
)


(deriv '(+ x 3) 'x)
(deriv '(+ x y) 'x)
(deriv '(+ x y) 'y)
(deriv '(* (* x y) (+ x 3)) 'x)
