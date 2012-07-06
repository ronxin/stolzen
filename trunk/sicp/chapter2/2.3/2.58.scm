#lang scheme

(define (variable? e)
    (symbol? e)
)

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2))
)

(define (not-empty? seq)
    (not (null? seq))
)

(define (contains? seq symbol)
    (not-empty? (filter (lambda (x) (eq? x symbol)) seq))
)

(define (expression? e symbol)
    (and 
        (pair? e) 
        (contains? e symbol))
)

(define (sum? e)
    (expression? e '+)
)


(define (addend e)
    (car (split e '+))
)

(define (augend e)
    (cadr (split e '+))
)

(define (=number? exp num)
    (and (number? exp) (= exp num))
)

(define (make-sum addend augend)
    (cond
        ((=number? addend 0) augend)
        ((=number? augend 0) addend)
        ((and (number? addend) (number? augend)) (+ addend augend))
        (else
            (list addend '+ augend))
    )
)

(define (product? e)
    (and (pair? e) (eq? (cadr e) '*))
)

(define (multiplier e)
    (car (split e '*))
)

(define (multiplicand e)
    (cadr (split e '*))
)

(define (make-product m1 m2)
    (cond
        ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else
            (list m1 '* m2))
    )
)

(define (exponentiation? e)
    (and (pair? e) (eq? (car e) 'exp))
)

(define (operands seq)
    (cdr seq)
)

(define (base e)
    (cadr e)
)

(define (exponent e)
    (caddr e)
)

(define (make-exponentiation base exponent)
   (cond
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (exp base exponent))
        (else
            (list 'exp base exponent))
    )
)

(define (deriv exp var)
    (cond 
        ((number? exp) 0)
        ((variable? exp)
            (if (same-variable? exp var) 1 0))
        ((sum? exp) 
            (make-sum (deriv (addend exp) var)
                      (deriv (augend exp) var)))
        ((product? exp)
            (make-sum
                (make-product (multiplier exp)
                              (deriv (multiplicand exp) var))
                (make-product (deriv (multiplier exp) var)
                              (multiplicand exp))
            )
        )
        ((exponentiation? exp)
            (make-product 
                (exponent exp)
                (make-product 
                    (make-exponentiation (base exp) (- (exponent exp) 1))
                    (deriv (base exp) var)
                )
            )
        )
        (else 
            (error "unknown expression tupe -- DERIV" exp))
    )
)

; (deriv '(x + x) 'x)
; (deriv '(x + 3) 'x)
; (deriv '(x + y) 'x)
; (deriv '(x + y) 'y)
; (deriv '(x * y) 'y)
; (deriv '(x + (3 * (x + (y + 2)))) 'x)
; (deriv '(x + 3 * (x + y + 2)) 'x)
; (deriv '(x * 3 + (x + y + 2)) 'x)