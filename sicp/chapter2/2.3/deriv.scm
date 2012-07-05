#lang scheme

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))
    )
)

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

(define (=number? exp num)
    (and (number? exp) (= exp num))
)

; (define (make-sum a1 a2)
;     (cond
;         ((=number? a1 0) a2)
;         ((=number? a2 0) a1)
;         ((and (number? a1) (number? a2)) (+ a1 a2))
;         (else
;             (list '+ a1 a2))
;     )
; )


(define (make-sum seq)
    (display seq) (newline)
    (define (not-number? n)
        (not (number? n))
    )
    (define (make-sum-inner seq)
        (cons 
            (accumulate + 0 (filter number? seq))
            (filter not-number? seq)
        )
    )
    (let ((res (make-sum-inner (cdr seq))))
        (cond
            ((null? (cdr res)) (car res))
            (else
                (cons '+ res))
        )
    )
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
    (cond
        ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else
            (list '* m1 m2))
    )
)

; 2.56 

(define (exponentiation? e)
    (and (pair? e) (eq? (car e) 'exp))
)

(define (tail seq)
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
        ; ??
        ((and (number? base) (number? exponent)) (exp base exponent))
        (else
            (list 'exp base exponent))
    )
)

(define (deriv exp var)
    (display exp) (newline)
    (define (deriv-inner param)
        (deriv exp var)
    )
    (cond 
        ((number? exp) 0)
        ((variable? exp)
            (if (same-variable? exp var) 1 0))
        ((sum? exp) 
            ; (make-sum 
            ;     (deriv (addend exp) var)
            ;     (deriv (augend exp) var))
            (make-sum (map deriv-inner (tail exp)))
        )
        ((product? exp)
            (make-sum
                (make-product (multiplier exp)
                              (deriv (multiplicand exp) var))
                (make-product (deriv (multiplier exp) var)
                              (multiplicand exp))
            )
        )
        ; 2.56 
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


(deriv '(+ x 3) 'x)
(deriv '(+ x y) 'x)
(deriv '(+ x y) 'y)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

(deriv '(exp x 2) 'x)
(deriv '(exp x 3) 'x)
(deriv '(exp (* x 10) 3) 'x)

(deriv '(* x y (+ x 3)) 'x)
