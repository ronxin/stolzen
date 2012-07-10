#lang scheme

(require racket/trace)

(define (variable? e)
    (symbol? e)
)

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2))
)

(define (=number? exp num)
    (and (number? exp) (= exp num))
)

(define (operator exp)
    (car exp)
)

(define (operands exp)
    (cdr exp)
)

(define (deriv exp var)
    (cond
        ((number? exp) 0)
        ((variable? exp) 
            (if (same-variable? exp var) 1 0))
        (else
            ((get 'deriv (operator exp)) (operands exp) var))
    )
)


(define (put op type item)
    true
)

; a
; because number won't contain a type-tag
; same is with same-variable?


; b

(define (make-sum addend augend)
    (cond
        ((=number? addend 0) augend)
        ((=number? augend 0) addend)
        ((and (number? addend) (number? augend)) (+ addend augend))
        (else
            (list '+ addend augend))
    )
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


(define (add-plus-mult-package)
    (define (addend e)
        (car e)
    )

    (define (augend e)
        (cadr e)
    )

    (define (make-sum-deriv operands var)
        (make-sum (deriv (addend operands) var)
                  (deriv (augend operands) var))
    )

    (put 'deriv '+ make-sum-deriv)


    (define (multiplier e)
        (car e)
    )

    (define (multiplicand e)
        (cadr e)
    )

    (define (make-product-deriv operands var)
        (make-sum
            (make-product (multiplier operands)
                          (deriv (multiplicand operands) var))
            (make-product (deriv (multiplier operands) var)
                          (multiplicand operands))
        )
    )

    (put 'deriv '* make-product-deriv)


    ; for checking
    (define (get-plus-mult op type)
        (if (eq? op 'deriv)
            (cond
                ((eq? type '+) make-sum-deriv)
                ((eq? type '*) make-product-deriv)
                (else false)
            )
            false
        )
    )

    get-plus-mult
)

; c
(define (add-exp-package)
    (define (base e)
        (car e)
    )

    (define (exponent e)
        (cadr e)
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

    (define (exponent-deriv operands var)
        (make-product 
            (exponent operands)
            (make-product 
                (make-exponentiation (base operands) (- (exponent operands) 1))
                (deriv (base operands) var)
            )
        )
    )

    ; for checking
    (define (get-exp op type)
        (if (and (eq? op 'deriv) (eq? type 'exp)) 
            exponent-deriv
            false
        )
    )

    get-exp
)

; d

; if change to ((get (operator exp) 'deriv) (operands) var), then you'll have to
; change only the get procedure



; get for testing

(define packages (list (add-plus-mult-package) (add-exp-package)))

(define (get op type)
    (define (get-inner gets-list)
        (if (null? gets-list)
            false
            (let
                ((res ((car gets-list) op type)))
                (if res
                    res
                    (get-inner (cdr gets-list))
                )
            )
        )
    )

    (get-inner packages)
)

(get 'deriv '+)
(get 'deriv '*)
(get 'deriv 'exp)

(= 2 (deriv '(+ x x) 'x))
(= 1 (deriv '(+ x 3) 'x))
(= 1 (deriv '(+ x y) 'x))
(= 1 (deriv '(+ x y) 'y))
(eq? 'y (deriv '(* x y) 'x))

(equal? '(* 2 x) (deriv '(exp x 2) 'x))
