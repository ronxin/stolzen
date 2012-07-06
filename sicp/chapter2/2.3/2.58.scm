#lang scheme

(require racket/trace)

(define (variable? expression)
    (symbol? expression)
)

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2))
)

(define (operation? expression symbol)
    (define (not-empty? seq)
        (not (null? seq))
    )
    (define (contains? seq symbol)
        (not-empty? (filter (lambda (x) (eq? x symbol)) seq))
    )
    (and 
        (pair? expression) 
        (contains? expression symbol))
)

(define (sum? expression)
    (operation? expression '+)
)

(define (first-if-singleton seq)
    (if (null? (cdr seq))
        (car seq)
        seq
    )
)

(define (cut-before symbol expression)
    (define (cut-before-inner expression)
        (if (or (null? expression) (eq? (car expression) symbol))
            null
            (cons (car expression) (cut-before-inner (cdr expression)))
        )
    )
    (first-if-singleton (cut-before-inner expression))
)

(= 1 (cut-before '+ '(1 + x * x)))
(equal? '(1 * 2) (cut-before '+ '(1 * 2 + x * x)))

(define (cut-after symbol expression)
    (define (cut-after-inner expression)
        (cond 
            ((null? expression) null)
            ((eq? symbol (car expression)) (cdr expression))
            (else 
                (cut-after-inner (cdr expression)))
        )
    )
    (first-if-singleton (cut-after-inner expression))
)

(equal? '(x * x) (cut-after '+ '(1 + x * x)))

(define (addend expression)
    (cut-before '+ expression)
)

(define (augend expression)
    (cut-after '+ expression)
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

(define (product? expression)
    (operation? expression '*)
)

(define (multiplier expression)
    (cut-before '* expression)
)

(define (multiplicand expression)
    (cut-after '* expression)
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

(define (exponentiation? expression)
    (and (pair? expression) (eq? (car expression) 'exp))
)

(define (base expression)
    (cadr expression)
)

(define (exponent expression)
    (caddr expression)
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
            (error "unknown expression type -- DERIV" exp))
    )
)

(trace deriv)

(=       2        (deriv '(x + x) 'x))
(=       1        (deriv '(x + 3) 'x))
(=       1        (deriv '(x + y) 'x))
(=       1        (deriv '(x + y) 'y))
(eq?     'x       (deriv '(x * y) 'y))
(=       4        (deriv '(x + (3 * (x + (y + 2)))) 'x))
(=       4        (deriv '(x + 3 * (x + y + 2)) 'x))
(=       8        (deriv '(x * 3 + 5 * (x + y + 2)) 'x))
(equal? '(y + 5)  (deriv '(x * y + 5 * (x + y + 2)) 'x))