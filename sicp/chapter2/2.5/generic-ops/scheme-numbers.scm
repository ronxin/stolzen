#lang scheme 

(require "container.scm")
(require "tags.scm")

(define (install-scheme-number-package)

    (put 'add '(scheme-number scheme-number)
        (lambda (x y) (+ x y))
    )

    (put 'sub '(scheme-number scheme-number)
        (lambda (x y) (- x y))
    )

    (put 'negate '(scheme-number)
        (lambda (x) (- x))
    )

    (put 'mul '(scheme-number scheme-number)
        (lambda (x y) (* x y))
    )

    (put 'div '(scheme-number scheme-number)
        (lambda (x y) (/ x y))
    )

    (put 'cos '(scheme-number)
        (lambda (x) (cos x))
    )

    (put 'sin '(scheme-number)
        (lambda (x) (sin x))
    )

    (put 'sqrt '(scheme-number)
        (lambda (x) (sqrt x))
    )

    (put 'atan '(scheme-number scheme-number)
        (lambda (y x) (atan y x))
    )

    (put 'make 'scheme-number
        (lambda (x) x)
    )

    (put '=zero? '(scheme-number)
        (lambda (x) (= x 0))
    )

    (void)
)

(define (make-scheme-number n)
    ((get 'make 'scheme-number) n)
)

(define (scheme-number? n)
    (or (number? n) (eq? (type-tag n) 'scheme-number))
)

(define (tag-if-needed v)
    (if (number? v)
        (make-scheme-number v)
        v
    )
)

(provide (all-defined-out))