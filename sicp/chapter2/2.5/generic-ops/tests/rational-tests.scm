#lang racket/base

(require rackunit)
(require rackunit/text-ui)

(require "../container.scm")
(require "../generic-operations.scm")
(require "../rational-numbers.scm")

(require "../install-modules.scm")


(define rational-tests (test-suite
    "Rationals test suite"

    (test-case
        "Numerator of rational number is exported"
        (check-equal? 
            ((get 'numer 'rational) (make-rational 1 2))
            1)
    )

    (test-case
        "Denomenator of rational number is exported"
        (check-equal? 
            ((get 'denom 'rational) (make-rational 1 2))
            2)
    )   

    (check-equal?  
        (add (make-rational 3 2) (make-rational 2 3))
        (make-rational 13 6)
        "Add for rational numbers")

    (check-equal?  
        (sub (make-rational 3 2) (make-rational 2 3))
        (make-rational 5 6)
        "Div for rational numbers")

    (check-equal?  
        (mul (make-rational 3 2) (make-rational 2 3))
        1
        "Mul for rational numbers")

    (check-equal? 
        (div (make-rational 3 2) (make-rational 2 3))
        (make-rational 9 4)
        "Div for rational numbers")

    (test-case
        "=zero? for zero"
        (check-true  
            (=zero? (make-rational 0 1)))
    )

    (test-case
        "=zero? for non-zero"
        (check-false 
            (=zero? (make-rational 1 1)))
    )

    (test-case
        "negate"
        (check-equal? 
            (negate (make-rational 1 1))
            (make-rational -1 1))
    )
))

(run-tests rational-tests)