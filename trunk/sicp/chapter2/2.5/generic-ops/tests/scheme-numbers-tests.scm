#lang scheme

(require rackunit)
(require rackunit/text-ui)

(require "../tags.scm")
(require "../generic-operations.scm")
(require "../scheme-numbers.scm")

(require "../install-modules.scm")

(define scheme-numbers-tags-suite (test-suite
    "Sheme numbers tags tests"

    (test-case
        "Scheme-number won't be attached to a number"
        (check-equal? (attach-tag 'scheme-number 2) 2)
    )

    (test-case
        "Type of number is 'scheme-number"
        (check-equal? (type-tag 1) 'scheme-number)
    )

    (test-case
        "Contents of a number is the number itself"
        (check-equal? (contents 2) 2)
    )
    
))

(define scheme-numbers-ops-suite (test-suite
    "Sheme numbers operations tests"

    (test-case
        "Add for two tagged scheme-numbers"
        (check-equal?   
            (add (make-scheme-number 3) (make-scheme-number 1))
                 (make-scheme-number 4))
    )

    (test-case
        "Sud for two tagged scheme-numbers"
        (check-equal?   
            (sub (make-scheme-number 3) (make-scheme-number 1))
                 (make-scheme-number 2))
    )

    (test-case
        "Mul for two tagged scheme-numbers"
        (check-equal?   
            (mul (make-scheme-number 3) (make-scheme-number 2))
                 (make-scheme-number 6))
    )

    (test-case
        "Div for two tagged scheme-numbers"
        (check-equal?   
            (div (make-scheme-number 3) (make-scheme-number 1))
                 (make-scheme-number 3))
    )

    (check-equal? 
        (add 1 2)
        (make-scheme-number 3) 
        "Add for two usual numbers works as for tagged numbers")

    (test-case
        "=zero? for zero"
        (check-true  
            (=zero? 0))
    )

    (test-case
        "=zero? for non-zero"
        (check-false  
            (=zero? 1))
    )

    (test-case
        "negate for positive"
        (check-equal?
            (negate 1)
            -1)
    )

    (test-case
        "negate for negative"
        (check-equal?
            (negate -1)
            1)
    )

    (test-case
        "negate for zero"
        (check-equal?
            (negate 0)
            0)
    )
))

(run-tests (test-suite 
    "Sheme numbers operations"
    scheme-numbers-tags-suite
    scheme-numbers-ops-suite
))