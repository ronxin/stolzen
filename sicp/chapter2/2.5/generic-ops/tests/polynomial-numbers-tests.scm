#lang scheme


(require rackunit)
(require rackunit/text-ui)

(require "../generic-operations.scm")
(require "../polynomial-numbers.scm")
(require "../install-modules.scm")


(define polynomial-tests (test-suite
    "Polynomials test suite"

    (test-case
        "Make polynomial"
        (check-equal? 
            (make-polynomial 'x '((100 2) (2 4) (0 2)))
            '(polynomial (x (100 2) (2 4) (0 2))))
    )

    (test-case
        "Check add for polynomials"
        (check-equal? 
            (add (make-polynomial 'x '((100 1) (2 2) (0 1))) 
                 (make-polynomial 'x '((100 2) (2 4) (0 3)))) 
            (make-polynomial 'x '((100 3) (2 6) (0 4))))
    )

    (test-case
        "Check mul for polynomials"
        (check-equal? 
            (mul (make-polynomial 'x '((2 10) (1 5)))
                 (make-polynomial 'x '((1 20) (0 10)))) 
            (make-polynomial 'x '((3 200) (2 200) (1 50))))
    )

    (test-case
        "All? procedure for =zero?"
        (check-true 
            (all? null? '(() () ())))
    )

    (test-case
        "All? procedure for =zero?"
        (check-true 
            (all? null? '()))
    )

    (test-case
        "All? procedure for =zero?"
        (check-false
            (all? null? '(() (1) ())))
    )

    (test-case
        "=zero? for empty term list"
        (check-true
            (=zero? (make-polynomial 'x '())))
    ) 

    (test-case
        "=zero? for term list with one zero element"
        (check-true
            (=zero? (make-polynomial 'x '((3 0)))))
    ) 

    (test-case
        "=zero? for term list with two zero elements"
        (check-true
            (=zero? (make-polynomial 'x '((3 0) (2 0)))))
    ) 

    (test-case
        "=zero? for term with zero polynom"
        (check-true
            (=zero? 
                (make-polynomial 'x 
                    (list (list 3 (make-polynomial 'y '((3 0))))))
            )
        )
    ) 

    (test-case
        "Negate for polynomials"
        (check-equal? 
            (negate (make-polynomial 'x '((2 10) (1 5)))) 
            (make-polynomial 'x '((2 -10) (1 -5))))
    )


    (test-case
        "Negate for polynomials"
        (check-equal? 
            (sub (make-polynomial 'x '((2 10) (1 5)))
                 (make-polynomial 'x '((3 20) (2 15))))
            (add (make-polynomial 'x '((2 10) (1 5)))
                 (negate (make-polynomial 'x '((3 20) (2 15)))))
        )
    )

))


(run-tests polynomial-tests)

