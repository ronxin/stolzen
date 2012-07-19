#lang scheme

(require rackunit)
(require rackunit/text-ui)

(require "../generic-operations.scm")
(require "../complex-numbers.scm")
(require "../install-modules.scm")
(require "../rational-numbers.scm")


(define z11 (make-complex-from-real-imag 3 4))
(define z21 (make-complex-from-real-imag 2 3))
(define z12 (make-complex-from-mag-ang 3 2))
(define z22 (make-complex-from-mag-ang 2 3))

(define complex-numbers-test-suite (test-suite
    "Complex number test suite"

    (check-equal? 
        (add z11 z21)
        (make-complex-from-real-imag 5 7)
        "Add for two rectangular complex numbers")

    (check-equal? 
        (add z12 z22)
        (make-complex-from-real-imag -3.228425502842318 3.010132296596779)
        "Add for rectangular and polar complex numbers")

    (check-equal? 
        (sub z11 z21)
        (make-complex-from-real-imag 1 1)
        "Sub for two rectangular complex numbers")

    (check-equal? 
        (sub z12 z22)
        (make-complex-from-real-imag 0.7315444835594636 2.4456522643573106)
        "Sub for rectangular and polar complex numbers")

    (check-equal? 
        (mul z11 z21)
        (make-complex-from-mag-ang 18.027756377319946 1.9100889412489412))

    (check-equal? 
        (mul z12 z22)
        (make-complex-from-mag-ang 6 5))

    (check-equal? 
        (div z11 z21)
        (make-complex-from-mag-ang 1.386750490563073 -0.05549850524571687))

    (check-equal? 
        (div z12 z22)
        (make-complex-from-mag-ang 3/2 -1))

    (check-equal?
        (add (make-complex-from-real-imag (make-rational 3 4) (make-rational 4 3))
             (make-complex-from-real-imag (make-rational 1 2) (make-rational 2 3)))
        (make-complex-from-real-imag (make-rational 5 4) 2))

    (test-case
        "=zero? for zero both number zeros"
        (check-true  
            (=zero? (make-complex-from-real-imag 0 0)))
    )

    (test-case
        "=zero? for non-zero both number non-zeros"
        (check-false 
            (=zero? (make-complex-from-real-imag 1 1)))
    )

    (test-case
        "=zero? for non-zero, one zero number"
        (check-false 
            (=zero? (make-complex-from-real-imag 0 1)))
    )

    (test-case
        "=zero? for non-zero, another zero number"
        (check-false 
            (=zero? (make-complex-from-real-imag 1 0)))
    )

    (test-case
        "=zero? for zero, one rational zero, another number zero"
        (check-true  
            (=zero? (make-complex-from-real-imag (make-rational 0 1) 0)))
    )

    (test-case
        "=zero? for zero, one number zero, another rational zero"
        (check-true  
            (=zero? (make-complex-from-real-imag 0 (make-rational 0 1))))
    )

    (test-case
        "=zero? for zero, both rational zeros"
        (check-true  
            (=zero? (make-complex-from-real-imag (make-rational 0 1) (make-rational 0 2))))
    )

    (test-case
        "=zero? for non-zero, one rational non-zero"
        (check-false  
            (=zero? (make-complex-from-real-imag (make-rational 1 1) 0)))
    )

    (test-case
        "negate for scheme-number rectangular"
        (check-equal?  
            (negate (make-complex-from-real-imag 1 1))
            (make-complex-from-real-imag -1 -1))
    )

    (test-case
        "negate for rational rectangular"
        (check-equal?  
            (negate (make-complex-from-real-imag (make-rational 1 2) 1))
            (make-complex-from-real-imag (make-rational -1 2) -1))
    )

    (test-case
        "negate for polar"
        (check-equal?  
            (negate (make-complex-from-mag-ang 2 (/ pi 4)))
            (make-complex-from-mag-ang 2 (* 3 (/ pi 4))))
    )

    (test-case
        "negate for negative polar"
        (check-equal?  
            (negate (make-complex-from-mag-ang 2 (* 3 (/ pi 4))))
            (make-complex-from-mag-ang 2 (/ pi 4)))
    )

      
))


(run-tests complex-numbers-test-suite)