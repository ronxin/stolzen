#lang scheme

(require rackunit)
(require rackunit/text-ui)

(require "../container.scm")

(require "../drop.scm")
(require "../raise.scm")
(require "../complex-numbers.scm")
(require "../rational-numbers.scm")
(require "../generic-operations.scm")
(require "../install-modules.scm")

(define drop-test-suite (test-suite
    "Drop test suite"

    (test-case
        "Can't drop complex with non-zero im part"
        (check-equal? 
            (drop (make-complex-from-real-imag 1 2))
            (make-complex-from-real-imag 1 2))
    )

    (test-case
        "Drop complex with number zero im part"
        (check-equal? 
            (drop (make-complex-from-real-imag 1 0))
                  1)
    )

    (test-case
        "Drop complex with rational real part and number zero im part"
        (check-equal? 
            (drop (make-complex-from-real-imag (make-rational 2 3) 0))
            (make-rational 2 3))
    )

    (test-case
        "Can drop complex with rational zero im part"
        (check-equal? 
            (drop (make-complex-from-real-imag (make-rational 2 3) (make-rational 0 3)))
            (make-rational 2 3))
    )

    (test-case
        "Can drop rational with one denum"
        (check-equal? 
            (drop (make-rational 2 1))
            2)
    )

    (test-case
        "Cant drop rational with non-one denum"
        (check-equal? 
            (drop (make-rational 2 3))
            (make-rational 2 3))
    )

    (test-case
        "Dropped to number when mul rationals"
        (check-equal? 
            (mul (make-rational 2 3) (make-rational 3 2))
            1)
    )

    (test-case
        "Dropped to number when add complex numbers"
        (check-equal? 
            (add (make-complex-from-real-imag 1 -2) (make-complex-from-real-imag 1 2))
            2)
    )

    (test-case
        "Dropped to number when add complex numbers with rational numbers"
        (check-equal? 
            (add (make-complex-from-real-imag (make-rational 2 3) -2) 
                 (make-complex-from-real-imag (make-rational 1 3) 2))
            1)
    )

    (test-case
        "Dropped to rational when add complex numbers"
        (check-equal? 
            (add (make-complex-from-real-imag (make-rational 2 3) -2) 
                 (make-complex-from-real-imag (make-rational 2 3) 2))
            (make-rational 4 3))
    )

        (test-case
        "Dropped to rational when add complex numbers with rational numbers"
        (check-equal? 
            (add (make-complex-from-real-imag (make-rational 2 3) (make-rational -2 3)) 
                 (make-complex-from-real-imag (make-rational 2 3) (make-rational 2 3)))
            (make-rational 4 3))
    )

    (test-case
        "Dropped to number when sub complex numbers"
        (check-equal? 
            (sub (make-complex-from-real-imag 2 2) 
                 (make-complex-from-real-imag 1 2))
            1)
    )

    (test-case
        "Dropped to number when div rational numbers"
        (check-equal? 
            (div (make-rational 2 3) (make-rational 2 3)) 
            1)
    )
))


(run-tests drop-test-suite)