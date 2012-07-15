#lang scheme

(require rackunit)
(require rackunit/text-ui)

(require "../container.scm")

(require "../drop.scm")
(require "../complex-numbers.scm")
(require "../rational-numbers.scm")

(require "../install-modules.scm")

(define drop-test-suite (test-suite
    "Drop test suite"

    (test-case
        "Complex projected to number, project-complex"
        (check-equal? 
            (project-complex (make-complex-from-real-imag 1 2))
            1)
    )

    (test-case
        "Complex projected to rational, project-complex"
        (check-equal? 
            (project-complex (make-complex-from-real-imag (make-rational 2 3) 2))
            (make-rational 2 3))
    )

    (test-case
        "Rational projected to number, project-rational"
        (check-equal? 
            (project-rational (make-rational 2 3))
            2)
    )
    
    (test-case
        "Complex projected to number"
        (check-equal? 
            (project (make-complex-from-real-imag 1 2))
            1)
    )
    
    (test-case
        "Complex projected to rational"
        (check-equal? 
            (project (make-complex-from-real-imag (make-rational 2 3) 2))
            (make-rational 2 3))
    )

    (test-case
        "Rational projected to number"
        (check-equal? 
            (project (make-rational 2 3))
            2)
    )
))


(run-tests drop-test-suite)