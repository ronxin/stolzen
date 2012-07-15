#lang scheme

(require rackunit)
(require rackunit/text-ui)

(require "../raise.scm")

(require "../rational-numbers.scm")
(require "../complex-numbers.scm")
(require "../install-modules.scm")

(define raise-procedures-test (test-suite
    "Raise procedure"

    (check-equal? 
        (make-rational 1 1)
        (raise 1)
        "Raise on a number yields rational")

    (check-equal? 
        (make-complex-from-real-imag (make-rational 1 2) 0)
        (raise (make-rational 1 2))
        "Raise on a rational yields rectangular complex")

    (check-equal? 
        (make-complex-from-real-imag (make-rational 1 1) 0)
        (raise (raise 1))
        "Raise twice yields rectangular complex")
))

(run-tests raise-procedures-test)