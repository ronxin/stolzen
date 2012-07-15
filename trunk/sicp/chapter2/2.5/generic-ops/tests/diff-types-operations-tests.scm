#lang scheme


(require rackunit)
(require rackunit/text-ui)

(require "../raise.scm")
(require "../generic-operations.scm")
(require "../complex-numbers.scm")
(require "../scheme-numbers.scm")

(require "../install-modules.scm")


(define z (make-complex-from-real-imag 3 4))

(define different-types-test-suite (test-suite
    "Operations on different types suite"

    (check-equal? 
        (add z (make-scheme-number 4))
        (add (make-scheme-number 4) z))
))




(run-tests different-types-test-suite)