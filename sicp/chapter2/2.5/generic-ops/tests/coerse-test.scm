#lang scheme

(require rackunit)
(require rackunit/text-ui)

(require "../coerce.scm")
(require "../generic-operations.scm")

(require "../complex-numbers.scm")
(require "../install-modules.scm")
(require "../rational-numbers.scm")

(define coerse-test-suite (test-suite
    "Coerse test suite"

    (check-equal? 
        (get-index 'complex inheritance-indexes) 0
        "Complex is the zeroth in the tower")

    (check-equal? 
        (get-index 'rational inheritance-indexes) 1
        "Rational is the first if the tower")

    (check-equal? 
        (get-index 'scheme-number inheritance-indexes) 2
        "Scheme-numer is the last in the tower")

    (check-equal? 
        (get-type 0 inheritance-indexes) 'complex
        "zeroth element of the tower is complex")

    (check-equal? 
        (get-type 1 inheritance-indexes) 'rational
        "First element of the tower is rational")

    (check-equal? 
        (get-type 2 inheritance-indexes) 'scheme-number
        "Second element of the tower is scheme-numer")

    (check-equal? 
        (find-superest-parent '(scheme-number scheme-number rational) inheritance-indexes) 
        'rational
        "in (scheme-number scheme-number rational) rational is superest")

    (check-equal? 
        (find-superest-parent '(scheme-number scheme-number scheme-number) inheritance-indexes)
        'scheme-number
        "in (scheme-number scheme-number scheme-number) scheme-number is superest")

    (check-equal? 
        (find-superest-parent '(scheme-number complex rational) inheritance-indexes)
        'complex
        "in (scheme-number complex rational) complex is the superest")

    (check-equal? 
        (calc-distance 'complex 'complex inheritance-indexes) 0
        "Distance between complex and complex is 0")
    (check-equal? 
        (calc-distance 'complex 'rational inheritance-indexes) 1
        "Distance between complex and rational is 1")
    (check-equal? 
        (calc-distance 'complex 'scheme-number inheritance-indexes) 2
        "Distance between complex and scheme-number is 2")


    (check-equal? 
        (distances '(complex rational scheme-number) inheritance-indexes)
        '(0 1 2)
        "for (complex rational scheme-number) distances are (0 1 2)")

    (check-equal? 
        (distances '(complex complex complex) inheritance-indexes)
        '(0 0 0) 
        "for all complex numbers all distances are 0")

    (check-equal? 
        (distances '(scheme-number scheme-number scheme-number) inheritance-indexes)
        '(0 0 0)
        "for all scheme-numbers all the distances are 0")

    (check-equal? 
        (raise-times 1 0) 1
        "Number raised 0 times still number")

    (check-equal? 
        (raise-times 1 1)
        (make-rational 1 1)
        "Number raised 1 times is rational")

    (check-equal? 
        (raise-times 1 2)
        (make-complex-from-real-imag (make-rational 1 1) 0)
        "Number raised 2 times is complex")

    (check-equal? 
        (raise-seq (list 1 (make-complex-from-real-imag 2 2)) '(2 0))
        (list (make-complex-from-real-imag (make-rational 1 1) 0)
              (make-complex-from-real-imag 2 2))
        "Raising pairs")

    (check-equal? 
        (coerce (list (make-complex-from-real-imag 1 2) 
                      (make-complex-from-real-imag 2 2)) 
                inheritance-indexes)
        (list         (make-complex-from-real-imag 1 2) 
                      (make-complex-from-real-imag 2 2))
        "Coerse on same types yields same types")
))


(run-tests coerse-test-suite)