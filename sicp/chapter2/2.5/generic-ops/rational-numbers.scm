#lang scheme

(require "container.scm")
(require "tags.scm")
(require "scheme-numbers.scm")
(require "generic-operations.scm")

(define (install-rational-package)
    ;; internal procedures
    (define (numer x) 
        (car x)
    )

    (define (denom x) 
        (cadr x)
    )

    (put 'numer 'rational (lambda (x) (numer (contents x))))
    (put 'denom 'rational (lambda (x) (denom (contents x))))

    (define (make-rat n d)
        (if (and (scheme-number? n) (scheme-number? d))
            (let 
               ((g (gcd n d)))
               (list (/ n g)
                     (/ d g))
            )
            (list n d)
        )
    )

    (define (value rat)
        (if (and (scheme-number? (numer rat)) (scheme-number? (denom rat)))
            (/ (numer rat) (denom rat))
            rat
        )
    )

    (define (add-rat x y)
        (make-rat (add (mul (numer x) (denom y))
                       (mul (numer y) (denom x)))
                  (mul (denom x) (denom y)))
    )

    (define (sub-rat x y)
        (make-rat (sub (mul (numer x) (denom y))
                       (mul (numer y) (denom x)))
                  (mul (denom x) (denom y)))
    )

    (define (mul-rat x y)
        (make-rat (mul (numer x) (numer y))
                  (mul (denom x) (denom y)))
    )

    (define (div-rat x y)
        (make-rat (mul (numer x) (denom y))
                  (mul (denom x) (numer y)))
    )

    ;; interface to rest of the system
    (define (tag x) 
        (attach-tag 'rational x)
    )

    (put 'add '(rational rational)
        (lambda (x y) (tag (add-rat (contents x) (contents y))))
    )

    (put 'sub '(rational rational)
        (lambda (x y) (tag (sub-rat (contents x) (contents y))))
    )

    (put 'mul '(rational rational)
        (lambda (x y) (tag (mul-rat (contents x) (contents y))))
    )

    (put 'div '(rational rational)
        (lambda (x y) (tag (div-rat (contents x) (contents y))))
    )

    (put 'cos '(rational)
        (lambda (x) (cosine (value (contents x))))
    )

    (put 'sin '(rational)
        (lambda (x) (sine (value (contents x))))
    )

    (put 'sqrt '(rational)
        (lambda (x) (sqroot (value (contents x))))
    )

    (put 'atan '(rational rational)
        (lambda (y x) (artangent2 (value (contents y)) (value (contents x))))
    )

    (put 'make 'rational
        (lambda (n d) (tag (make-rat n d)))
    )

    (define (zero? a)
        (=zero? (numer a))
    )

    (put '=zero? '(rational)
        (lambda (a) (zero? (contents a)))
    )

    (define (drop a)
        (if (equal? 1 (denom a))
            (numer a)
            false
        )
    )

    (put 'drop 'rational
        (lambda (a) (drop (contents a)))
    )

    (void)
)

(define (make-rational n d)
    ((get 'make 'rational) n d)
)

(define (number->rational x) 
    (make-rational x 1)
)

(put 'raise 'scheme-number number->rational)

(provide (all-defined-out))