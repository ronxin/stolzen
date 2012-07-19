#lang scheme

(require "container.scm")
(require "tags.scm")
(require "generic-operations.scm")


(define (square x)
    (mul x x)
)

(define (install-rectangular-package)
    (define RECTANGULAR 'rectangular)

    (define (real-part z)
        (car z)
    )

    (define (imag-part z)
        (cdr z)
    )

    (define (magnitude z)
        (sqroot (add (square (real-part z))
                     (square (imag-part z))))
    )

    (define (angle z)
        (artangent2 (imag-part z) (real-part z))
    )

    (define (negate-rectangular z)
        (make-from-real-imag (negate (real-part z)) (negate (imag-part z)))
    )

    (define (make-from-real-imag x y)
        (cons x y)
    )

    (define (make-from-mag-ang r a)
        (cons (mul r (cosine a)) (mul r (sine a)))
    ) 

    ;; interface

    (define (tag x)
        (attach-tag RECTANGULAR x)
    )

    
    (put 'real-part RECTANGULAR real-part)
    (put 'imag-part RECTANGULAR imag-part)
    (put 'magnitude RECTANGULAR magnitude)
    (put 'angle RECTANGULAR angle)

    (put 'negate RECTANGULAR (lambda (x) (tag (negate-rectangular x))))

    (put 'make-from-real-imag RECTANGULAR 
        (lambda (x y) (tag (make-from-real-imag x y)))
    )

    (put 'make-from-mag-ang RECTANGULAR 
        (lambda (r a) (tag (make-from-mag-ang r a)))
    )

    (void)
)


(define (install-polar-package)
    (define POLAR 'polar)

    (define (real-part-polar z)
        (mul (magnitude z) (cosine (angle z)))
    )

    (define (imag-part z)
        (mul (magnitude z) (sine (angle z)))
    )

    (define (magnitude z)
        (car z)
    )

    (define (angle z)
        (cdr z)
    )

    (define (rotate180 a)
        (let 
            ((res (+ a (/ pi 2))))
            (if (> res pi)
                (- res pi)
                res
            )
        )
    )

    (define (negate-polar z)
        (make-from-mag-ang (magnitude z) (rotate180 (angle z)))
    )

    (define (make-from-real-imag x y)
        (cons 
            (sqroot (add (square x) (square y)))
            (artangent2 y x)
        )
    )

    (define (make-from-mag-ang r a)
        (cons r a)
    )

    ;; interface

    (define (tag x)
        (attach-tag POLAR x)
    )

    (put 'real-part POLAR real-part-polar)
    (put 'imag-part POLAR imag-part)
    (put 'magnitude POLAR magnitude)
    (put 'angle POLAR angle)

    (put 'negate POLAR (lambda (x) (tag (negate-polar x))))

    (put 'make-from-real-imag POLAR 
        (lambda (x y) (tag (make-from-real-imag x y)))
    )

    (put 'make-from-mag-ang POLAR 
        (lambda (r a) (tag (make-from-mag-ang r a)))
    )

    (void)
)

(define (install-complex-package)
    (install-rectangular-package)
    (install-polar-package)

    ;; imported procedures from rectangular and polar packages
    (define make-from-real-imag (get 'make-from-real-imag 'rectangular))
    (define make-from-mag-ang (get 'make-from-mag-ang 'polar))

    (define (get-op name z)
        (get name (type-tag z))
    )

    (define (operation name z)
        ((get-op name z) (contents z))
    )

    (define (real-part z)
        (operation 'real-part z)
    )

    (define (imag-part z)
        (operation 'imag-part z)
    )

    (define (magnitude z)
        (operation 'magnitude z)
    )

    (define (angle z)
        (operation 'angle z)
    )

    ;; internal procedures
    (define (add-complex z1 z2)
        (make-from-real-imag (add (real-part z1) (real-part z2))
                             (add (imag-part z1) (imag-part z2)))
    )

    (define (sub-complex z1 z2)
        (make-from-real-imag (sub (real-part z1) (real-part z2))
                             (sub (imag-part z1) (imag-part z2)))
    )

    (define (negate-complex z)
        (operation 'negate z)
    )

    (define (mul-complex z1 z2)
        (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                           (add (angle z1) (angle z2)))
    )
    
    (define (div-complex z1 z2)
        (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                           (sub (angle z1) (angle z2)))
    )

    ;; interface to rest of the system
    (define (tag z) (attach-tag 'complex z))

    (put 'real-part 'complex (lambda (z) (real-part (contents z))))
    (put 'imag-part 'complex (lambda (z) (imag-part (contents z))))
    (put 'magnitude 'complex (lambda (z) (magnitude (contents z))))
    (put 'angle     'complex (lambda (z) (angle (contents z))))

    (put 'add '(complex complex)
        (lambda (z1 z2) (tag (add-complex (contents z1) (contents z2))))
    )

    (put 'negate '(complex)
        (lambda (z) (tag (negate-complex (contents z))))
    )

    (put 'sub '(complex complex)
        (lambda (z1 z2) (tag (sub-complex (contents z1) (contents z2))))
    )

    (put 'mul '(complex complex)
        (lambda (z1 z2) (tag (mul-complex (contents z1) (contents z2))))
    )

    (put 'div '(complex complex)
        (lambda (z1 z2) (tag (div-complex (contents z1) (contents z2))))
    )

    (put 'make-from-real-imag 'complex
        (lambda (x y) (tag (make-from-real-imag x y)))
    )

    (put 'make-from-mag-ang 'complex
        (lambda (r a) (tag (make-from-mag-ang r a)))
    )

    (define (zero? z)
        (and (=zero? (real-part z)) (=zero? (imag-part z)))
    )

    (put '=zero? '(complex)
        (lambda (a) (zero? (contents a)))
    )

    (define (drop a)
        (if (=zero? (imag-part a))
            (real-part a)
            false
        )
    )

    (put 'drop 'complex
        (lambda (a) (drop (contents a)))
    )

    (void)
)

(define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'complex) x y)
)

(define (make-complex-from-mag-ang r a)
    ((get 'make-from-mag-ang 'complex) r a)
)

(define (rational->complex x) 
    (make-complex-from-real-imag x 0)
)

(put 'raise 'rational rational->complex)

(provide (all-defined-out))