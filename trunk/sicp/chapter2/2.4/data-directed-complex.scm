#lang scheme

(define (square x)
    (* x x)
)





(define (put op type item)
    true
)

(define (get op type)
    (define (res . params)
        'whatever
    )

    res
)

;
; ===  RECTANGULAR ===
;


(define (install-rectangular-package)
    (define RECTANGULAR 'rectangular)

    (define (real-part z)
        (car z)
    )

    (define (imag-part z)
        (cdr z)
    )

    (define (magnituge z)
        (sqrt (+ (square (real-part z))
                 (square (imag-part z))))
    )

    (define (angle z)
        (atan (imag-part z) (real-part z))
    )

    (define (make-from-real-imag x y)
        (attach-tag RECTANGULAR (cons x y))
    )

    (define (make-from-mag-ang r a)
        (attach-tag RECTANGULAR (cons (* r (cos a)) (* r (sin a)))) 
    ) 

    ;; interface

    (define (tag x)
        (attach-tag RECTANGULAR x)
    )

    (put 'real-part RECTANGULAR real-part)
    (put 'imag-part RECTANGULAR imag-part)
    (put 'magnitude RECTANGULAR magnitude)
    (put 'angle RECTANGULAR angle)
    (put 'make-from-real-imag RECTANGULAR 
        (lambda (x y) (tag (make-from-real-imag x y)))
    )
    (put 'make-from-mag-ang RECTANGULAR 
        (lambda (r a) (tag (make-from-mag-ang r a)))
    )

    'done
)


;
; ===  POLAR ===
;

(define (install-polar-package)
    (define POLAR 'polar)

    (define (real-part z)
        (* (magnituge z) (cos angle z))
    )

    (define (imag-part z)
        (* (magnituge z) (cos (angle z)))
    )

    (define (magnituge z)
        (car z)
    )

    (define (angle z)
        (cdr z)
    )

    (define (make-from-real-imag x y)
        (attach-tag POLAR 
            (cons 
                (sqrt (+ (square x) (square y)))
                (atan y x)
            )
        )
    )

    (define (make-from-mag-ang r a)
        (attach-tag POLAR (cons r a))
    )

    ;; interface

    (define (tag x)
        (attach-tag POLAR x)
    )

    (put 'real-part POLAR real-part)
    (put 'imag-part POLAR imag-part)
    (put 'magnitude POLAR magnitude)
    (put 'angle POLAR angle)

    (put 'make-from-real-imag POLAR 
        (lambda (x y) (tag (make-from-real-imag x y)))
    )
    (put 'make-from-mag-ang POLAR 
        (lambda (r a) (tag (make-from-mag-ang r a)))
    )


    'done
)



;
; ===  Generic ===
;

(define (attach-tag type-tag contents)
    (cons type-tag contents)
)

(define (type-tag datum)
    (if (pair? datum)
        (car datum)
        (error "Bad tagged datum -- type-tag" datum)
    )
)

(define (contents datum)
    (if (pair? datum)
        (cdr datum)
        (error "Bad tagged datum -- contents" datum)
    )
)

(define (apply-generic op . args)
    (let 
        ((type-tags (map type-tag args)))

        (let 
            ((proc (get op type-tags)))

            (if proc 
                (apply proc (map contents args))
                (error "No method for this type -- apply-generic" (list op type-tags))
            )
        )
    )
)


(define (real-part z)
    (apply-generic 'real-part z)
)

(define (imag-part z)
    (apply-generic 'imag-part z)
)

(define (magnituge z)
    (apply-generic 'magnituge z)
)

(define (angle z)
    (apply-generic 'magnituge z)
)

(define (make-from-real-imag real imag)
    ((get 'make-from-real-imag 'rectangular) real imag)
)

(define (make-from-mag-ang mag ang)
    ((get 'make-from-mag-ang 'polar) mag ang)
)

; 
; === OPERATIONS ===
; 

(define (add-complex z1 z2)
    (make-from-real-imag 
        (+ (real-part z1) (real-part z2))
        (+ (imag-part z1) (imag-part z2))
    )
)

(define (sub-complex z1 z2)
    (make-from-real-imag 
        (- (real-part z1) (real-part z2))
        (- (imag-part z1) (imag-part z2))
    )
)

(define (mul-complex z1 z2)
    (make-from-mag-ang 
        (* (magnituge z1) (magnituge z2))
        (+ (angle z1) (angle z2))
    )
)

(define (div-complex z1 z2)
    (make-from-mag-ang 
        (/ (magnituge z1) (magnituge z2))
        (- (angle z1) (angle z2))
    )
)

(add-complex 
    (make-from-real-imag 2 0)
    (make-from-real-imag 2 2)
)

(mul-complex 
    (make-from-real-imag 1 1)
    (make-from-real-imag 2 3)
)

