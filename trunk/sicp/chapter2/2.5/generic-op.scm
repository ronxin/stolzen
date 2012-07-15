#lang scheme

(require racket/trace)

; for testing 

(define (container)
    (define hash (make-hash))

    (define (put op type item)
        (hash-set! hash (cons op type) item)
    )

    (define (get op type)
        (hash-ref hash (cons op type))
    )

    (lambda (name) 
        (cond
            ((eq? name 'get) get)
            ((eq? name 'put) put)
        )
    )
)

(define storage (container))

(define put (storage 'put))
(define get (storage 'get))

(put '+ 'test (lambda (a b) (+ a b)))
(equal? ((get '+ 'test) 1 2) 3)

;
; Usual numbers
;

(define (install-scheme-number-package)
    (define (tag x)
        (attach-tag 'scheme-number x)
    )

    (put 'add '(scheme-number scheme-number)
        (lambda (x y) (tag (+ x y)))
    )

    (put 'sub '(scheme-number scheme-number)
        (lambda (x y) (tag (- x y)))
    )

    (put 'mul '(scheme-number scheme-number)
        (lambda (x y) (tag (* x y)))
    )

    (put 'div '(scheme-number scheme-number)
        (lambda (x y) (tag (/ x y)))
    )

    (put 'make 'scheme-number
        (lambda (x) (tag x))
    )

    ; 2.79
    (put 'equ? '(scheme-number scheme-number)
        (lambda (a b) (= a b))
    )

    ; 2.80
    (put 'zero? '(scheme-number)
        (lambda (a) (= a 0))
    )

    'done
)

(define (make-scheme-number n)
    ((get 'make 'scheme-number) n)
)


;
; rational
;

(define (install-rational-package)
    ;; internal procedures
    (define (numer x) 
        (car x)
    )

    (define (denom x) 
        (cdr x)
    )

    (define (make-rat n d)
        (let 
            ((g (gcd n d)))
            (cons (/ n g) (/ d g))
        )
    )

    (define (add-rat x y)
        (make-rat (+ (* (numer x) (denom y))
                     (* (numer y) (denom x)))
                  (* (denom x) (denom y)))
    )

    (define (sub-rat x y)
        (make-rat (- (* (numer x) (denom y))
                     (* (numer y) (denom x)))
                  (* (denom x) (denom y)))
    )

    (define (mul-rat x y)
        (make-rat (* (numer x) (numer y))
                  (* (denom x) (denom y)))
    )

    (define (div-rat x y)
        (make-rat (* (numer x) (denom y))
                  (* (denom x) (numer y)))
    )

    ;; interface to rest of the system
    (define (tag x) 
        (attach-tag 'rational x)
    )

    (put 'add '(rational rational)
        (lambda (x y) (tag (add-rat x y)))
    )

    (put 'sub '(rational rational)
        (lambda (x y) (tag (sub-rat x y)))
    )

    (put 'mul '(rational rational)
        (lambda (x y) (tag (mul-rat x y)))
    )

    (put 'div '(rational rational)
        (lambda (x y) (tag (div-rat x y)))
    )

    (put 'make 'rational
        (lambda (n d) (tag (make-rat n d)))
    )

    ; 2.79
    (put 'equ? '(rational rational)
        (lambda (r1 r2) (and (= (numer r1) (numer r2)) (= (denom r1) (denom r2))))
    )

    ; 2.80
    (put 'zero? '(rational)
        (lambda (a) (= (numer a) 0))
    )

    'done
)

(define (make-rational n d)
    ((get 'make 'rational) n d)
)

;
; Complex
;

(define (square x)
    (* x x)
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
        (sqrt (+ (square (real-part z))
                 (square (imag-part z))))
    )

    (define (angle z)
        (atan (imag-part z) (real-part z))
    )

    (define (make-from-real-imag x y)
        (cons x y)
    )

    (define (make-from-mag-ang r a)
        (cons (* r (cos a)) (* r (sin a)))
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


(define (install-polar-package)
    (define POLAR 'polar)

    (define (real-part z)
        (* (magnitude z) (cos (angle z)))
    )

    (define (imag-part z)
        (* (magnitude z) (sin (angle z)))
    )

    (define (magnitude z)
        (car z)
    )

    (define (angle z)
        (cdr z)
    )

    (define (make-from-real-imag x y)
        (cons 
            (sqrt (+ (square x) (square y)))
            (atan y x)
        )
    )

    (define (make-from-mag-ang r a)
        (cons r a)
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

(define (install-complex-package)
    ;; imported procedures from rectangular and polar packages
    (define (make-from-real-imag x y)
        ((get 'make-from-real-imag 'rectangular) x y)
    )
    
    (define (make-from-mag-ang r a)
        ((get 'make-from-mag-ang 'polar) r a)
    )

    (define (operation name z)
        ((get name (type-tag z)) (contents z))
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
        (make-from-real-imag (+ (real-part z1) (real-part z2))
                             (+ (imag-part z1) (imag-part z2)))
    )

    (define (sub-complex z1 z2)
        (make-from-real-imag (- (real-part z1) (real-part z2))
                             (- (imag-part z1) (imag-part z2)))
    )

    (define (mul-complex z1 z2)
        (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                           (+ (angle z1) (angle z2)))
    )
    
    (define (div-complex z1 z2)
        (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                           (- (angle z1) (angle z2)))
    )

    ;; interface to rest of the system
    (define (tag z) (attach-tag 'complex z))

    ; 2.77
    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle)

    (put 'add '(complex complex)
        (lambda (z1 z2) (tag (add-complex z1 z2)))
    )

    (put 'sub '(complex complex)
        (lambda (z1 z2) (tag (sub-complex z1 z2)))
    )

    (put 'mul '(complex complex)
        (lambda (z1 z2) (tag (mul-complex z1 z2)))
    )

    (put 'div '(complex complex)
        (lambda (z1 z2) (tag (div-complex z1 z2)))
    )

    (put 'make-from-real-imag 'complex
        (lambda (x y) (tag (make-from-real-imag x y)))
    )

    (put 'make-from-mag-ang 'complex
        (lambda (r a) (tag (make-from-mag-ang r a)))
    )

    ; 2.79
    (put 'equ? '(complex complex)
        (lambda (z1 z2) (and (= (real-part z1) (real-part z2)) (= (imag-part z1) (imag-part z2))))
    )

    ; 2.80
    (put 'zero? '(complex)
        (lambda (a) (and (= (real-part a) 0) (= (imag-part a) 0)))
    )

    'done
)

(define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'complex) x y)
)

(define (make-complex-from-mag-ang r a)
    ((get 'make-from-mag-ang 'complex) r a)
)

;
; Generic
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


(define (add x y) 
    (apply-generic 'add x y)
)

(define (sub x y) 
    (apply-generic 'sub x y)
)

(define (mul x y) 
    (apply-generic 'mul x y)
)

(define (div x y) 
    (apply-generic 'div x y)
)

;
; test
;

(install-scheme-number-package)

(add (make-scheme-number 3) (make-scheme-number 1))
(sub (make-scheme-number 3) (make-scheme-number 1))
(mul (make-scheme-number 3) (make-scheme-number 1))
(div (make-scheme-number 3) (make-scheme-number 1))

(install-rational-package)

(add (make-rational 3 2) (make-rational 2 3))
(sub (make-rational 3 2) (make-rational 2 3))
(mul (make-rational 3 2) (make-rational 2 3))
(div (make-rational 3 2) (make-rational 2 3))



(install-polar-package)
(install-rectangular-package)
(install-complex-package)


(define z11 (make-complex-from-real-imag 3 4))
(define z21 (make-complex-from-real-imag 2 3))
(define z12 (make-complex-from-mag-ang 3 2))
(define z22 (make-complex-from-mag-ang 2 3))

(add z11 z21)
(add z12 z22)
(sub z11 z21)
(sub z12 z22)

(mul z11 z21)
(mul z12 z22)
(div z11 z21)
(div z12 z22)


; 2.77

(apply-generic 'magnitude z11)
; in this case (in this code) apply-generic is applied only once. 
; but within "magnitude" it also uses "get"


; 2.79

(define (equ? v1 v2)
    (and 
        (eq? (type-tag v1) (type-tag v2))
        (apply-generic 'equ? v1 v2)
    )
)

(equ? z11 z11)
(not (equ? z11 z12))
(not (equ? (make-scheme-number 3) (make-scheme-number 1)))
(equ? (make-scheme-number 3) (make-scheme-number 3))

(equ? (make-rational 3 2) (make-rational 6 4))
(equ? (make-rational 3 2) (make-rational 6 4))

(not (equ? z11 (make-scheme-number 3)))
(not (equ? (make-rational 3 2) (make-scheme-number 2)))


; 2.80

(define (=zero? a)
    (apply-generic 'zero? a)
)

(=zero? (make-scheme-number 0))
(not (=zero? (make-scheme-number 1)))

(=zero? (make-rational 0 2))
(not (=zero? (make-rational 3 2)))

(=zero? (make-complex-from-real-imag 0 0))
(=zero? (make-complex-from-mag-ang 0 0))
(=zero? (make-complex-from-mag-ang 0 12))
(not (=zero? z11))