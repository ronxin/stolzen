#lang scheme

(define (square x)
    (* x x)
)




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

;
; ===  RECTANGULAR ===
;

(define RECTANGULAR 'rectangular)

(define (rectangular? z)
    (eq? (type-tag z) RECTANGULAR)
)

(define (real-part-rectangular z)
    (car z)
)

(define (imag-part-rectangular z)
    (cdr z)
)

(define (magnituge-rectangular z)
    (sqrt (+ (square (real-part-rectangular z))
             (square (imag-part-rectangular z))))
)

(define (angle-rectangular z)
    (atan (imag-part-rectangular z) (real-part-rectangular z))
)

(define (make-from-real-imag-rectangular x y)
    (attach-tag RECTANGULAR (cons x y))
)

(define (make-from-mag-ang-rectangular r a)
    (attach-tag RECTANGULAR (cons (* r (cos a)) (* r (sin a)))) 
)

;
; ===  POLAR ===
;

(define POLAR 'polar)

(define (polar? z)
    (eq? (type-tag z) POLAR)    
)

(define (real-part-polar z)
    (* (magnituge-polar z) (cos angle-polar z))
)

(define (imag-part-polar z)
    (* (magnituge-polar z) (cos (angle-polar z)))
)

(define (magnituge-polar z)
    (car z)
)

(define (angle-polar z)
    (cdr z)
)

(define (make-from-real-imag-polar x y)
    (attach-tag POLAR 
        (cons 
            (sqrt (+ (square x) (square y)))
            (atan y x)
        )
    )
)

(define (make-from-mag-ang-polar r a)
    (attach-tag POLAR (cons r a))
)

;
; ===  Generic ===
;


(define (real-part z)
    (cond
        ((rectangular? z) (real-part-rectangular (contents z)))
        ((polar? z) (real-part-polar (contents z)))
        (else
            (error "Unknown type - real-part" z))
    )
)

(define (imag-part z)
    (cond
        ((rectangular? z) (imag-part-rectangular (contents z)))
        ((polar? z) (imag-part-polar (contents z)))
        (else
            (error "Unknown type - real-part" z))
    )
)

(define (magnituge z)
    (cond
        ((rectangular? z) (magnituge-rectangular (contents z)))
        ((polar? z) (magnituge-polar (contents z)))
        (else
            (error "Unknown type - magnituge" z))
    )
)

(define (angle z)
    (cond
        ((rectangular? z) (angle-rectangular (contents z)))
        ((polar? z) (angle-polar (contents z)))
        (else
            (error "Unknown type - angle" z))
    )
)

(define (make-from-real-imag real imag)
    (make-from-real-imag-rectangular real imag)
)

(define (make-from-mag-ang mag ang)
    (make-from-mag-ang-polar mag ang)
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