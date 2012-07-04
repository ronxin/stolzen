#lang scheme 

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))
    )
)

(define (xor a b)
    (or (and a (not b))
        (and (not a) b)    
    )
)

(define (make-rat n d)
    (define (comp-sign num den)
        (if (xor (> num 0) (> den 0))
            -1 
            1
        )
    )
    (let ((absn (abs n))
          (absd (abs d))
          (sign (comp-sign n d)))
        (let ((g (gcd absn absd)))
            (cons (* sign (/ absn g)) (/ absd g))
        )    
    )
)

(define (numer x)
    (car x)
)

(define (denom x)
    (cdr x)
)

(define (add-rat x y)
    (make-rat 
        (+ (* (numer x) (denom y))
           (* (numer y) (denom x)))
        (* (denom x) (denom y))    
    )
)

(define (sub-rat x y)
    (make-rat
        (- (* (numer x) (denom y))
           (* (numer y) (denom x)))
        (* (denom x) (denom y))
    )
)

(define (mul-rat x y)
    (make-rat 
        (* (numer x) (numer y))
        (* (denom x) (denom y))
    )
)

(define (div-rat x y)
    (make-rat
        (* (numer x) (denom y))
        (* (denom x) (numer y))
    )
)

(define (equals-rat? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x)))
)

(define (print-rat x)
    (display (numer x))
    (display "/")
    (display (denom x))
    (newline) 
)


(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

(print-rat one-half)
(print-rat (add-rat one-third one-third))

(print (equals-rat? one-third one-third)) (newline)

(print-rat (make-rat -4 8))
(print-rat (make-rat -2 -4))
(print-rat (make-rat 2 4))
(print-rat (make-rat 3 -6))