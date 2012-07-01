#lang scheme

; =========
;    GCD
; =========

(define (gcd a b)
    (if (= b a)
        a
        (gcd b (remainder a b))
    )
)
