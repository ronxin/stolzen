#lang scheme

(define (println arg)
    (print arg) (newline)
)

(define (sqrt x)
    (sqrt-iter 1.0 x)
)

(define (sqrt-iter guess x)
    (println guess)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)
    )
)

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001)
)

(define (improve guess x)
    (average guess (/ x guess))
)

(define (average x y)
    (/ (+ x y) 2)
)

(define (square x)
    (* x x)
)

(define res (sqrt 0.5))
(println res)