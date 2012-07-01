#lang scheme

(define (println arg)
    (print arg) (newline)
)

(define (sqrt x)
    (define (sqrt-iter guess)
        (println guess)
        (if (good-enough? guess)
            guess
            (sqrt-iter (improve guess))
        )
    )

    (define (good-enough? guess)
        (< (abs (- (square guess) x)) 0.001)
    )

    (define (improve guess)
        (average guess (/ x guess))
    )

    (define (average a b)
        (/ (+ a b) 2)
    )

    (define (square a)
        (* a a)
    )

    (sqrt-iter 1.0)
)


(define res (sqrt 5.5))
(println res)