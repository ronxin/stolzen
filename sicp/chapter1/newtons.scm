#lang scheme 

(define dx 0.00001)
(define tolerance dx)

(define (println arg)
    (print arg) (newline)
)


(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance)
    )

    (define (try guess)
        (let 
            ((next (f guess)))

            (if (close-enough? guess next) 
                next
                (try next)
            )
        )
    )

    (try first-guess)
)

(define (average x y)
    (/ (+ x y) 2.0)
)

(define (average-damp f)
    (lambda (x) (average x (f x)))
)

(define (sqrt x)
    (fixed-point (average-damp (lambda (y) (/ x y))) 1.0)
)

(println "SQRT")
(println (sqrt 9))

(define (deriv g)
    (lambda (x)
        (/ (- (g (+ x dx)) (g x)) dx)
    )
)

(define (square x)
    (* x x)
)

(define (cube x)
    (* x x x)
)

(define dcube (deriv cube))
(println "derivative of x^3 in 5.0")
(println (dcube 5))


(define (newton-transform g)
    (let 
        ((dg (deriv g)))

        (lambda (x)
            (- x (/ (g x) (dg x)))
        )
    )
)


(define (newton-method g guess)
    (fixed-point (newton-transform g) guess)
)

(define (sqrt-newton x)
    (newton-method (lambda (y) (- (square y) x)) 1.0)
)


(println "SQRT-NEWTON")
(println (sqrt-newton 9))

