#lang scheme

(define (println arg)
    (print arg) (newline)
)

; 1.41

(define (double f)
    (lambda (x)
        (f (f x))
    )
)

(define (inc x) 
    (+ x 1)
)

; (inc 2)
; ((double inc) 5)

(define quadruple (double double))
(define octuple (double quadruple))
; ((octuple inc) 5)


; 1.42

(define (square x)
    (* x x)
)

(define (compose f g)
    (lambda (x)
        (f (g x))
    )
)

; ((compose square inc) 6)


; 1.43

(define (repeat f amount)
    (define (repeat-iter g n)
        (if (= n 1)
            g
            (repeat-iter (compose f g) (- n 1))
        )
    )
    (repeat-iter f amount)
)

((repeat inc 2) 1)
((repeat inc 5) 7)
((repeat square 2) 5)

; 1.44

(define dx 0.00001)

(define (smooth f)
    (define (three-avg a b c)
        (/ (+ a b c) 3.0)
    )

    (lambda (x) 
        (three-avg 
            (f (- x dx)) (f x) (f (+ x dx))
        )
    )
)

(sin 0.5)
((smooth sin) 0.5)

(define (n-smooth f n)
    ((repeat smooth n) f)
)

((n-smooth sin 1) 0.5)
((n-smooth sin 2) 0.5)
