#lang scheme


(define (println arg)
    (print arg) (newline)
)

(define (square x)
    (* x x)
)

(define (smallest-divisor n)
    (finddivisor n 2)
)

(define (finddivisor n testdivisor)
    (cond 
        ((> (square testdivisor) n) n)
        ((divides? testdivisor n) testdivisor)
        (else (finddivisor n (+ testdivisor 1))) 
    )
)

(define (divides? a b)
    (= (remainder b a) 0)
)

(define (prime? n)
    (= n (smallest-divisor n))
)

(println "Simple prime test")
(println (prime? 10))
(println (prime? 11))
(println (prime? 12))
(println (prime? 13))


; Fermat test


(define (expmod base exp mod)
    (cond 
        ((= exp 0) 1)
        ((even? exp) 
            (remainder (square (expmod base (/ exp 2) mod)) mod)
        )
        (else
            (remainder (* base (expmod base (- exp 1) mod)) mod)
        )
    )
)

(define (fermat-test n)
    (define (tryit a)
        (= (expmod a n n) a)
    )

    (define nextrand (random (- n 1)))
    (tryit (+ 1 nextrand))
)

(define (fast-prime? n times)
    (cond
        ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)
    )
)

(println "Fermat fast test")
(println (fast-prime? 10001 5))
(println (fast-prime? 11101 5))
(println (fast-prime? 12101 5))
(println (fast-prime? 13101 5))