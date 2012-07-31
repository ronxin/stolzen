#lang scheme

(require "streams.scm")

(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1)))
)

(define integers (integers-starting-from 1))

(stream-to-list (stream-first-n integers 10))


(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens 
    (stream-filter 
        (lambda (x) (not (divisible? x 7)))
    integers)
)


(stream-ref no-sevens 100)


(define (fibgen a b)
    (cons-stream a (fibgen b (+ a b)))
)

(define fibs (fibgen 0 1))


(stream-ref fibs 200)


(define (sieve stream)
    (cons-stream
        (stream-car stream)
        (sieve (stream-filter
            (lambda (x)
                (not (divisible? x (stream-car stream))))
            (stream-cdr stream))
        )
    )
)

(define primes (sieve (integers-starting-from 2)))

(stream-ref primes 50)



(define ones (cons-stream 1 ones))

(stream-to-list (stream-first-n ones 10))

(define (add-streams s1 s2)
    (stream-map + s1 s2)
)

(define integers2 (cons-stream 1 (add-streams ones integers2)))

(stream-to-list (stream-first-n integers2 10))


(define fibs2
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs2)
                                         fibs2))))

(stream-ref fibs2 10)

(define (scale-stream stream factor)
    (stream-map (lambda (x) (* x factor)) stream)
)

(define double (cons-stream 1 (scale-stream double 2)))
(stream-to-list (stream-first-n double 11))


