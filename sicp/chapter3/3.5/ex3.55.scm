#lang scheme

(require "streams.scm")

(define (add-streams s1 s2)
    (stream-map + s1 s2)
)

(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1)))
)

(define integers (integers-starting-from 1))

; 3.55


(define partial (cons-stream 0 (add-streams integers partial)))
(stream-to-list (stream-first-n partial 10))


(define (partial-sums stream)
    (define partial (cons-stream (stream-car stream) (add-streams (stream-cdr stream) partial)))
    partial
)

(stream-to-list (stream-first-n (partial-sums integers) 10))