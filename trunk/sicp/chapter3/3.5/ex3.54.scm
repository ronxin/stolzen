#lang scheme

(require "streams.scm")

(define (add-streams s1 s2)
    (stream-map + s1 s2)
)

(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1)))
)

(define integers (integers-starting-from 2))

; 3.54

(define (mul-streams s1 s2)
    (stream-map * s1 s2)
)

(define factorials (cons-stream 1 (mul-streams integers factorials)))

(stream-to-list (stream-first-n factorials 10))