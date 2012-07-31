#lang scheme

(require "streams.scm")

(define (show x) (display-line x) x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)

; should be (with memoisation on)
; 1 2 3 4 5; return value 5
; 6 7; return value 7