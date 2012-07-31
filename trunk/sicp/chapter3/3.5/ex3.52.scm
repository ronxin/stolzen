#lang scheme

(require "streams.scm")

(define sum 0)
(define (accum x)
    (set! sum (+ x sum))
    (display "Accum being processed, input param: ") (display x) 
    (display " sum: ") (display sum) 
    (newline)
    sum
)


sum
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
sum
seq


(define y (stream-filter even? seq))

(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))


(stream-ref y 7)
sum

(display-stream z)
sum

; What is the value of sum after each of the above expressions is evaluated?

; What is the printed response to evaluating the stream-ref and display-stream
; expressions? Would these responses differ if we had implemented (delay <exp>)
; simply as (lambda () <exp>) without using the optimization provided by memo-
; proc ? Explain.
