#lang scheme

(require rackunit)
(require "constraints.scm")

(define (averager in1 in2 output)
    (let 
        ((u    (make-connector))
         (half (make-connector)))

        (adder in1 in2 u)
        (multiplier u half output)
        
        (constant 0.5 half)
        (void)
    )
)

(define in1    (make-connector))
(define in2    (make-connector))
(define output (make-connector))

(probe "average in1"    in1)
(probe "average in2"    in2)
(probe "average output" output)

(averager in1 in2 output)

(set-value! in1 212 'user)
(set-value! in2 1   'user)


(check-equal? (get-value output) 106.5)