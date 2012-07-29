#lang scheme

(define (and-gate in1 in2 output) (void))
(define (inverter in1 out1) (void))
(define (make-wire) (void))
(define (add-action! wire proc) (void))

; 3.29

(define (or-gate in1 in2 output)
    (let 
        ((not-in1 (make-wire))
         (not-in2 (make-wire))
         (im1 (make-wire)))

        (inverter in1 not-in1)
        (inverter in2 not-in2)
        (and-gate not-in1 not-in2 im1)
        (inverter im1 output)
        
        (void)
    )
)

; delay is delay of inverter + and-gate + inverter