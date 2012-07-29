#lang scheme

(require "zip.scm")
(require "times.scm")

(define (make-wire) (void))
(define (full-adder in1 in2 c-in sum c-out) (void))

; ex 3.30

(define (ripple-carry-adder numbers1 numbers2 results c-out)
    (let 
        ((len (length numbers1)))

        ; remainders - set of {C} in the figure
        ; c-out of i-th full-adder is c-in for (i - 1)-th full-adder
        (let 
            ((remainders (times (- len 1) make-wire))
             (c0 (make-wire)))
            ; assuming make-wire sets signal to zero, so c0 is zero

            (let 
                ((c-ins (append remainders (list c0)))
                 (c-outs (cons c-out remainders)))

                ; passing each element of numbers1 numbers2 c-ins results c-outs to full-adder
                (for-each 
                    (lambda (params) (apply full-adder params)) 
                    (zip numbers1 numbers2 c-ins results c-outs))
                (void)
            )
        )
    )
)
