#lang scheme

(require rackunit)

(require "constraints.scm")


(define (square x)
    (* x x)
)

(define (squarer in-square out-square)
    (define (process-new-value)
        (cond
            ((has-value? out-square) 
                (if (< (get-value out-square) 0)
                    (error "square less than 0 -- SQUARER" (get-value out-square))
                    (set-value! in-square 
                        (sqrt (get-value out-square)) 
                        me-squarer)
                )
            )
            ((has-value? in-square)
                (set-value! out-square
                    (square (get-value in-square))
                    me-squarer)
            )
        )
    )

    (define (process-forget-value) 
        (forget-value! in-square  me-squarer)
        (forget-value! out-square me-squarer)
    )

    (define (me-squarer request)
        (cond 
            ((eq? request 'I-have-a-value)  
                (process-new-value))
            ((eq? request 'I-lost-my-value) 
                (process-forget-value))
            (else 
                (error "Unknown request -- SQUARER" request))
        )
    )

    (connect in-square  me-squarer)
    (connect out-square me-squarer)

    (void)
)


(define in-square  (make-connector))
(define out-square (make-connector))

(probe "in-square" in-square)
(probe "out-square" out-square)

(squarer in-square out-square)


(set-value! in-square 5 'user)
(check-equal? (get-value out-square) 25)

(in-square 'informant)
(out-square 'informant)

(newline)

(forget-value! in-square 'user)

(check-equal? (has-value? in-square) false)
(check-equal? (has-value? out-square) false)

(get-value in-square)
(get-value out-square)

(newline)

(set-value! out-square 16 'user)
(check-equal? (get-value in-square) 4)

; 3.36
; http://wqzhang.wordpress.com/2009/07/29/sicp-exercise-3-36/