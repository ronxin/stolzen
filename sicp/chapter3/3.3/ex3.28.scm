#lang scheme 


(define or-gate-delay 1)

(define (or-gate in1 in2 output)
    (define (or-action-procedure)
        (let 
            ((new-value
                (logical-or (get-signal in1) (get-signal in2))))
        
            (after-delay or-gate-delay
                (lambda () (set-signal! output new-value)))
        )
    )

    (add-action! in1 or-action-procedure)
    (add-action! in2 or-action-procedure)
    (void)
)


(define (logical-or s1 s2)
    (cond
        ((and (= s1 1) (= s2 1)) 1)
        ((and (= s1 0) (= s2 1)) 1)
        ((and (= s1 1) (= s2 0)) 1)
        ((and (= s1 0) (= s2 0)) 0)
        (else
            (error "Invalid signal" s))
    )
)
