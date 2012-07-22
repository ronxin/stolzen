#lang scheme

; http://www.physics.orst.edu/~rubin/nacphy/ComPhys/MONTE/mc3/node2.html
; http://en.wikipedia.org/wiki/Linear_congruential_generator

(define (new-random seed)
    (define a 1664525)
    (define c 1013904223)
    (define m (arithmetic-shift 1 32))

    (define (next)
        (begin
            (set! seed (modulo (+ (* a seed) c) m))
            seed
        )
    )

    (define (reset new-value)
        (set! seed new-value)
    )

    (define (dispatch command)
        (cond
            ((eq? command 'generate) next)
            ((eq? command 'reset) reset)
            (else
                (error "Unknown command -- dispatch" command))
        )
    )

    dispatch
)


(define rand (new-random 31))

((rand 'generate))
((rand 'generate))
((rand 'generate))

((rand 'reset) 31)

((rand 'generate))
((rand 'generate))
((rand 'generate))