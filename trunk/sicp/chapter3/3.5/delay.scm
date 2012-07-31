#lang scheme

(define-syntax stream-cons
    (syntax-rules ()
        ((stream-cons x y)
            (cons x (delay y))
        )
    )
)

(define (stream-car stream) 
    (car stream)
)

(define (stream-cdr stream) 
    (force (cdr stream))
)

(stream-cons 1 (stream-cons 2 (stream-cons 3 null)))
; here the first argument has to be delayed, but it execured right away
; so stream-cons has to be defined as macro
