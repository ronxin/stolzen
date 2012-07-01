#lang scheme

(define (map func items)
    (if (null? items)
        null
        (cons (func (car items)) (map func (cdr items)))
    )
)

(map (lambda (x) (* x x)) (list 1 2 3 4 5))

; 2.23

(define (for-each func items)
    (let
        ((head (car items))
         (tail (cdr items)))
        (cond 
            ((null? tail) (func head))
            (else 
                (func head) 
                (for-each func tail)
            )
        )
    )
)

(for-each (lambda (x) (display x) (newline)) (list 57 321 88))