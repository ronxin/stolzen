#lang scheme

(require racket/mpair)

(define cons mcons)
(define car mcar)
(define cdr mcdr)
(define list mlist)
(define set-cdr! set-mcdr!)
(define set-car! set-mcar!)

(define (mystery x)
    (define (loop x y)
        (if (null? x)
            y
            (let 
                ((temp (cdr x)))
                (set-cdr! x y)
                (loop temp x)
            )
        )
    )

    (loop x (list))
)

; mystery reverses a list

(mystery (list 'a 'b 'c 'd))