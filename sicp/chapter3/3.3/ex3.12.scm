#lang scheme

(require racket/mpair)

(define cons mcons)
(define car mcar)
(define cdr mcdr)
(define list mlist)
(define set-cdr! set-mcdr!)
(define set-car! set-mcar!)


(define (append x y)
    (if (null? x)
        y
        (cons (car x) (append (cdr x) y))
    )
)


(define (last-pair x)
    (if (null? (cdr x))
        x
        (last-pair (cdr x))
    )
)

(define (append! x y)
    (if (null? x)
        y
        (begin 
            (set-cdr! (last-pair x) y)
            x)
    )
)

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
(cdr x)
; {b}
(define w (append! x y))
w
(cdr x)
; {b c d}