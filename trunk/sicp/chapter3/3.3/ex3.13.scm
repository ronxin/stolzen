#lang scheme

(require racket/mpair)

(define cons mcons)
(define car mcar)
(define cdr mcdr)
(define list mlist)
(define set-cdr! set-mcdr!)
(define set-car! set-mcar!)

(define (last-pair x)
    (if (null? (cdr x))
        x
        (last-pair (cdr x))
    )
)

(define (make-cycle x)
    (set-cdr! (last-pair x) x)
    x
)


(define z (make-cycle (list 'a 'b 'c)))

; (last-pair z)
; infinitive loop