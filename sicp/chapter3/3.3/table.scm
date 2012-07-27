#lang scheme

(require racket/mpair)

(define cons mcons)
(define car mcar)
(define (caar val) (car (car val)))
(define cdr mcdr)

(define list mlist)
(define set-cdr! set-mcdr!)
(define set-car! set-mcar!)

(define (lookup key table)
    (let 
        ((record (assoc key (cdr table))))
        (if record
            (cdr record)
            false)
    )
)

(define (assoc key records)
    (cond 
        ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else 
            (assoc key (cdr records)))
    )
)

(define (insert! key value table)
    (let 
        ((record (assoc key (cdr table))))
        
        (if record
            (set-cdr! record value)
            (set-cdr! table
                (cons (cons key value) (cdr table)))
        )
    )
    
    (void)
)

(define (make-table)
    (list '*table*)
)

; (define t1 (make-table))
; t1

; (insert! 'a 'value t1)
; (insert! 'b 'value2 t1)
; (insert! 'c 'value3 t1)
; t1

; (lookup 'a t1)

(provide make-table insert! lookup)