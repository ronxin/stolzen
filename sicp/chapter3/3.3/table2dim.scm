#lang scheme

(require racket/mpair)

(define cons mcons)
(define car mcar)
(define (caar val) (car (car val)))
(define cdr mcdr)

(define list mlist)
(define set-cdr! set-mcdr!)
(define set-car! set-mcar!)

(define (lookup key-1 key-2 table)
    (let 
        ((subtable (assoc key-1 (cdr table))))

        (if subtable
            (let 
                ((record (assoc key-2 (cdr subtable))))

                (if record
                    (cdr record)
                    false
                )
            )
            false
        )
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

(define (insert! key-1 key-2 value table)
    (let 
        ((subtable (assoc key-1 (cdr table))))

        (if subtable
            (let 
                ((record (assoc key-2 (cdr subtable))))

                (if record
                    (set-cdr! record value)
                    (set-cdr! subtable
                            (cons (cons key-2 value) (cdr subtable)))
                )
            )

            (set-cdr! table (cons (list key-1 (cons key-2 value)) (cdr table))) 
        )
    )
    (void)
)

(define (make-table)
    (list '*table*)
)

(define t1 (make-table))
t1

(insert! 'key1 'key21 'value1 t1)
(insert! 'key1 'key22 'value2 t1)
(insert! 'key2 'key21 'value3 t1)
(insert! 'key2 'key22 'value4 t1)
t1

(lookup 'key1 'key22 t1)