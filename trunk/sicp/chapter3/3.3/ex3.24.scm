#lang scheme

(require racket/mpair)

(define cons mcons)
(define car mcar)
(define (caar val) (car (car val)))
(define cdr mcdr)

(define list mlist)
(define set-cdr! set-mcdr!)
(define set-car! set-mcar!)

; 3.24


(define (make-table same-key?)

    (define local-table (list '*table*))

    ; 3.24
    (define (assoc key records)
        (cond 
            ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else 
                (assoc key (cdr records)))
        )
    )

    (define (lookup key-1 key-2)
        (let 
            ((subtable (assoc key-1 (cdr local-table))))

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

    (define (insert! key-1 key-2 value)
        (let 
            ((subtable (assoc key-1 (cdr local-table))))

            (if subtable
            
                (let 
                    ((record (assoc key-2 (cdr subtable))))
                    
                    (if record
                        (set-cdr! record value)
                        (set-cdr! subtable 
                            (cons (cons key-2 value) (cdr subtable))))
                )

                (set-cdr! local-table
                    (cons (list key-1 (cons key-2 value)) (cdr local-table)))))

        (void)
    )

    (define (dispatch m)
        (cond 
            ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'list) local-table)
            (else 
                (error "Unknown operation -- TABLE" m)))
    )

    dispatch
)

(define (lookup key-1 key-2 table)
    ((table 'lookup-proc) key-1 key-2)
)

(define (insert! key-1 key-2 value table)
    ((table 'insert-proc!) key-1 key-2 value)
)

(define t1 (make-table equal?))
(t1 'list)

(insert! 'key1 'key21 'value1 t1)
(insert! 'key1 'key22 'value2 t1)
(insert! 'key2 'key21 'value3 t1)
(insert! 'key2 'key22 'value4 t1)
(t1 'list)

(lookup 'key1 'key22 t1)