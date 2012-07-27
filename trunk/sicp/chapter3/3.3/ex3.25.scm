#lang scheme

(require racket/mpair)

(define cons mcons)
(define car mcar)
(define (caar val) (car (car val)))
(define cdr mcdr)

(define list mlist)
(define set-cdr! set-mcdr!)
(define set-car! set-mcar!)


(define (assoc key records)
    (cond 
        ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else 
            (assoc key (cdr records)))
    )
)

(define (make-table)

    (define local-table (list '*table*))

     (define (lookup key)
        (let 
            ((record (assoc key (cdr local-table))))
            (if record
                (cdr record)
                false)
        )
    )

    (define (insert! key value)
        (let 
            ((record (assoc key (cdr local-table))))
            
            (if record
                (set-cdr! record value)
                (set-cdr! local-table
                    (cons (cons key value) (cdr local-table)))
            )
        )
        
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

(define (lookup keys table)
    ((table 'lookup-proc) keys)
)

(define (insert! keys value table)
    ((table 'insert-proc!) keys value)
)

(define t1 (make-table))
(t1 'list)

(insert! '(key1 key21) 'value1 t1)
(insert! '(key1 key22) 'value2 t1)
(insert! '(key2 key21) 'value3 t1)
(insert! '(key2 key22) 'value4 t1)
(t1 'list)

(lookup '(key1 key22) t1)