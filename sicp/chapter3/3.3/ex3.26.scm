#lang scheme


(require racket/mpair)

(define cons mcons)
(define car mcar)
(define cdr mcdr)
(define (cddr val) (cdr (cdr val)))
(define (caar val) (car (car val)))
(define (cadr val) (car (cdr val)))
(define (caddr val) (car (cddr val)))
(define (cdddr val) (cdr (cddr val)))
(define (cadddr val) (car (cdddr val)))

(define list mlist)
(define set-cdr! set-mcdr!)
(define set-car! set-mcar!)


(define (key tree)
    (car tree)
)

(define (set-key! tree key)
    (set-car! tree key)
    (void)
)

(define (entry tree)
    (cadr tree)
)

(define (set-entry! tree entry)
    (set-car! (cdr tree) entry)
    (void)
)

(define (left-branch tree)
    (caddr tree)
)

(define (set-left-branch! tree branch)
   (set-car! (cddr tree) branch) 
   (void)
)

(define (right-branch tree)
    (cadddr tree)
)

(define (set-right-branch! tree branch)
   (set-car! (cdddr tree) branch) 
   (void)
)

(define (make-tree key entry left right)
    (list key entry left right)
)

(define (make-table comparator)

    (define table null)

    (define (c-eq? k1 k2)
        (= (comparator k1 k2) 0)
    )

    (define (c-less? k1 k2)
        (< (comparator k1 k2) 0)
    )

    (define (c-greater? k1 k2)
        (> (comparator k1 k2) 0)
    )

    (define (insert! k v node)
        (cond
            ((c-eq? k (key node)) 
                (set-entry! node v))
            ((c-less? k (key node)) 
                (if (null? (left-branch node))
                    (set-left-branch! node 
                        (make-tree k v null null))
                    (insert! k v (left-branch node))
                )
            )
            ((c-greater? k (key node)) 
                (if (null? (right-branch node))
                    (set-right-branch! node 
                        (make-tree k v null null))
                    (insert! k v (right-branch node))
                )
            )
        )
    )

    (define (insert!-inner key value)
        (cond
            ((null? table)
                (set! table (make-tree key value null null)))
            (else
                (insert! key value table))
        )
    )

    (define (lookup k node)
        (cond
            ((null? node) false)
            ((c-eq? k (key node)) 
                (entry node))
            ((c-less? k (key node)) 
                (lookup k (left-branch node)))
            ((c-greater? k (key node)) 
                (lookup k (right-branch node)))
        )
    )

    (define (lookup-inner k)
        (lookup k table)
    )

    (define (to-list table)
        (if (null? table) 
            (list)
            (mappend 
                (to-list (left-branch table))
                (cons (cons (key table) (entry table))
                      (to-list (right-branch table))))
        )
    )

    (define (to-list-inner)
        (to-list table)
    )

    (define (dispatch m)
        (cond
            ((eq? m 'lookup) lookup-inner)
            ((eq? m 'insert!) insert!-inner)
            ((eq? m 'to-list) to-list-inner)
            ((eq? m 'table) table)
            (else
                (error "Wrong message" m))
        )
    )

    dispatch
)

(define (number-comparator n1 n2)
    (- n1 n2)
)

(define t1 (make-table number-comparator))

(define (lookup-table table key)
    ((table 'lookup) key)
)

(define (insert-table! table key value)
    ((table 'insert!) key value)
)

((t1 'to-list))
(insert-table! t1 5 'five)
((t1 'to-list))

(insert-table! t1 2 'two)
((t1 'to-list))


(insert-table! t1 7 'seven)
(t1 'table)
((t1 'to-list))
