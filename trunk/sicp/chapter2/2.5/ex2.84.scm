#lang scheme


(define (container)
    (define hash (make-hash))

    (define (put op type item)
        (hash-set! hash (cons op type) item)
    )

    (define (get op type)
        (let
            ((key (cons op type)))
            (if (hash-has-key? hash key)
                (hash-ref hash key)
                false
            )
        )
    )

    (lambda (name) 
        (cond
            ((eq? name 'get) get)
            ((eq? name 'put) put)
            ((eq? name 'hash) hash)
        )
    )
)

(define storage (container))

(define put (storage 'put))
(define get (storage 'get))


(define (attach-tag type-tag contents)
    (cons type-tag contents)
)

(define (type-tag datum)
    (if (pair? datum)
        (car datum)
        (error "Bad tagged datum -- type-tag" datum)
    )
)

(define (contents datum)
    (if (pair? datum)
        (cdr datum)
        (error "Bad tagged datum -- contents" datum)
    )
)


; 2.84

(define inheritance-tower '(complex rational number))

(define (index-tower tower)
    (define (index-inner seq current-index)
        (if (null? seq)
            null
            (cons (cons (car seq) current-index)
                  (index-inner (cdr seq) (+ 1 current-index) ))
        )
    )
    (index-inner tower 0)
)

; making pairs of (type - ordinal number), i.e complex: 0, rational: 1, etc
(define inheritance-indexes (index-tower inheritance-tower))

(define (get-index type indexes)
    (cond
        ((null? indexes) -1)
        ((eq? type (car (car indexes))) (cdr (car indexes)))
        (else
            (get-index type (cdr indexes)))
    )
)

(equal? 0 (get-index 'complex inheritance-indexes))
(equal? 1 (get-index 'rational inheritance-indexes))
(equal? 2 (get-index 'number inheritance-indexes))


(define (get-type ord indexes)
    (cond
        ((null? indexes) false)
        ((= ord (cdr (car indexes))) (car (car indexes)))
        (else
            (get-type ord (cdr indexes)))
    )    
)

(equal? 'complex (get-type 0 inheritance-indexes))
(equal? 'rational (get-type 1 inheritance-indexes))
(equal? 'number (get-type 2 inheritance-indexes))

(define (find-superest-parent seq inheritance-indexes)
    (define (index type)
        (get-index type inheritance-indexes)
    )
    (get-type 
        (apply min (map index seq))
        inheritance-indexes
    )
)

(equal? 'rational (find-superest-parent '(number number rational) inheritance-indexes))
(equal? 'number (find-superest-parent '(number number number) inheritance-indexes))
(equal? 'complex (find-superest-parent '(number complex rational) inheritance-indexes))


(define (apply-generic2 op . args)
    (define (coerce type1 a1 type2 a2)
        (let 
            ((t1->t2 (get-coercion type1 type2))
             (t2->t1 (get-coercion type2 type1)))

            (cond 
                (t1->t2
                    (apply-generic2 op (t1->t2 a1) a2))
                (t2->t1
                    (apply-generic2 op a1 (t2->t1 a2)))
                (else
                    (error "No method for these types" (list op type1 type2)))
            )
        )
    )

    (define (try-coerce type-tags args)
        (if (= (length args) 2)
            (let 
                ((type1 (car type-tags))
                 (type2 (cadr type-tags))
                 (a1 (car args))
                 (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "Types are equal, can't coerce" (list op type-tags args)) 
                    (coerce type1 a1 type2 a2)  
                )
            )
            (error "No method for these types" (list op type-tags)) 
        )
    )

    (let 
        ((type-tags (map type-tag args)))
        (let 
            ((proc (get op type-tags)))
            
            (if proc
                (apply proc (map contents args))
                (try-coerce type-tags args)
            )
        )
    )
)