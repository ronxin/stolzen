#lang scheme 

(require racket/trace)

; for testing 

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

(define coercion-storage (container))
(define put-coercion (coercion-storage 'put))
(define get-coercion (coercion-storage 'get))

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

; 2.82

(define (identity x) x)

(define (all-satisfy predicate seq)
    (foldr (lambda (el res) (and (predicate el) res)) true seq)
)

(define (eq-with-head? seq)
    (if (null? seq) 
        true
        (let 
            ((head (car seq))
             (tail (cdr seq)))
            (all-satisfy (lambda (el) (eq? el head)) tail)
        )
    )
)

(eq-with-head? '(a a a))
(not (eq-with-head? '(a a b)))

(define (contains? seq el)
    (cond
        ((null? seq) false)
        ((equal? el (car seq)) true)
        (else
            (contains? (cdr seq) el))
    )
)

(contains? (list 1 2 false) false)
(not (contains? (list 1 2 false) true))



(not (equal? (and identity identity) false))
(not (and identity false))

(define (coerce-procedures seq)
    (define (coerce-procedures-inner el seq-symb)
        (define (get-coerce-procedure x)
            (if (eq? el x)
                identity
                (get-coercion x el)
            )
        )

        (if (null? seq-symb)
            null
            (let 
                ((res (map get-coerce-procedure seq-symb)))
                (if (and res (not (contains? res false)))
                    res
                    false
                )
            )
        )
    )

    (define (procedures prev curr last)
        (let 
            ((beg (coerce-procedures-inner curr prev))
             (end (coerce-procedures-inner curr last)))

            (if (and beg end)
                (append (reverse beg) (cons identity end))
                (if (null? last)
                    false
                    (procedures (cons curr prev) (car last) (cdr last))
                )
            )
        )
    )

    (procedures null (car seq) (cdr seq))
)


(define (number->complex x) (cons 'complex x))
(define (number->rational x) (cons 'rational x))
(define (rational->complex x) (cons 'complex x))


(put-coercion 'number 'complex number->complex)
(put-coercion 'number 'rational number->rational)
(put-coercion 'rational 'complex rational->complex)

(equal? (coerce-procedures '(number number rational)) 
    (list number->rational number->rational identity))
(equal? (coerce-procedures '(number complex rational))
    (list number->complex identity rational->complex))

(define (transform transformers transormables)
    (if (null? transformers)
        null
        (let
            ((transformer (car transformers))
             (transormable (car transormables)))
            (cons (transformer transormable) 
                  (transform (cdr transformers) (cdr transormables)))
        )
    )
)

(define transformers (coerce-procedures '(number complex rational)))
(define transormables '(1 2 3))
(transform transformers transormables)


(define (apply-generic-test op . args)
    (list op args)
)

(apply apply-generic-test (cons 'op (transform transformers transormables)))

(define (apply-generic op . args)
    (define (try-coerce type-tags args)
        (if (eq-with-head? type-tags)
            (error "Types are equal, can't coerce" (list op type-tags args)) 
            (let 
                ((procs (coerce-procedures args)))
                (if procs
                    (apply apply-generic (cons op (transform procs args)))
                    (error "Can't coerce") 
                )
            )
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