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


; 2.83

(define (raise-number x)
    (attach-tag 'rational (contents x))
)

(define (raise-rational x)
    (attach-tag 'complex (contents x))
)

(put 'raise 'number raise-number)
(put 'raise 'rational raise-rational)

(define (raise x)
    ((get 'raise (type-tag x)) x)
)

(raise '(number . 10))
(raise '(rational . 10))


