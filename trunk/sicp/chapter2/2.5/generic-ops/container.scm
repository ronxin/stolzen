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

(provide put get)