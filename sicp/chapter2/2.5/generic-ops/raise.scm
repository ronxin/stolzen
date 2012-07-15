#lang scheme

(require "tags.scm")
(require "container.scm")



(define (raise x)
    (let 
        ((raise-proc (get 'raise (type-tag x))))
        (if raise-proc
            (raise-proc x)
            (error "raise procedure is not found -- RAISE" x)
        )
    )
)


(provide (all-defined-out))
