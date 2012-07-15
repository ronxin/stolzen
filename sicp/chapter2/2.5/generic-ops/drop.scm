#lang scheme

(require "tags.scm")
(require "container.scm")



(define (project-complex x)
    (define (real-part)
        (get 'real-part 'complex)
    )
    ((real-part) x)
)


(define (project-rational x)
    (define (numer)
        (get 'numer 'rational)
    )
    ((numer) x)
)


(put 'project 'complex project-complex)
(put 'project 'rational project-rational)

(define (project x)
    (let 
        ((proc (get 'project (type-tag x))))
        (if proc
            (proc x)
            false
        )
    )
)

; let equal? be the generic equality predicate
(define (can-drop x)
    (let
        ((projection (project x)))
        (if projection
            (equal? x (raise projection))
            false
        )
    )
)

(define (drop x)
    (if (can-drop x)
        (drop (project x))
        x
    )
)


(provide (all-defined-out))