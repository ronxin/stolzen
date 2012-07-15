#lang scheme

(define (attach-tag type-tag contents)
    (if (or (eq? type-tag 'scheme-number) (number? contents))
        contents
        (list type-tag contents)
    )
)


(define (type-tag datum)
    (cond
        ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else
            (error "Bad tagged datum -- type-tag" datum))
    )
)

(define (contents datum)
    (cond
        ((number? datum) datum)
        ((pair? datum) (cadr datum))
        (else
            (error "Bad tagged datum -- contents" datum))
    )
)


(provide (all-defined-out))