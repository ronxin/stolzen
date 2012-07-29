#lang scheme

(define (times n proc)
    (define (times-inner res n)
        (if (= n 0)
            res
            (times-inner (cons (proc) res) (- n 1))
        )
    )
    (times-inner null n)
)

(provide times)