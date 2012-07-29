#lang scheme

(require rackunit)

(define (zip . lists)
    (define (zip-inner params)
        (if (null? (car params))
            null
            (cons (apply list (map car params))
                  (zip-inner  (map cdr params)))
        )
    )
    (zip-inner lists)
)

(check-equal? 
    (zip '(1 2 3) '(4 5 6) '(7 8 9))
    '((1 4 7) (2 5 8) (3 6 9)))

(provide zip)