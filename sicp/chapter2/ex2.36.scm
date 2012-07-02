#lang scheme

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))
    )
)

; 2.36

(define (accumulate-n op initial sequences)
    (display sequences)
    (newline)
    (if (null? (car sequences))
        null
        (cons (accumulate op initial (map car sequences))
              (accumulate-n op initial (map cdr sequences)))
    )
)

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n + -26 s)
(accumulate-n + 0 (list (list 10 11)))
