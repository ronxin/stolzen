#lang scheme

(define (square x)
    (* x x)
)

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))
    )
)

; 2.33

(define (map2 p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) null sequence)
)

(map square (list 1 2 3 4 5))
(map2 square (list 1 2 3 4 5))

(define (append2 seq1 seq2)
    (accumulate cons seq2 seq1)
)

(append (list 1 2) (list 3 4))
(append2 (list 1 2) (list 3 4))

(define (length2 seq)
    (accumulate (lambda (x y) (+ y 1)) 0 seq)
)

(length (list 1 2 3 4))
(length2 (list 1 2 3 4))