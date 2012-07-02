#lang scheme

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))
    )
)

; 2.34

; http://ru.wikipedia.org/wiki/%D0%A1%D1%85%D0%B5%D0%BC%D0%B0_%D0%93%D0%BE%D1%80%D0%BD%D0%B5%D1%80%D0%B0

(define (horner-eval x coeffs)
    (accumulate (lambda (c prev) (+ c (* prev x))) 0 coeffs)
)

(horner-eval 2 (list 1 3 0 5 0 1))


; 2.35

(define (count-leaves0 tree)
    (cond
        ((null? tree) 0)  
        ((not (pair? tree)) 1)
        (else (+ (count-leaves0 (car tree))
                 (count-leaves0 (cdr tree))))
    )
)

(define (count-leaves tree)
    (accumulate + 0 (map 
        (lambda (t) 
            (if (pair? t)
                (count-leaves t)
                1
            )
        ) 
        tree)
    )
)

(define x (cons (list 1 2) (list 3 4)))
(count-leaves0 (list x x))
(count-leaves (list x x))