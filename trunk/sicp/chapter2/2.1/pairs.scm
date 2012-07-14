#lang scheme

(define (pair-closure x y)
    (lambda (n)
        (cond
            ((= n 0) x)
            ((= n 1) y)
            (else (error "Argument not 0 or 1"))
        )
    )
)

(define (first-closure p)
    (p 0)
)

(define (second-closure p)
    (p 1)
)

(define (print-pair-closure p)
    (display (first-closure p))
    (display ":")
    (display (second-closure p))
    (newline)
)

(print-pair-closure (pair-closure 1 2))

; 2.4

; возвращает функцию, у которой аргумент - функция, принимающая элементы пары
(define (pair first-el second-el)
    (lambda (element-func) (element-func first-el second-el))
)

; пара - функция, вызываем ее с аргументом функцией, принимающей оба аргумента, 
; возвращающей первый аргумет. так же и для второго
(define (first pair)
    (pair (lambda (first-el second-el) first-el))
)

(define (second pair)
    (pair (lambda (first-el second-el) second-el))
)


(define (print-pair p)
    (display (first p))
    (display ":")
    (display (second p))
    (newline)
)

(print-pair (pair 1 2))
