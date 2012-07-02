#lang scheme

(define (fold-right op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (fold-right op initial (cdr sequence)))
    )
)

; 2.37

(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result 
            (iter (op result (car rest))
                (cdr rest))
        )
    )
    (iter initial sequence)
)

(define (fold-left2 op initial sequence)
    (if (null? sequence)
        initial
        (op (fold-left2 op initial (cdr sequence))
            (car sequence))
    )
)

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-left2 / 1 (list 1 2 3))

(fold-right list null (list 1 2 3))
(fold-left list null (list 1 2 3))
(fold-left2 list null (list 1 2 3))

; if (op a b) == (op b a) then both fold-left and fold-right return the same result