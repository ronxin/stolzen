#lang scheme

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))
    )
)

(define (enumerate-interval begin end)
    (if (> begin end)
        null
        (cons begin (enumerate-interval (+ begin 1) end))
    )
)

(define (flat seq)
    (accumulate append null seq)
)

(define (flatmap proc seq)
    (flat (map proc seq))
)


; 2.40

(define (unique-pairs n)
    (flatmap 
        (lambda (i)
            (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1)))
        )
        (enumerate-interval 1 n)
    )
)

(unique-pairs 5)


; 2.41

(define (C3 n)
    (flatmap 
        (lambda (i)
            (flatmap 
                (lambda (j) 
                    (map (lambda (k) (list k j i)) (enumerate-interval 1 (- j 1)))
                ) 
                (enumerate-interval 1 (- i 1)))
        )
        (enumerate-interval 1 n)
    )
)

(C3 5)

(define (sum seq)
    (accumulate + 0 seq)
)

(define (sum-equal-to? value)
    (lambda (seq) 
        (= value (sum seq))
    )
)

(define (triples-with-sum n s)
    (filter (sum-equal-to? s) (C3 n))
)

(triples-with-sum 5 10)