#lang scheme 

(define (square x)
    (* x x)
)

(define (sum-odd-squares tree)
    (cond
        ((null? tree) 0)
        ((not (pair? tree))
            (if (odd? tree) (square tree) 0)
        )
        (else 
            (+ (sum-odd-squares (car tree))
               (sum-odd-squares (cdr tree)))
        )
    )
)

(sum-odd-squares (list 1 2 3 (list 1 2 3)))

(define (fib-rec n)
    (cond
        ((= n 0) 0)
        ((= n 1) 1)
        (else 
            (+ (fib (- n 1)) (fib (- n 2))))
    )
)

(define (fib n)
    (define (fib-iter a b count)
        (if (= count 0) 
            a
            (fib-iter (+ a b) a (- count 1))
        )
    )
    (fib-iter 0 1 n)
)

(fib-rec 10)
(fib 10)

(define (even-fibs n)
    (define (next k)
        (if (> k n)
            null
            (let 
                ((f (fib k)))
                (if (even? f)
                    (cons f (next (+ k 1)))
                    (next (+ k 1))
                )
            )
        )
    )
    (next 0)
)

(even-fibs 10)

; filter

(define (filter predicate sequence)
    (cond 
        ((null? sequence) null)
        ((predicate (car sequence))
            (cons (car sequence) (filter predicate (cdr sequence))))
        (else 
            (filter predicate (cdr sequence)))
    )
)

(filter odd? (list 1 2 3 4))
(filter even? (list 1 2 3 4))

; reduce

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))
    )
)

(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
(accumulate cons null (list 1 2 3 4 5))


(define (enumerate-interval begin end)
    (if (> begin end)
        null
        (cons begin (enumerate-interval (+ begin 1) end))
    )
)

(enumerate-interval 2 7)


(define (enumerate-tree tree)
    (cond
        ((null? tree) null)
        ((pair? tree)
            (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree)))
        )
        (else (list tree))
    )
)

(enumerate-tree (list 1 2 3 (list 4 5 (list 6 7))))


(define (sum-odd-squares2 tree)
    (accumulate + 0 (map square (filter odd? (enumerate-tree tree))))
) 

(sum-odd-squares2 (list 1 2 3 (list 1 2 3)))

(define (even-fibs2 n)
    (accumulate cons null (filter even? (map fib (enumerate-interval 0 n))))
)

(even-fibs2 10)