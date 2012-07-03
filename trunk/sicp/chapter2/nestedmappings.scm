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

(define (C2 n)
    (accumulate append null 
        (map 
            (lambda (i)
                (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1)))
            )
            (enumerate-interval 1 n)
        )
    )
)

(C2 5)

(define (flat seq)
    (accumulate append null seq)
)

(flat (list (list 1 2) (list 3 4 5)))

(define (flatmap proc seq)
    (flat (map proc seq))
)

(flatmap (lambda (x) (list x x x)) (list 1 2 3))

(define (prime? n)
    (define (square x)
        (* x x)
    )
    (define (expmod base exp mod)
        (cond 
            ((= exp 0) 1)
            ((even? exp) 
                (remainder (square (expmod base (/ exp 2) mod)) mod))
            (else
                (remainder (* base (expmod base (- exp 1) mod)) mod))
        )
    )
    (define (fermat-test n)
        (define (tryit a)
            (= (expmod a n n) a)
        )
        (define nextrand (random (- n 1)))
        (tryit (+ 1 nextrand))
    )
    (define (fast-prime? n times)
        (cond
            ((= times 0) #t)
            ((fermat-test n) (fast-prime? n (- times 1)))
            (else #f)
        )
    )
    (fast-prime? n 10)
)

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair)))
)

(define (make-pair-sum pair)
    (let
        ((first (car pair))
         (second (cadr pair)))
        (list first second (+ first second))
    )
)

(make-pair-sum (list 5 2))

(define (prime-sum-pairs n)
    (map make-pair-sum (filter prime-sum? (C2 n)))
)

(prime-sum-pairs 6)


(define (remove item sequence)
    (filter (lambda (x) (not (= x item))) sequence)
)

(remove 2 (list 1 2 3 4 2))

; permutations

(display "permutations") (newline)

; algorithm:
; for each element s in S do:
;   calc permutations for S without s
;   append s to each permutation in result


(define (permutations s)
    (if (null? s)
        (list null)
        (flatmap 
            (lambda (x) 
                (map (lambda (p) (cons x p)) (permutations (remove x s)))
            )
            s
        )
    )
)

; Notice how this strategy reduces the problem of generating
; permutations of S to the problem of generating the permutations of
; sets with fewer elements than S. In the terminal case, we work our way
; down to the empty list, which represents a set of no elements. For
; this, we generate (list nil), which is a sequence with one item,
; namely the set with no elements.

(permutations (list 1 2 3))