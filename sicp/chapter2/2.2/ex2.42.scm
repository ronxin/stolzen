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

; (define (repeat el n)
;     (define (repeat-iter count tail)
;         (if (= count 0)
;             tail
;             (repeat-iter (- count 1) (cons el tail))
;         )
;     )
;     (repeat-iter n null)
; )
; (repeat empty-cell 8)

; 2.42

(define empty-board null)

(define (position row col)
    (cons row col)
)

(define (position-row pos)
    (car pos)
)

(define (position-col pos)
    (cdr pos)
)

; for safe? 

(define (not-same-row? pos1 pos2)
    (not (= (position-row pos1) (position-row pos2)))
)

(define (not-same-col? pos1 pos2)
    (not (= (position-col pos1) (position-col pos2)))
)

(define (not-same-diagonal? pos1 pos2)
    (not (=
        (abs (- (position-row pos1) (position-row pos2)))
        (abs (- (position-col pos1) (position-col pos2)))
    ))
)

(define (all? el predicate seq)
    (define (and-f value1 value2) ; cant pass to accumulate otherwise
        (and value1 value2)
    )
    (define (check predicate el)
        (lambda (x)
            (predicate x el)
        )
    )
    (accumulate and-f true (map (check predicate el) seq))
)

(define (each-with-each predicate seq)
    (if (null? (cdr seq))
        true
        (and (all? (car seq) predicate (cdr seq))
             (each-with-each predicate (cdr seq)))
    )
)

(define (head-with-tail predicate seq) 
    (if (null? (cdr seq))
        true
        (all? (car seq) predicate (cdr seq))
    )
)

(define (safe? positions)
    ; enough to compare only head
    ; for only it's added in add-queen
    (and (head-with-tail not-same-row? positions)
         (head-with-tail not-same-col? positions)
         (head-with-tail not-same-diagonal? positions)) 
)

(define (adjoint-position new-row k rest-of-queens)
    (cons (position new-row k) rest-of-queens)
)

(define (queens board-size)
    (define (add-queen k)
        (flatmap 
            (lambda (rest-of-queens) 
                (map 
                    (lambda (new-row)
                        (adjoint-position new-row k rest-of-queens)
                    )
                    (enumerate-interval 1 board-size)
                )
            )
            (queens-cols (- k 1))
        )
    )
    (define (queens-cols k)
        (if (= k 0)
            (list empty-board)
            (filter safe? (add-queen k))
        )
    )
    (queens-cols board-size)
)

; http://oeis.org/A000170
(= (length (queens 1)) 1)
(= (length (queens 2)) 0)
(= (length (queens 3)) 0)
(= (length (queens 4)) 2)
(= (length (queens 5)) 10)
(= (length (queens 6)) 4)
(= (length (queens 7)) 40)
(= (length (queens 8)) 92)

; 2.43 
; because for each iteration it will calculate all the previous data
; over and over again 