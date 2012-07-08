#lang scheme 

(require racket/trace)

(define (element-of-set? x set)
    (cond
        ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else
            (element-of-set? x (cdr set)))
    )
)

(element-of-set? 1 '(2 3 4))
(element-of-set? 1 '(1 2 3))

(define (intersection-set set1 set2)
    (if (or (null? set1) (null? set2))
        null
        (let
            ((head1 (car set1)) (head2 (car set2))
             (tail1 (cdr set1)) (tail2 (cdr set2)))

            (cond
                ((= head1 head2) 
                    (cons head1 (intersection-set tail1 tail2)))
                ((< head1 head2) 
                    (intersection-set tail1 set2))
                ((> head1 head2) 
                    (intersection-set set1 tail2))
            )
        )
    )
)

(intersection-set '(1 2 3) '(2 3 4))

; 2.61

(define (adjoint-set x set)
    (cond
        ((null? set) 
            (list x))
        ((= x (car set)) 
            set)
        ((> x (car set))
            (cons (car set) (adjoint-set x (cdr set))))
        ((< x (car set))
            (cons x set))
    )
)

(adjoint-set 2 '(1 3 4))
(adjoint-set 3 '(1 3 4))
(adjoint-set 6 '(1 3 4))
(adjoint-set 6 '())

; 2.62


(define (union-set set1 set2)
   (cond
        ((null? set1) 
            set2)
        ((null? set2)
            set1)
        ((= (car set1) (car set2))
            (cons 
                (car set1) 
                (union-set (cdr set1) (cdr set2))
            )
        )
        ((< (car set1) (car set2))
            (cons 
                (car set1) 
                (union-set (cdr set1) set2)
            )
        )
        ((> (car set1) (car set2))
            (cons 
                (car set2) 
                (union-set set1 (cdr set2))
            )
        )
    )
)

(union-set '(2 3 6) '(1 2 3 4 5))





