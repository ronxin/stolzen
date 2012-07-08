#lang scheme 

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))
    )
)

(define (entry tree)
    (car tree)
)

(define (left-branch tree)
    (cadr tree)
)

(define (right-branch tree)
    (caddr tree)
)

(define (make-tree entry left right)
    (list entry left right)
)

(define (element-of-set? x set)
    (cond
        ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set)) 
            (element-of-set? x (left-branch set)))
        ((> x (entry set)) 
            (element-of-set? x (right-branch set)))
    )
)

(define (adjoint-set x set)
    (cond
        ((null? set) (make-tree x null null))
        ((= x (entry set)) set)
        ((< x (entry set)) 
            (make-tree 
                (entry set)
                (adjoint-set x (left-branch set))
                (right-branch set)
            )
        )
        ((> x (entry set)) 
            (make-tree 
                (entry set)
                (left-branch set)
                (adjoint-set x (right-branch set))
            )
        )
    )
)

(define (fill-set seq)
    (accumulate adjoint-set null seq)
)

(define s (fill-set '(9 7 2 3 4 6 1 8 5)))
(element-of-set? 9 s)
(element-of-set? 10 s)

; 2.63

(define (tree-list-1 tree)
    (if (null? tree)
        null
        (append 
            (tree-list-1 (left-branch tree))
            (cons (entry tree)
                  (tree-list-1 (right-branch tree)))
        )
    )
)

(tree-list-1 s)

(define (tree-list-2 tree)
    (define (copy-to-list tree result)
        (if (null? tree)
            result
            (copy-to-list
                (left-branch tree)
                (cons (entry tree)
                      (copy-to-list (right-branch tree) result))
            )
        )
    )
    (copy-to-list tree null)
)

(tree-list-2 s)

(define tree2-16-1 (fill-set '(1 5 3 11 9 7)))
tree2-16-1
(tree-list-1 tree2-16-1)
(tree-list-2 tree2-16-1)


(define tree2-16-2 (fill-set '(11 5 9 7 1 3)))
tree2-16-2
(tree-list-1 tree2-16-2)
(tree-list-2 tree2-16-2)

(define tree2-16-3 (fill-set '(1 7 11 3 9 5)))
tree2-16-3
(tree-list-1 tree2-16-3)
(tree-list-2 tree2-16-3)

; a. Yes, same result; list are above
; b. second is iterative and doesn't use append, which leads to O(n), while 
; the first tends to O(n log n)

; 2.64

(define (partial-tree elts n)
    (if (= n 0)
        (cons null elts)
        (let
            ((left-size (quotient (- n 1) 2)))
            (let
                ((left-result (partial-tree elts left-size)))
                (let
                    ((left-tree (car left-result))
                     (non-left-elts (cdr left-result))
                     (right-size (- n (+ left-size 1))))
                    (let
                        ((this-entry (car non-left-elts))
                         (right-result (partial-tree (cdr non-left-elts) right-size)))
                        (let
                            ((right-tree (car right-result))
                             (remaining-elts (cdr right-result)))
                            (cons (make-tree this-entry left-tree right-tree) remaining-elts)
                        )
                    )
                )
            )
        )
    )
)

(define (list-tree elements)
    (car (partial-tree elements (length elements)))
)


(list-tree '(1 2 3 4 5 6 7 8 9))

; a. partial-tree gets a sorted list, takes first (n - 1) / 2 element for the left
; tree, element in the middle as the head and the rest as the right tree.
; it returns a cons of tree plus remaining elemens for the last can be processed 
; recurcively 

; tree should be as following:
;              5
;          1       9
;            3   7   11

(list-tree '(1 3 5 7 9 11))

; b. O (n)


; 2.65

(define (intersection-set-merge set1 set2)
    (if (or (null? set1) (null? set2))
        null
        (let
            ((head1 (car set1)) (head2 (car set2))
             (tail1 (cdr set1)) (tail2 (cdr set2)))

            (cond
                ((= head1 head2) 
                    (cons head1 (intersection-set-merge tail1 tail2)))
                ((< head1 head2) 
                    (intersection-set-merge tail1 set2))
                ((> head1 head2) 
                    (intersection-set-merge set1 tail2))
            )
        )
    )
)

(define (intersectoin-set set1 set2)
    (let 
        ((res 
            (intersection-set-merge 
                (tree-list-2 set1)
                (tree-list-2 set2))))
        (list-tree res)
    )
)

(tree-list-2 tree2-16-1)
(tree-list-2 s)
(tree-list-2 (intersectoin-set tree2-16-1 s))

(define (union-set-merge set1 set2)
   (cond
        ((null? set1) 
            set2)
        ((null? set2)
            set1)
        ((= (car set1) (car set2))
            (cons 
                (car set1) 
                (union-set-merge (cdr set1) (cdr set2))
            )
        )
        ((< (car set1) (car set2))
            (cons 
                (car set1) 
                (union-set-merge (cdr set1) set2)
            )
        )
        ((> (car set1) (car set2))
            (cons 
                (car set2) 
                (union-set-merge set1 (cdr set2))
            )
        )
    )
)

(define (union-set set1 set2)
    (let 
        ((res 
            (union-set-merge 
                (tree-list-2 set1)
                (tree-list-2 set2))))
        (list-tree res)
    )
)

(tree-list-2 (union-set tree2-16-1 s))