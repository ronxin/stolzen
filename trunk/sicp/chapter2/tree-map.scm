#lang scheme 

(define (scale-tree tree factor)
    (map (lambda (subtree)
            (if (pair? subtree)
                (scale-tree subtree factor)
                (* subtree factor)
            ))
        tree
    )
)

(define x (list 1 (list 2 (list 3 4))))
(scale-tree x 10)

(define (map-tree func tree)
    (map (lambda (subtree)
            (if (pair? subtree)
                (map-tree func subtree)
                (func subtree)
            ))
        tree
    )
)

(map-tree (lambda (elem) (* elem 10)) x)

; 2.30, 2.31

(define (square-tree tree)
    (map-tree (lambda (x) (* x x)) tree)
)

(square-tree 
    (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)
    )
)