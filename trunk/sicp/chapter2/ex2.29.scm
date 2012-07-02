#lang scheme

; 2.29

(define (make-mobile left right)
    (list left right)
)

(define (make-branch length structure)
    (list length structure)
)

(define (branch-left mobile)
    (car mobile)
)

(define (branch-right mobile)
    (car (cdr mobile))
)

(define (branch-length branch)
    (car branch)
)

(define (branch-structure branch)
    (car (cdr branch))
)

(define branch1 (make-branch 1 10))
(define branch2 (make-branch 1 20))

(define mobile1 (make-mobile branch1 branch2))
(define branch3 (make-branch 2 mobile1))

(branch-length branch1)
(branch-structure branch1)
(branch-structure branch3)

(define (total-weight branch)
    (let 
        ((structure (branch-structure branch)))

        (if (pair? structure)
            (+ (total-weight (branch-left structure))
               (total-weight (branch-right structure)))
            structure
        )
    )
)

(total-weight branch3)

(define (torque branch)
    (let 
        ((len (branch-length branch))
         (weight (total-weight branch)))
        (* len weight)
    )
)

(define (balanced? mobile)
    (= (torque (branch-left mobile))
       (torque (branch-right mobile)))
)

(balanced? mobile1)

(define branch5 (make-branch 10 10))
(define branch6 (make-branch 10 10))

(balanced? (make-mobile branch5 branch6))

; d - change only selectors