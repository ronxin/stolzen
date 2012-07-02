#lang scheme

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))
    )
)

(define (accumulate-n op initial sequences)
    (if (null? (car sequences))
        null
        (cons (accumulate op initial (map car sequences))
              (accumulate-n op initial (map cdr sequences)))
    )
)

; 2.37


(define (dot-product v w)
    (accumulate + 0 (map * v w))
)

(dot-product (list 0 1 2 3 4) (list 0 1 2 3 4))

(define (matrix-vector m v)
    (map (lambda (row) (dot-product v row)) m)
)

(matrix-vector (list (list 1 2 3) (list 2 3 4)) (list 1 2 3))

(define (transpose mat)
    (accumulate-n cons null mat)
)

(transpose (list (list 1 2 3 4) (list 2 3 4 5) (list 3 4 5 6)))

(define (matrix-matrix m n)
    (let
        ((cols (transpose n)))
        (map (lambda (row) (matrix-vector cols row)) m)
    )
)

; http://bmanolov.free.fr/matrixcalc.php for checking

(matrix-matrix (list (list 1)) (list (list 9)))

(define a (list (list 1 2 3 4) (list 2 3 4 5) (list 3 4 5 6)))
a

(matrix-matrix a (transpose a))

(define mat1 (list (list 1 9 3) (list 2 2 3) (list 0 12 8) (list 5 2 3)))
(define mat2 (list (list 12 5) (list 9 10) (list 8 5)))
(matrix-matrix mat1 mat2)

; expect
; (117 110) (66 45) (172 160) (102 60)


(define (verticalize vector)
    (map list vector)
)

(verticalize (list 1 2 3))
(transpose (list (list 1 2 3)))
(transpose (verticalize (list 1 2 3)))