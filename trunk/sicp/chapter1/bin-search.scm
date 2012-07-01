#lang scheme 

(define (println arg)
    (print arg) (newline)
)


(define (search f neg-point pos-point)
    (let
        ((midpoint (average neg-point pos-point)))

        (if (close-enough? neg-point pos-point)
            
            midpoint

            (let 
                ((test-value (f midpoint)))

                (cond 
                    ((positive? test-value) (search f neg-point midpoint))
                    ((negative? test-value) (search f midpoint pos-point))
                    (else midpoint)
                )
            )
        )
    )
)

(define (average a b)
    (/ (+ a b) 2.0)
) 

(define (close-enough? x y)
    (< (abs (- x y)) 0.001)
)

(define (half-interval-method f a b)
    (let 
        ((a-value (f a))
         (b-value (f b)))

        (cond
            ((and (negative? a-value) (positive? b-value))
             (search f a b))

            ((and (negative? b-value) (positive? a-value))
             (search f b a))

            (else (error "Values are not of opposite sign" a b))
        )
    )
)

(println (half-interval-method sin 2.0 4.0))


(define (always-positive x) 10)
(println (half-interval-method always-positive 2.0 4.0))