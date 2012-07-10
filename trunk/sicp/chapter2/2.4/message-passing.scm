#lang scheme

(define (square x)
    (* x x)
)

(define (make-from-real-imag x y)
    (define (dispatch-real-img op)
        (cond
            ((eq? op 'real-part) x)
            ((eq? op 'imag-part) y)
            ((eq? op 'magnitude) 
                (sqrt (square x) (square y))
            )
            ((eq? op 'angle) (atan y x))
            (else
                (error "Unknown op - make-from-real-imag" op))
        )
    )

    dispatch-real-img
)

(define (apply-generic op arg)
    (arg op)
)

; 2.75

(define (make-from-mag-ang mag ang)
    (define (dispatch-mag-ang op)
        (cond
            ((eq? op 'real-part) (* mag ang))
            ((eq? op 'imag-part) (* mag (cos ang)))
            ((eq? op 'magnitude) mag)
            ((eq? op 'angle) ang)
            (else
                (error "Unknown op - make-from-real-imag" op))
        )
    )

    dispatch-mag-ang
)

; 2.76.  

; As a large system with generic operations evolves, new types of data objects
; or new operations may be needed. For each of the three strategies -- generic
; operations with explicit dispatch, data-directed style, and message-
; passing-style -- describe the changes that must be made to a system in order
; to add new types or new operations.


; __generic operations with explicit dispatch__
; a set of operations must be created, checking for type-tag, and in each generic operaion 
; a new condition is to be added

; __data-directed style___
; new procedure which installs the operation and exports it using put, new type-tag

; __message-passing-style__
; new object with a dispatcher

; Q: Which organization would be most appropriate for a system in which new types
; must often be added?
; A: message-passing-style 

; Q: Which would be most appropriate for a system in which new operations must
; often be added?
; A: data-directed style
