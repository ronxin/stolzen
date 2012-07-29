#lang scheme


(require racket/mpair)

(define front-ptr mcar)
(define rear-ptr mcdr)

(define (set-front-ptr! queue item)
    (set-mcar! queue item)
)

(define (set-rear-ptr! queue item)
    (set-mcdr! queue item)
)

(define (empty-queue? queue)
    (null? (front-ptr queue))
)

(define (make-queue)
    (mcons null null)
)

(define (front-queue queue)
    (if (empty-queue? queue)
        (error "FRONT called on empty queue" queue)
        (mcar (front-ptr queue))
    )
)

(define (insert-queue! queue item)
    (let 
        ((new-pair (mcons item null)))

        (cond
            ((empty-queue? queue) 
                (set-front-ptr! queue new-pair)
                (set-rear-ptr! queue new-pair)
                queue
            )
            (else
                (set-mcdr! (rear-ptr queue) new-pair)
                (set-rear-ptr! queue new-pair)
                queue
            )
        )
    )
)


(define (delete-queue! queue)
    (cond
        ((empty-queue? queue) (error "DELETE! called on empty queue"))
        (else
            (set-front-ptr! queue (mcdr (front-ptr queue)))
            queue
        )
    )
)

; 3.21

(define (print-queue queue)
    (display (front-ptr queue)) (newline)
)

(provide (all-defined-out))


