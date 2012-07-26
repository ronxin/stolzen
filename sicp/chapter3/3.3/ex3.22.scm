#lang scheme

(define (make-queue)
    (let 
        ((front-ptr null) (rear-ptr null))
        
        (define (set-front-ptr! item)
            (set! front-ptr item)
        )

        (define (set-rear-ptr! item)
            (set! rear-ptr item)
        )

        (define (empty-queue?)
            (null? front-ptr)
        )

        (define (front-queue)
            (if (empty-queue?)
                (error "FRONT called on empty queue" front-queue)
                (mcar front-ptr)
            )
        )

        (define (insert-queue! item)
            (let 
                ((new-pair (mcons item null)))

                (cond
                    ((empty-queue?) 
                        (set-front-ptr! new-pair)
                        (set-rear-ptr! new-pair)
                        front-ptr
                    )
                    (else
                        (set-mcdr! rear-ptr new-pair)
                        (set-rear-ptr! new-pair)
                        front-ptr
                    )
                )
            )
        )

        (define (delete-queue!)
            (cond
                ((empty-queue?) (error "DELETE! called on empty queue"))
                (else
                    (set-front-ptr! (mcdr front-ptr))
                    front-ptr
                )
            )
        )

        (define (print-queue)
            (display front-ptr) (newline)
        )

        (define (dispatch m) 
            (cond
                ((eq? m 'set-front-ptr!) set-front-ptr!)
                ((eq? m 'set-rear-ptr!) set-rear-ptr!)
                ((eq? m 'empty-queue?) empty-queue?)
                ((eq? m 'front-queue) front-queue)
                ((eq? m 'insert-queue!) insert-queue!)
                ((eq? m 'delete-queue!) delete-queue!)
                ((eq? m 'print-queue) print-queue)
                (else
                    (error "Wrong command - QUEUE" m))
            )
        )

        dispatch
    )
)

(define (set-front-ptr! queue item)
    ((queue 'set-front-ptr!) item)
)

(define (set-rear-ptr! queue item)
    ((queue 'set-rear-ptr!) item)
)

(define (empty-queue? queue)
    ((queue 'empty-queue?))
)

(define (front-queue queue)
    ((queue 'front-queue))
)

(define (insert-queue! queue item)
    ((queue 'insert-queue!) item)
)


(define (delete-queue! queue)
    ((queue 'delete-queue!))
)

(define (print-queue queue)
    ((queue 'print-queue))
)


(define q1 (make-queue))
(print-queue q1)
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(delete-queue! q1)
(delete-queue! q1)
