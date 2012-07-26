#lang scheme


(define (linked-node value prev next)
    (define (set-value! new-value)
        (set! value new-value)
    )

    (define (set-prev! new-prev)
        (set! prev new-prev)
    )

    (define (set-next! new-next)
        (set! next new-next)
    )

    (define (dispatch command)
        (cond
            ((eq? command 'set-value!) set-value!)
            ((eq? command 'set-prev!) set-prev!)
            ((eq? command 'set-next!) set-next!)
            ((eq? command 'value) value)
            ((eq? command 'prev) prev)
            ((eq? command 'next) next)
            (else
                (error "Unknown command -- LINKED-NODE" command))
        )
    )

    dispatch
)


(define (list-from-linked linked)
    (if (null? (linked 'next))
        (list (linked 'value))
        (cons (linked 'value) (list-from-linked (linked 'next)))
    )
)


(define front-ptr null) 
(define rear-ptr null)

(define (empty?)
    (and (null? front-ptr) (null? rear-ptr))
)

(define (add-to-empty new-node)
    (set! front-ptr new-node)
    (set! rear-ptr new-node)
    (void)
)

(define (make-empty)
    (set! front-ptr null)
    (set! rear-ptr null)
    (void)    
)

(define (rear-insert! value)
    (let 
        ((new-rear (linked-node value rear-ptr null)))

        (cond
            ((empty?) 
                (add-to-empty new-rear)
            )
            (else
                ((rear-ptr 'set-next!) new-rear)
                (set! rear-ptr new-rear)
                (void)
            )
        )
    )
)

(rear-insert! 1)
(list-from-linked front-ptr)

(rear-insert! 2)
(list-from-linked front-ptr)

(define (front-insert! value)
    (let 
        ((new-front (linked-node value null front-ptr)))

        (cond
            ((empty?) 
                (add-to-empty new-front)
            )
            (else
                ((front-ptr 'set-prev!) new-front)
                (set! front-ptr new-front)
                (void)
            )
        )
    )
)

(front-insert! 0)
(list-from-linked front-ptr)


(define (front-delete!)
    (cond
        ((empty?) 
            (error "Cannot delete front on empty deque"))
        ((null? (front-ptr 'next)) 
            (make-empty))
        (else
            (set! front-ptr (front-ptr 'next)))
    )
)

(front-delete!)
(list-from-linked front-ptr)


(define (rear-delete!)
    (cond
        ((empty?) 
            (error "Cannot delete front on empty deque"))
        ((null? (rear-ptr 'prev)) 
            (make-empty))
        (else
            (set! rear-ptr (rear-ptr 'prev))
            ((rear-ptr 'set-next!) null))
    )
)

(rear-delete!)
(list-from-linked front-ptr)

(define (make-deque)
     
    (define front-ptr null) 
    (define rear-ptr null)
    

    (define (dispatch m) 
        (cond
            ((eq? m 'empty-deque?) empty-deque?)
            ((eq? m 'front-deque) front-deque)
            ((eq? m 'rear-deque) rear-deque)
            ((eq? m 'front-insert-deque!) front-insert-deque!)
            ((eq? m 'rear-insert-deque!) rear-insert-deque!)
            ((eq? m 'front-delete-deque!) front-delete-deque!)
            ((eq? m 'rear-delete-deque!) rear-delete-deque!)
            ((eq? m 'print-deque) print-deque)
            (else
                (error "Wrong command - QUEUE" m))
        )
    )

    dispatch
    
)


(define (set-front-ptr! deque item)
    ((deque 'set-front-ptr!) item)
)

(define (set-rear-ptr! deque item)
    ((deque 'set-rear-ptr!) item)
)

(define (empty-deque? deque)
    ((deque 'empty-deque?))
)

(define (front-deque deque)
    ((deque 'front-deque))
)

(define (rear-deque deque)
    ((deque 'rear-deque))
)

(define (front-insert-deque! deque item)
    ((deque 'front-insert-deque!) item)
)

(define (rear-insert-deque! deque item)
    ((deque 'rear-insert-deque!) item)
)

(define (front-delete-deque! deque)
    ((deque 'front-delete-deque!))
)

; TODO: not done
(define (rear-delete-deque! deque)
    ((deque 'rear-delete-deque!))
)

(define (print-deque queue)
    ((queue 'print-deque))
)


; (define d1 (make-deque))

; (front-insert-deque! d1 'b)
; (rear-insert-deque! d1 'c)
; (rear-insert-deque! d1 'a)

; (eq? (front-deque d1) 'b)

; (eq? (rear-deque d1) 'a)

; (front-delete-deque! d1)
; (rear-delete-deque! d1)
