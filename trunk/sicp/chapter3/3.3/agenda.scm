#lang scheme

(require "queue.scm")

(define (make-time-segment time queue)
    (mcons time queue)
)

(define (segment-time s) 
    (mcar s)
)

(define (segment-queue s) 
    (mcdr s)
)

(define (make-agenda) 
    (mcons 0 null)
)

(define (current-time agenda) 
    (mcar agenda)
)

(define (set-current-time! agenda time)
    (set-mcar! agenda time)
)

(define (segments agenda) 
    (mcdr agenda)
)

(define (set-segments! agenda segments)
    (set-mcdr! agenda segments)
)

(define (first-segment agenda) 
    (mcar (segments agenda))
)

(define (rest-segments agenda) 
    (mcdr (segments agenda))
)

(define (empty-agenda? agenda)
    (null? (segments agenda))
)

(define (add-to-agenda! time action agenda)
    (define (belongs-before? segments)
        (or (null? segments)
            (< time (segment-time (mcar segments))))
    )

    (define (make-new-time-segment time action)
        (let 
            ((q (make-queue)))
            
            (insert-queue! q action)
            (make-time-segment time q)
        )
    )

    (define (add-to-segments! segments)
        (if (= (segment-time (mcar segments)) time)
            (insert-queue! (segment-queue (mcar segments)) action)
            
            (let 
                ((rest (mcdr segments)))

                (if (belongs-before? rest)
                    (set-mcdr! segments
                              (mcons (make-new-time-segment time action) (mcdr segments)))
                    (add-to-segments! rest)
                )
            )
        )
    )

    (let 
        ((segments (segments agenda)))
        
        (if (belongs-before? segments)
            (set-segments! agenda
                (mcons (make-new-time-segment time action) segments))
            (add-to-segments! segments)
        )
    )
)

(define (remove-first-agenda-item! agenda)
    (let 
        ((q (segment-queue (first-segment agenda))))

        (delete-queue! q)

        (if (empty-queue? q)
            (set-segments! agenda (rest-segments agenda))
            (void)
        )
    )
)

(define (first-agenda-item agenda)
    (if (empty-agenda? agenda)

        (error "Agenda is empty -- FIRST-AGENDA-ITEM")
        
        (let 
            ((first-seg (first-segment agenda)))

            (set-current-time! agenda (segment-time first-seg))
            (front-queue (segment-queue first-seg))
        )
    )
)


(provide (all-defined-out))

; ex 3.32
; We wouldn't have priority in case of LIFO.