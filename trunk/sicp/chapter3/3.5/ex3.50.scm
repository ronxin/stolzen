#lang scheme


(define stream-null? null?)
(define the-empty-stream null)
(define stream-car car)
(define stream-cdr cdr)
(define stream-cons cons)

(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
        the-empty-stream
        (stream-cons
            (apply proc (map stream-car argstreams))
            (apply stream-map
                (cons proc (map stream-cdr argstreams)))
        )
    )
)


(stream-map list '(1 2 3) '(4 5 6) '(7 8 9))