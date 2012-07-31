#lang scheme

(define the-empty-stream null)

; http://people.cs.aau.dk/~normark/prog3-03/html/notes/eval-order_themes-delay-stream-section.html
(define-syntax cons-stream
    (syntax-rules ()
        ((cons-stream x y)
            (cons x (delay y))
        )
    )
)

(define (stream-car stream) 
    (car stream)
)

(define (stream-cdr stream) 
    (force (cdr stream))
)


(define (stream-ref s n)
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))
    )
)

(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
        the-empty-stream
        (cons-stream
            (apply proc (map stream-car argstreams))
            (apply stream-map
                (cons proc (map stream-cdr argstreams)))
        )
    )
)

(define (stream-for-each proc s)
    (if (stream-null? s)
        (void)
        (begin 
            (proc (stream-car s))
            (stream-for-each proc (stream-cdr s)))
    )
)


(define (display-stream s)
    (display "(") (stream-for-each display-line s) (display ")") (newline)
)

(define (display-line x)
    (display x) (display " ") 
)



(define (stream-enumerate-interval low high)
    (if (> low high)
        the-empty-stream
        (cons-stream
            low
            (stream-enumerate-interval (+ low 1) high))
    )
)

(define stream-null? null?)

(define (stream-filter pred stream)
    (cond 
        ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
            (cons-stream 
                (stream-car stream)
                (stream-filter pred (stream-cdr stream)))
        )
        (else 
            (stream-filter pred (stream-cdr stream)))
    )
)

; force is scheme primitive
; (define (force delayed-object)
;     (delayed-object)
; )

(define (memo-proc proc)
    (let 
        ((already-run? false) (result false))
        (lambda ()
            (if (not already-run?)
                (begin 
                    (set! result (proc))
                    (set! already-run? true)
                    result)
                result
            )
        )
    )
)


; delay - scheme primitive
; (define (delay op)
;     (memo-proc (lambda () op))
; )


(define (stream-first-n stream n)
    (if (= n 0)
        the-empty-stream
        (cons-stream (stream-car stream)
            (stream-first-n (stream-cdr stream) (- n 1)))
    )
)

(define (stream-to-list stream)
    (if (stream-null? stream)
        null
        (cons (stream-car stream) (stream-to-list (stream-cdr stream)))
    )
)

(provide (all-defined-out))
