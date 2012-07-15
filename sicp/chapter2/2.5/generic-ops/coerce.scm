#lang scheme

; (require racket/trace)
(require "trace.scm")

(require "tags.scm")
(require "raise.scm")

(define inheritance-tower '(complex rational scheme-number))

(define (index-tower tower)
    (define (index-inner seq current-index)
        (if (null? seq)
            null
            (cons (cons (car seq) current-index)
                  (index-inner (cdr seq) (+ 1 current-index) ))
        )
    )
    (index-inner tower 0)
)

; making pairs of {type: ordinal number in inheritance list}
; i.e {complex: 0, rational: 1, etc}
(define inheritance-indexes (index-tower inheritance-tower))

(define (get-index type indexes)
    (cond
        ((null? indexes) -1)
        ((eq? type (car (car indexes))) (cdr (car indexes)))
        (else
            (get-index type (cdr indexes)))
    )
)

(define (get-type ord indexes)
    (cond
        ((null? indexes) false)
        ((= ord (cdr (car indexes))) (car (car indexes)))
        (else
            (get-type ord (cdr indexes)))
    )    
)

(define (find-superest-parent seq inheritance-indexes)
    (define (index type)
        (get-index type inheritance-indexes)
    )
    (get-type 
        (apply min (map index seq))
        inheritance-indexes
    )
)

(define (calc-distance type1 type2 inheritance-indexes)
    (define (index type)
        (get-index type inheritance-indexes)
    )

    (abs (- (index type1) (index type2)))
)

(define (distances type-tags inheritance-indexes)
    (define (distance supertype)
        (lambda (type) (calc-distance supertype type inheritance-indexes))
    )

    (let
        ((super-parent (find-superest-parent type-tags inheritance-indexes)))
        
        (map (distance super-parent) type-tags)
    )
)

(define (raise-times arg times)
    (if (= times 0)
        arg
        (raise-times (raise arg) (- times 1))
    )
)

(define (raise-seq args distances)
    (if (null? args)
        null
        (cons (raise-times (car args) (car distances)) 
              (raise-seq (cdr args) (cdr distances)))
    )
) 

(define (coerce args inheritance-indexes)
    (let
        ((dis (distances (map type-tag args) inheritance-indexes)))
        (raise-seq args dis)
    )
)


(trace coerce)
(trace distances)
(trace calc-distance)
(trace raise-seq)

(provide (all-defined-out))