#lang scheme

; (require racket/trace)
(require "trace.scm")

(require "container.scm")
(require "tags.scm")

(require "coerce.scm")
(require "drop.scm")



(define (apply-generic op . args)
    ; (drop (apply-generic2 op args))
    (apply-generic2 op args)
)

(define (apply-generic2 op args)
    (let 
        ((type-tags (map type-tag args)))
        (let 
            ((proc (get op type-tags)))
            
            (if proc
                (apply proc args)
                (apply-generic2 op (coerce args inheritance-indexes))
            )
        )
    )
)

(trace apply-generic)
(trace apply-generic2)


(define (add x y) 
    (drop (apply-generic 'add x y))
)

(define (sub x y) 
    (drop (apply-generic 'sub x y))
)

(define (negate x) 
    (apply-generic 'negate x)
)

(define (mul x y) 
    (drop (apply-generic 'mul x y))
)

(define (div x y) 
    (drop (apply-generic 'div x y))
)

(define (cosine x) 
    (apply-generic 'cos x)
)

(define (sine x) 
    (apply-generic 'sin x)
)

(define (sqroot x) 
    (apply-generic 'sqrt x)
)

(define (artangent2 y x) 
    (apply-generic 'atan y x)
)

(define (=zero? x) 
    (apply-generic '=zero? x)
)



(provide (all-defined-out))