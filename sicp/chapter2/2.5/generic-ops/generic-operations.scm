#lang scheme

; (require racket/trace)
(require "trace.scm")

(require "container.scm")
(require "tags.scm")

(require "coerce.scm")



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
    (apply-generic 'add x y)
)

(define (sub x y) 
    (apply-generic 'sub x y)
)

(define (mul x y) 
    (apply-generic 'mul x y)
)

(define (div x y) 
    (apply-generic 'div x y)
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




(provide (all-defined-out))