#lang scheme

(require racket/trace)

; for testing 

(define (container)
    (define hash (make-hash))

    (define (put op type item)
        (hash-set! hash (cons op type) item)
    )

    (define (get op type)
        (hash-ref hash (cons op type))
    )

    (lambda (name) 
        (cond
            ((eq? name 'get) get)
            ((eq? name 'put) put)
        )
    )
)

(define storage (container))

(define put (storage 'put))
(define get (storage 'get))

;
; Usual numbers
;

(define (install-number-package)
    (put 'add '(number number) +)
    (put 'sub '(number number) -)
    (put 'mul '(number number) *)
    (put 'div '(number number) /)
    (put 'make 'number (lambda (x) x))
    'done
)

(define (make-number n)
    ((get 'make 'number) n)
)

;
; Generic
;

(define (attach-tag type-tag contents)
    (cons type-tag contents)
)

(define (type-tag datum)
    (cond
        ((number? datum) 'number)
        ((pair? datum) (car datum))
        (else
            (error "Bad tagged datum -- type-tag" datum))
    )
)

(define (contents datum)
    (cond
        ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else
            (error "Bad tagged datum -- contents" datum))
    )
)

(define (apply-generic op . args)
    (let 
        ((type-tags (map type-tag args)))

        (let 
            ((proc (get op type-tags)))

            (if proc 
                (apply proc (map contents args))
                (error "No method for this type -- apply-generic" (list op type-tags))
            )
        )
    )
)


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

;
; test
;

(install-number-package)

(add 3 1)
(sub 3 2)
(mul 3 2)
(div 3 2)


