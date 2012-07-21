#lang scheme 

; (require racket/trace)
(require "trace.scm")

(require "container.scm")
(require "tags.scm")
(require "generic-operations.scm")

(define (all? predicate seq)
    (foldr (lambda (curr res) (and (predicate curr) res)) true seq)
)

(define (install-polynomial-package)

    (define (make-poly variable term-list)
        (cons variable term-list)
    )

    (define (variable p) (car p))
    (define (term-list p) (cdr p))

    (define (variable? e)
        (symbol? e)
    )

    (define (same-variable? v1 v2)
        (and (variable? v1) (variable? v2) (eq? v1 v2))
    )

    (define (adjoin-term term term-list)
        (if (=zero? (coeff term))
            term-list
            (cons term term-list)
        )
    )

    (define (the-empty-termlist) '())
    (define (first-term term-list) (car term-list))
    (define (rest-terms term-list) (cdr term-list))

    (define (empty-termlist? term-list) (null? term-list))

    (define (make-term order coeff) (list order coeff))

    (define (order term) (car term))
    (define (coeff term) (cadr term))

    (trace make-poly)
    (trace variable)
    (trace term-list)
    (trace variable?)
    (trace same-variable?)
    (trace adjoin-term)
    (trace first-term)
    (trace rest-terms)
    (trace make-term)    
    (trace order)
    (trace coeff)

    (define (add-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                (add-terms (term-list p1) (term-list p2)))
            (error "Polys not in same var -- ADD-POLY" (list p1 p2))
        )
    )

    (trace add-poly)


    (define (add-terms L1 L2)
        (cond 
            ((empty-termlist? L1) L2)
            ((empty-termlist? L2) L1)
            (else
                (let 
                    ((t1 (first-term L1)) 
                     (t2 (first-term L2)))
                    
                    (cond 
                        ((> (order t1) (order t2))
                            (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                        ((< (order t1) (order t2))
                            (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                        (else
                            (adjoin-term
                                (make-term (order t1)
                                    (add (coeff t1) (coeff t2)))
                                    (add-terms (rest-terms L1) (rest-terms L2)))
                        )
                    )
                )
            )
        )
    )

    (trace add-terms)

    (define (mul-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                (mul-terms (term-list p1) (term-list p2)))
            (error "Polys not in same var -- MUL-POLY" (list p1 p2)))
    )

    (define (mul-terms L1 L2)
        (if (empty-termlist? L1)
            (the-empty-termlist)
            (add-terms (mul-term-by-all-terms (first-term L1) L2)
                       (mul-terms (rest-terms L1) L2))
        )
    )
    
    (define (mul-term-by-all-terms t1 L)
        (if (empty-termlist? L)
            (the-empty-termlist)
            (let 
                ((t2 (first-term L)))
                
                (adjoin-term
                    (make-term (+ (order t1) (order t2))
                               (mul (coeff t1) (coeff t2)))
                    (mul-term-by-all-terms t1 (rest-terms L)))
            )
        )
    )

    (trace mul-poly)
    (trace mul-terms)
    (trace mul-term-by-all-terms)


    ; 2.85

    (define (negate-term term)
        (make-term (order term) (negate (coeff term)))
    )

    (define (neg-poly p)
        (make-poly (variable p) (map negate-term (term-list p)))
    )

    (define (sub-poly p1 p2)
        (add-poly p1 (neg-poly p2))
    )

    ; 2.91

    (define (div-terms L1 L2) 
        (if (empty-termlist? L1) 
            (list (the-empty-termlist) (the-empty-termlist)) 
         
            (let 
                ((t1 (first-term L1)) 
                 (t2 (first-term L2)))

                (if (> (order t2) (order t1)) 
                    (list (the-empty-termlist) L1) 
                    
                    (let 
                        ((new-c (div (coeff t1) (coeff t2))) 
                         (new-o (- (order t1) (order t2))) 
                         (new-t (make-term new-o new-c))) 
                    
                        (let 
                            ((rest-of-result 
                                (div-terms (add-poly L1 (negate (mul-poly (list new-t) L2))) L2)))
                            (list (adjoin-term new-t (car rest-of-result)) (cadr rest-of-result))
                        )
                    )
                )
            )
        )
    )

    (define (div-poly p1 p2) 
        (if (same-variable? (variable p1) (variable p2)) 
            (make-poly (variable p1) 
                (div-terms (term-list p1) 
                (term-list p2))) 
            (error "not the same variable -- div-poly" (list p1 p2)))
    )

    ; 2.84

    (define (zero? p)
        (all? =zero? (map coeff (term-list p)))
    )

    ;; interface to rest of the system
    
    (define (tag p) (attach-tag 'polynomial p))
    
    (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly (contents p1) (contents p2)))))
    
    (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly (contents p1) (contents p2)))))

    (put 'div '(polynomial polynomial) 
       (lambda (p1 p2) (tag (div-poly (contents p1) (contents p2)))))

    (put 'sub '(polynomial polynomial) 
       (lambda (p1 p2) (tag (sub-poly (contents p1) (contents p2)))))

    (put 'negate '(polynomial) 
       (lambda (p) (tag (neg-poly (contents p)))))

    (put '=zero? '(polynomial) 
       (lambda (p) (zero? (contents p))))

    (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))

    (void)
)


(define (make-polynomial var terms)
    ((get 'make 'polynomial) var terms)
)


(provide (all-defined-out))

; 2.89 2.90 skipped