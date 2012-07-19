#lang scheme

(require racket/trace)
(require rackunit)

(define (*mapper word)
    (cons word 1)
)

(define (*map seq)
    (map *mapper seq)
)


(define (get-key pair)
    (car pair)
)

(define (get-values pair)
    (cdr pair)
)

(define (make-pair key value)
    (list key value)
)

(define (append-value pair value)
    (cons (get-key pair) (cons value (get-values pair)))
)

(check-equal? 
    (make-pair 1 'word)
    '(1 word))

(check-equal? 
    (append-value (make-pair 1 'word) 'another)
    '(1 another word))

(check-equal? 
    (append-value (append-value (make-pair 1 'word) 'another) 'yet)
    '(1 yet another word))


(define (number-comparator n1 n2)
    (- n1 n2)
)

(define (add dict comparator key value)
    (if (null? dict)
        (list (make-pair key value))
        (let 
            ((res (comparator key (get-key (car dict)))))
            (cond
                ((> res 0) 
                    (cons (car dict) (add (cdr dict) comparator key value)))
                ((< res 0) 
                    (cons (make-pair key value) dict))
                (else
                    (cons (append-value (car dict) value) (cdr dict)))
            )
        )
    )
)


(define (map-reduce intermidiate-pairs comparator)
    (define (mr-inner result tail)
        (if (null? tail)
            result
            (let 
                ((key (car (car tail)))
                 (value (cdr (car tail))))
                (mr-inner (add result comparator key value) (cdr tail))
            )
        )
    )

    (mr-inner null intermidiate-pairs)
)

(define (comparator-string s1 s2)
    (cond
        ((string<? s1 s2) -1)
        ((string>? s1 s2) 1)
        (else
            0)
    )
)

(define lorem-ipsum (list "lorem" "ipsum" "dolor" "sit" "amet" "sed"
    "dolor" "elit" "lorem" "ipsum" "lorem" "sed" "vel"
    "ipsum" "sit" "amet" "dolor" "sed" "purus"))

(check-equal?
    (map-reduce (*map lorem-ipsum) comparator-string)
    '(("amet" 1 1) ("dolor" 1 1 1) ("elit" 1) ("ipsum" 1 1 1) ("lorem" 1 1 1) ("purus" 1) 
        ("sed" 1 1 1) ("sit" 1 1) ("vel" 1))
)

(define (reducer entry)
    (cons (get-key entry) (foldr + 0 (get-values entry)))
)

(define (*reduce input)
    (map reducer input)
)

(*reduce (map-reduce (*map lorem-ipsum) comparator-string))