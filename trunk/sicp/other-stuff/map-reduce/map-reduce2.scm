#lang scheme

(require racket/trace)
(require rackunit)

(define (map-reducer mapper reducer comparator)
    ; map
    ; assume mapper returns only one value, not a sequence of them
    ; otherwise, should use flatmap instead of map
    (define (*map seq)
        (map mapper seq)
    )

    ; map-reduce
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

    ; reduce
    (define (for-reducer pair)
        (reducer (get-key pair) (get-values pair))
    )

    (define (*reduce input)
        (map for-reducer input)
    )

    ; export
    (define (map-reducer-proc input)
        (*reduce (map-reduce (*map input) comparator))
    )

    map-reducer-proc
)


(define (mapper word)
    (cons word 1)
)

(define (reducer key values)
    (cons key (foldr + 0 values))
)

(define (comparator-string s1 s2)
    (cond
        ((string<? s1 s2) -1)
        ((string>? s1 s2) 1)
        (else 0)
    )
)

(define word-counter (map-reducer mapper reducer comparator-string))

(define lorem-ipsum (list "lorem" "ipsum" "dolor" "sit" "amet" "sed"
    "dolor" "elit" "lorem" "ipsum" "lorem" "sed" "vel"
    "ipsum" "sit" "amet" "dolor" "sed" "purus"))


(check-equal?
    (word-counter lorem-ipsum)
    '(("amet" . 2) ("dolor" . 3) ("elit" . 1) ("ipsum" . 3) 
      ("lorem" . 3) ("purus" . 1) ("sed" . 3) ("sit" . 2) ("vel" . 1))
)