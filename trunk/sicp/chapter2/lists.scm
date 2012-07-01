#lang scheme 

(define (println x)
    (display x)
    (newline)
)



(define one-four (list 1 2 3 4))
one-four


(println "car and cdr on lists:")

(car one-four)
(cdr one-four)
(car (cdr one-four))

(cons 10 one-four)

(println "list-ref")

; available as primitive
(define (list-ref items i) 
    (if (= i 0)
        (car items)
        (list-ref (cdr items) (- i 1))
    )
)


(define squares (list 0 1 4 9 25 36))
(list-ref squares 3)

; avalilable as primitive
(define (length items)
    (if (null? items)
        0
        (+ 1 (length (cdr items)))
    )
)

(println "Length of squares")
(length squares)

(define (length-iter items)
    (define (length-inner l count)
        (if (null? l)
            count
            (length-inner (cdr l) (+ 1 count))
        )
    )
    (length-inner items 0)
)

(println "Length of squares")
(length-iter squares)

(define (append list1 list2)
    (if (null? list1)
        list2
        (let 
            ((head (car list1))
             (tail (cdr list1)))
            (append tail (cons head list2))     
        )
    )
)

(define (append2 list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append2 (cdr list1) list2))
    )
)

(define (append3 list1 list2)
    (if (null? list1)
        list2
        (let 
            ((head (car list1))
             (tail (cdr list1)))
            (cons head (append3 tail list2))     
        )
    )
)

(define odds (list 1 3 5 7 9))
(define evens (list 2 4 6 8 10))

(append odds evens)
(append2 odds evens)
(append3 odds evens)

; 2.17

(define (last items)
    (if (null? (cdr items))
        (car items)
        (last (cdr items))
    )        
)

(println "last")
(last (list 1 2 3))
(last (list 9))

; 2.18

(define (reverse items)
    (define (move list1 list2)
        (if (null? list1)
            list2
            
            (let 
                ((head (car list1))
                 (tail (cdr list1)))
                (move tail (cons head list2))
            )     
        )
    )
    (move items null)
)

(println "reverse")
(reverse (list 1 4 9 16 25))