#lang scheme


(define (count-leaves item)
    (cond 
        ((null? item) 0)
        ((not (pair? item)) 1)
        (else
            (+ (count-leaves (car item))
               (count-leaves (cdr item)))

        )
    )
)


(define x (cons (list 1 2) (list 3 4)))

(length x)
(count-leaves x)

(count-leaves (list x x))


; 2.24

(list 1 (list 2 (list 3 4)))


; 2.25

(define list1 (list 1 3 (list 5 7) 9))
list1
(car (cdr (car (cdr (cdr list1)))))

(define list2 (list (list 7)))
list2 
(car (car list2))

(define list3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
list3
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list3))))))))))))

; 2.27

(define x1 (list 1 2 3))
(define y1 (list 4 5 6))

(append x1 y1)
(cons x1 y1)
(list x1 y1)


; 2.28 

(define to-reverse (list (list 1 2) (list 3 4)))

(define (deep-reverse item)
    (define (move list1 list2)
        (if (null? list1)
            list2
            
            (let 
                ((head (car list1))
                 (tail (cdr list1)))
                (move tail (cons (deep-reverse head) list2))
            )     
        )
    )
    (if (pair? item)
       (move item null)
       item
    )
)

(reverse to-reverse)
(deep-reverse to-reverse)


