; 2.6

; http://en.wikipedia.org/wiki/Church_encoding
; zero - это функция с параметром f, возвращающая identity функцию 

(define zero 
    (lambda (f) 
        (lambda (x) x)
    )
)

; функция синуса начинает вести себя как identity функция
(= ((zero sin) 1) 1)


; f - элемент из множества 0.. функций Черча, означает, сколько раз функция применяется
; в ходе выполнения этой функции значение f увеличивается на 1
; param - какая-либо функция
; x - значение, передаваемое param
(define (add-1 f)
    (lambda (param) 
        (lambda (x) (param ((f param) x)))
    )
)

; разворачиваем (add-1 zero)
; 1.
; (add-1 zero)
; 2. 
; (lambda (f) 
;     (lambda (x) (f ((zero f) x)))
; )
; 3
; (lambda (f) 
;     (lambda (x) (f (identity x)))
; )
; 4. 
; (lambda (f) 
;     (lambda (x) (f x))
; )

; разворачиваем (add-1 one)
; 1
; (add-1 one)
; 2
; (lambda (f) 
;     (lambda (x) (f ((one f) x)))
; )
; 3
; (lambda (f) 
;     (lambda (x) (f (f x)))
; )
; 4
; sin2x = (lambda (x) (sin (sin x)))

(define one-d (add-1 zero))
(= ((one-d sin) 1) (sin 1))

(define two-d (add-1 one-d))
(= ((two-d sin) 1) (sin (sin 1)))


(define (one f)
    (lambda (x) (f x))
)

(define (two f)
    (lambda (x) (f (f x)))
)


(= ((one sin) 1) (sin 1))
(= ((two sin) 1) (sin (sin 1)))