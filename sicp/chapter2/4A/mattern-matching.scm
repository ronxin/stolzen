#lang scheme

(require racket/trace)


(define ns (make-base-namespace)) ; для eval
(define FAILED 'failed)

(define (atom? x)
    (and (not (pair? x))
         (not (null? x)))
)

;
; ==== DICTIONARIES ====
;


(define empty-dict null)


(define (variable-name pattern) 
    (cadr pattern)
)

(define (extend-dict pat dat dict)
    (let
        ((name (variable-name pat)))
        (let
            ((v (assq name dict)))
            (cond
                ((not v) (cons (list name dat) dict))
                ((eq? (cadr v) dat) dict)
                (else
                    FAILED)
            )
        )
    )
)

; извлекаем имя переменной, 
; пытаемся найти ее в списке
;   если переменняа не найдена, добавляем ее в словарь
;   если переменная найдена, проверяется ее значение в списке
;       если оно не совпадает - ошибка
;       иначе возвращается нетронутый словарь


(equal? (extend-dict '(? x) '10 empty-dict)     '((x 10)))
(equal? (extend-dict '(? x) '10 '((x 10)))      '((x 10)))
(equal? (extend-dict '(? x) '10 '((x 11)))      FAILED)

(define (lookup var dict)
    (let 
        ((v (assq var dict)))
        (if (not v)
            var
            (cadr v)
        )
    )
)


(equal? (assq 'x '((x 10)))         '(x 10))
(equal? (assq 'x '((y 10) (x 10)))  '(x 10))
(equal? (assq 'z '((y 10) (x 10)))   false)


(equal? (lookup 'x '((x 10)))       10)
(equal? (lookup 'y '((x 10)))       'y)

;
; ==== MATCH ====
;

(define (arbitrary-constant? pat)
    (and (pair? pat) (eq? (car pat) '?c))
)

(define (constant? pat)
    (number? pat)
)

(define (arbitrary-variable? pat)
    (and (pair? pat) (eq? (car pat) '?v))
)

(define (variable? pat)
    (symbol? pat)
)

(define (arbitrary-expression? pat)
    (and (pair? pat) (eq? (car pat) '?))
)

(define (match pat exp dict)
    (cond
        ((and (null? pat) (null? exp)) dict)
        ((eq? dict FAILED) FAILED)
        ((atom? pat)
            (if (atom? exp)
                (if (eq? pat exp)
                    dict
                    FAILED
                )
                FAILED
            )
        )
        ((arbitrary-constant? pat) 
            (if (constant? exp)
                (extend-dict pat exp dict)
                FAILED
            )
        )
        ((arbitrary-variable? pat)
            (if (variable? exp)
                (extend-dict pat exp dict)      
                FAILED
            )
        )
        ((arbitrary-expression? pat)
            (extend-dict pat exp dict)
        )
        ((atom? exp) FAILED)
        (else
            (match
                (cdr pat) (cdr exp)
                (match (car pat) (car exp) dict)
            )
        )
    )
)


(equal? (match null null empty-dict)                         empty-dict)
(equal? (match null null '((x 10)))                         '((x 10)))
(equal? (match '* '* empty-dict)                             empty-dict)

(equal? (match '(? x) '10 empty-dict)                       '((x 10)))
(equal? (match '(?c x) '10 empty-dict)                      '((x 10)))
(equal? (match '(?v x) '10 empty-dict)                       FAILED)
(equal? (match '(?v x) 'a empty-dict)                       '((x a)))

(equal? (match '((? x) (? x)) '(10 10) empty-dict)          '((x 10)))
(equal? (match '((? x) (? x)) '(10 11) empty-dict)           FAILED)
(equal? (match '((? x) (? y)) '(10 11) empty-dict)          '((y 11) (x 10)))



(define mult-deriv-pattern '(+ (* (? x) (? y)) (? y)))

(equal? (match mult-deriv-pattern '(+ (* 3 x) x) null)      '((y x) (x 3)))
(equal? (match mult-deriv-pattern '(+ (* 3 x) 4) null)       FAILED)

;
; ==== SKELETON ====
;

(define (skeleton-evaluation? exp)
    (and (pair? exp) (eq? (car exp) ':))
)

(define (eval-exp skeleton)
    (cadr skeleton)
)


; магия
(define (evaluate form dictionary)
    (if (atom? form)
        (lookup form dictionary)

        (apply 
            (eval (lookup (car form) dictionary) ns)
            (map (lambda (v) (lookup v dictionary))
                 (cdr form))
        )
    )
)

(equal? (evaluate '(+ x x) '((x 3))) 6)


(define (instantiate skeleton dict)
    (define (loop s)
        (cond
            ((null? s) null)
            ((atom? s) s)
            ((skeleton-evaluation? s)
                (evaluate (eval-exp s) dict)
            )
            (else
                (cons (loop (car s)) (loop (cdr s))))
        )
    )
    (loop skeleton)
)


(equal? (instantiate 'foo empty-dict)           'foo)
(equal? (instantiate '(f a b) empty-dict)       '(f a b))

; ((y x) (x 3))
(define dict1 (match mult-deriv-pattern '(+ (* 3 x) x) null))

(equal? (instantiate '(: y) dict1)              'x)
(equal? (instantiate '(: x) dict1)              '3)
(equal? (instantiate '(+ (: x) (: y)) dict1)    '(+ 3 x))
(equal? (instantiate '(: (+ x x)) dict1)        6)


;
; ==== SIMPLIFIER ====
;

(define (pattern  rule) 
    (car rule)
)

(define (skeleton rule) 
    (cadr rule)
)

(define (simplifier the-rules)
    (define (try-rules exp)
        (define (scan rules)
            (if (null? rules)
                exp
                (let 
                    ((dictionary 
                        (match (pattern (car rules))
                               exp
                               empty-dict)))
                    (if (eq? dictionary FAILED)
                        (scan (cdr rules))
                        (simplify-exp 
                            (instantiate (skeleton (car rules)) dictionary))
                    )
                )
            )
        )
        (scan the-rules)
    )

    (define (simplify-exp exp)
        (try-rules 
            (if (pair? exp)
                (map simplify-exp exp)
                exp
            )
        )
    )

    simplify-exp
)


(define deriv-rules '(
    ((dd (?c c) (? v))             0)
    ((dd (?v v) (? v))             1)
    ((dd (?v u) (? v))             0)
    ((dd (+ (? x1) (? x2)) (? v))  (+ (dd (: x1) (: v))
                                      (dd (: x2) (: v))))
    ((dd (* (? x1) (? x2)) (? v))  (+ (* (: x1) (dd (: x2) (: v)))
                                      (* (dd (: x1) (: v)) (: x2))))
    ((dd (** (? x) (?c n)) (? v))  (* (* (: n) (+ (: x) (: (- n 1))))
                                   (dd (: x) (: v))))
))


(define algebra-rules '(
    (((? op) (?c c1) (?c c2))                (: (op c1 c2)))
    (((? op) (?  e ) (?c c))                 ((: op) (: c) (: e)))
    ((+ 0 (? e))                             (: e))
    ((* 1 (? e))                             (: e))
    ((* 0 (? e))                             0)
    ((* (?c c1) (* (?c c2) (? e )))          (* (: (* c1 c2)) (: e)))
    ((* (?  e1) (* (?c c ) (? e2)))          (* (: c ) (* (: e1) (: e2))))
    ((* (* (? e1) (? e2)) (? e3))            (* (: e1) (* (: e2) (: e3))))
    ((+ (?c c1) (+ (?c c2) (? e )))          (+ (: (+ c1 c2)) (: e)))
    ((+ (?  e1) (+ (?c c ) (? e2)))          (+ (: c ) (+ (: e1) (: e2))))
    ((+ (+ (? e1) (? e2)) (? e3))            (+ (: e1) (+ (: e2) (: e3))))
    ((+ (* (?c c1) (? e)) (* (?c c2) (? e))) (* (: (+ c1 c2)) (: e)))
    ((* (? e1) (+ (? e2) (? e3)))            (+ (* (: e1) (: e2))))
))

(define dsimp (simplifier deriv-rules))
(define alsimp (simplifier algebra-rules))

(define (deriv exp)
    (alsimp (dsimp exp))
)


(equal? (deriv '(dd (+ x y) x))          1)
(equal? (deriv '(dd (* x y) x))         'y)
(equal? (deriv '(dd (+ (* x y) x) x))   '(+ 1 y))


