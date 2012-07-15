#lang scheme


(define (container)
    (define hash (make-hash))

    (define (put op type item)
        (hash-set! hash (cons op type) item)
    )

    (define (get op type)
        (let
            ((key (cons op type)))
            (if (hash-has-key? hash key)
                (hash-ref hash key)
                false
            )
        )
    )

    (lambda (name) 
        (cond
            ((eq? name 'get) get)
            ((eq? name 'put) put)
            ((eq? name 'hash) hash)
        )
    )
)

(define storage (container))

(define put (storage 'put))
(define get (storage 'get))


(define (attach-tag type-tag contents)
    (cons type-tag contents)
)

(define (type-tag datum)
    (if (pair? datum)
        (car datum)
        (error "Bad tagged datum -- type-tag" datum)
    )
)

(define (contents datum)
    (if (pair? datum)
        (cdr datum)
        (error "Bad tagged datum -- contents" datum)
    )
)

(define (make-number x)
    (attach-tag 'number x)
)

(define (make-rational x)
    (attach-tag 'rational x)
)

(define (make-complex x)
    (attach-tag 'complex x)
)

(define (raise-number x)
    (attach-tag 'rational (contents x))
)

(define (raise-rational x)
    (attach-tag 'complex (contents x))
)

(put 'raise 'number raise-number)
(put 'raise 'rational raise-rational)

(define (raise x)
    ((get 'raise (type-tag x)) x)
)

; 2.84

; representing inheritance tower as list
; the superer type - the nearer it to the head

(define inheritance-tower '(complex rational number))

(define (index-tower tower)
    (define (index-inner seq current-index)
        (if (null? seq)
            null
            (cons (cons (car seq) current-index)
                  (index-inner (cdr seq) (+ 1 current-index) ))
        )
    )
    (index-inner tower 0)
)

; making pairs of {type: ordinal number in inheritance list}
; i.e {complex: 0, rational: 1, etc}
(define inheritance-indexes (index-tower inheritance-tower))

(define (get-index type indexes)
    (cond
        ((null? indexes) -1)
        ((eq? type (car (car indexes))) (cdr (car indexes)))
        (else
            (get-index type (cdr indexes)))
    )
)

(equal? 0 (get-index 'complex inheritance-indexes))
(equal? 1 (get-index 'rational inheritance-indexes))
(equal? 2 (get-index 'number inheritance-indexes))



(define (get-type ord indexes)
    (cond
        ((null? indexes) false)
        ((= ord (cdr (car indexes))) (car (car indexes)))
        (else
            (get-type ord (cdr indexes)))
    )    
)

(equal? 'complex (get-type 0 inheritance-indexes))
(equal? 'rational (get-type 1 inheritance-indexes))
(equal? 'number (get-type 2 inheritance-indexes))



(define (find-superest-parent seq inheritance-indexes)
    (define (index type)
        (get-index type inheritance-indexes)
    )
    (get-type 
        (apply min (map index seq))
        inheritance-indexes
    )
)

(equal? 'rational (find-superest-parent '(number number rational) inheritance-indexes))
(equal? 'number (find-superest-parent '(number number number) inheritance-indexes))
(equal? 'complex (find-superest-parent '(number complex rational) inheritance-indexes))



(define (calc-distance type1 type2 inheritance-indexes)
    (define (index type)
        (get-index type inheritance-indexes)
    )

    (abs (- (index type1) (index type2)))
)

(equal? 0 (calc-distance 'complex 'complex inheritance-indexes))
(equal? 1 (calc-distance 'complex 'rational inheritance-indexes))
(equal? 2 (calc-distance 'complex 'number inheritance-indexes))



(define (distances type-tags inheritance-indexes)
    (define (distance supertype)
        (lambda (type) (calc-distance supertype type inheritance-indexes))
    )

    (let
        ((super-parent (find-superest-parent type-tags inheritance-indexes)))
        
        (map (distance super-parent) type-tags)
    )
)

(equal? '(0 1 2) (distances '(complex rational number) inheritance-indexes))
(equal? '(0 0 0) (distances '(complex complex complex) inheritance-indexes))
(equal? '(0 0 0) (distances '(number number number) inheritance-indexes))




(define (raise-times arg times)
    (if (= times 0)
        arg
        (raise-times (raise arg) (- times 1))
    )
)

(equal? (make-number 1)   (raise-times (make-number 1) 0))
(equal? (make-rational 1) (raise-times (make-number 1) 1))
(equal? (make-complex 1)  (raise-times (make-number 1) 2))



(define (raise-seq args distances)
    (if (null? args)
        null
        (cons (raise-times (car args) (car distances)) 
              (raise-seq (cdr args) (cdr distances)))
    )
) 

(equal? (list (make-complex 1) (make-complex 2))
        (raise-seq (list (make-number 1) (make-complex 2)) '(2 0)))



(define (coerce args inheritance-indexes)
    (let
        ((dis (distances (map type-tag args) inheritance-indexes)))
        (raise-seq args dis)
    )
)

(equal? (list (make-complex 1) (make-complex 2))
        (coerce (list (make-complex 1) (make-complex 2)) inheritance-indexes))



(define (apply-generic op . args)
    (apply-generic2 op args)
)

(define (apply-generic2 op args)
    (let 
        ((type-tags (map type-tag args)))
        (let 
            ((proc (get op type-tags)))
            
            (if proc
                (apply proc (map contents args))
                (apply-generic2 op (coerce args inheritance-indexes))
            )
        )
    )
)


; 2.85


(define (project-complex x)
    (attach-tag 'rational (contents x))
)

(define (project-rational x)
    (attach-tag 'number (contents x))
)

(put 'project 'complex project-complex)
(put 'project 'rational project-rational)

(define (project x)
    (let 
        ((proc (get 'project (type-tag x))))
        (if proc
            (proc x)
            false
        )
    )
)

; let equal? be the generic equality predicate
(define (can-drop x)
    (let
        ((projection (project x)))
        (if projection
            (equal? x (raise projection))
            false
        )
    )
)

(define (drop x)
    (if (can-drop x)
        (drop (project x))
        x
    )
)


(equal? (make-number 1) (drop (make-complex 1)))
(equal? (make-number 1) (drop (make-rational 1)))
(equal? (make-number 1) (drop (make-number 1)))


(define (apply-generic-improved op . args)
    (drop (apply-generic2 op args))
)


; 2.86

; for this change, it's needed to replace * and + in the complex package onto
; generic add and mul. Also generic sin and cos need to be added. And in the
; apply-generic procedure it's not needed anymore to cut the tag, let packeges
; decide on themselves whether to cut or not
