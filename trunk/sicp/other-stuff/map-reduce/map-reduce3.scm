#lang scheme

(require racket/trace)
(require rackunit)

(define (add-to-bucket buckets pair)
    (cond
        ((null? buckets) 
            (list (list pair)))
        ((eq? (caaar buckets) (car pair)) 
            (cons (cons pair (car buckets)) (cdr buckets)))
        (else
            (cons (car buckets) (add-to-bucket (cdr buckets) pair)))
    )
)

(check-equal?
    (add-to-bucket null '(a . 1))
    '(((a . 1))))

(check-equal?
    (add-to-bucket (add-to-bucket null '(a . 1)) '(b . 1))
    '(((a . 1)) ((b . 1))))

(check-equal?
    (add-to-bucket (add-to-bucket (add-to-bucket null '(a . 1)) '(b . 1)) '(a . 2))
    '(((a . 2) (a . 1)) ((b . 1))))


(define (sort-into-buckers seq)
    (define (buckets-inner buckets tail)
        (if (null? tail)
            buckets
            (buckets-inner (add-to-bucket buckets (car tail)) (cdr tail))
        )
    )

    (buckets-inner null seq)
)

(check-equal?
    (sort-into-buckers '((a . 1) (a . 2) (b . 1) (b . 2) (a . 3)))
    '(((a . 3) (a . 2) (a . 1)) ((b . 2) (b . 1))))



(define (*map mapper sequence)
    (map (lambda (pair) (mapper (car pair) (cdr pair))) sequence)
)

(define (flat sequence)
    (foldr append null sequence)
)

(define lorem-ipsum '(lorem ipsum dolor sit amet sed
    dolor elit lorem ipsum lorem sed vel
    ipsum sit amet dolor sed purus))

(define lorem-file (cons 'lorem-ipsum lorem-ipsum))

(define (word-count-mapper key value)
    (map (lambda (word) (cons word 1)) value) 
)


(define lorem-pairs (flat (*map word-count-mapper (list lorem-file lorem-file))))

(define (flat-map mapper sequence)
    (flat (*map mapper sequence))
)

(check-equal?
    lorem-pairs
    '((lorem . 1) (ipsum . 1) (dolor . 1) (sit . 1) (amet . 1) (sed . 1) (dolor . 1) 
        (elit . 1) (lorem . 1) (ipsum . 1) (lorem . 1) (sed . 1) (vel . 1) (ipsum . 1) 
        (sit . 1) (amet . 1) (dolor . 1) (sed . 1) (purus . 1) (lorem . 1) (ipsum . 1) 
        (dolor . 1) (sit . 1) (amet . 1) (sed . 1) (dolor . 1) (elit . 1) (lorem . 1) 
        (ipsum . 1) (lorem . 1) (sed . 1) (vel . 1) (ipsum . 1) (sit . 1) (amet . 1) 
        (dolor . 1) (sed . 1) (purus . 1)))



(check-equal?
    (flat-map word-count-mapper (list lorem-file lorem-file))
    lorem-pairs)

; (flat-map mapper (list lorem-file))

(check-equal?
    (sort-into-buckers (flat-map word-count-mapper (list lorem-file)))
    '(((lorem . 1) (lorem . 1) (lorem . 1)) 
        ((ipsum . 1) (ipsum . 1) (ipsum . 1)) 
        ((dolor . 1) (dolor . 1) (dolor . 1)) 
        ((sit . 1) (sit . 1)) 
        ((amet . 1) (amet . 1)) 
        ((sed . 1) (sed . 1) (sed . 1)) 
        ((elit . 1)) ((vel . 1)) 
        ((purus . 1))))

(define (groupreduce reducer start-value buckets)   
    (define (reduce-bucket bucket)
        (cons (caar bucket)
              (foldr reducer start-value (map cdr bucket)))
    )

    (map reduce-bucket buckets)
)


(check-equal?
    (groupreduce + 0 (sort-into-buckers lorem-pairs))
    '((lorem . 6) (ipsum . 6) (dolor . 6) (sit . 4) (amet . 4) (sed . 6) (elit . 2) (vel . 2) (purus . 2)))


(define (mapreduce mapper reducer start-value sequence)
    (groupreduce reducer start-value (sort-into-buckers (flat-map mapper sequence)))
)

(check-equal?
    (mapreduce word-count-mapper + 0 (list lorem-file))
    '((lorem . 3) (ipsum . 3) (dolor . 3) (sit . 2) (amet . 2) 
        (sed . 3) (elit . 1) (vel . 1) (purus . 1)))