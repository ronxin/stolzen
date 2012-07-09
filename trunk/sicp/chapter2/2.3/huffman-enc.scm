#lang scheme

(require racket/trace)

(define LEAF 'leaf)
(define TREE 'tree)

;
; ===  leaf  === 
;

(define (make-leaf symbol weight)
    (list LEAF symbol weight)
)

(define (leaf? node)
    (eq? (car node) LEAF)
)

(define (symbol-leaf leaf)
    (cadr leaf)
)

(define (weight-leaf leaf)
    (caddr leaf)
)

;
; ===  tree  === 
;

(define (make-tree left right symbols weight)
    (list TREE left right symbols weight) 
)

(define (tree? node)
    (eq? (car node) TREE)
)

(define (left-branch tree)
    (cadr tree)
)

(define (right-branch tree)
    (caddr tree)
)

(define (symbols-tree tree)
    (cadddr tree)
)

(define (weight-tree tree)
    (car (cddddr tree))
)

;
; ===  leaves and trees generic  === 
;

(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (symbols-tree tree)
    )
)

(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (weight-tree tree)
    )
)

(define (make-code-tree left right)
    (make-tree 
        left 
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))
    )
)

; (trace make-code-tree)

(define c-leaf (make-leaf 'C 2))
(define d-leaf (make-leaf 'D 1))

(leaf? c-leaf)
(leaf? d-leaf)
(equal? '(C) (symbols c-leaf))
(= 2 (weight c-leaf))
(= 1 (weight d-leaf))

(define tree1 (make-code-tree c-leaf d-leaf))

(not (leaf? tree1))
(= 3 (weight tree1))
(equal? c-leaf (left-branch tree1))
(equal? d-leaf (right-branch tree1))


(define (decode bits tree)
    (define (decode-1 bits current-branch)
        (if (null? bits)
            null
            (let
                ((next-branch (choose-branch (car bits) current-branch)))

                (if (leaf? next-branch)
                    (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
                    (decode-1 (cdr bits) next-branch)
                )
            )
        )
    )
    (decode-1 bits tree)
)


(define (choose-branch bit branch)
    (cond
        ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else
            (error ("bad bit -- CHOOSE-BRANCH" bit)))
    )
)


(define (adjoint-set x set)
    (cond
        ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else
            (cons 
                (car set)
                (adjoint-set x (cdr set))
            )
        )
    )
)

(adjoint-set (make-leaf 'G 4) null)


(define (make-leaf-set pairs)
    (if (null? pairs)
        null
        (let 
            ((pair (car pairs)))
            ;                         symbol    frequency
            (adjoint-set (make-leaf (car pair) (cadr pair))
                         (make-leaf-set (cdr pairs)))
        )
    )
)

(make-leaf-set '((A 4) (B 2) (C 1) (D 1)))

; 2.67

(define sample-tree 
    (make-code-tree
        (make-leaf 'A 4)
        (make-code-tree
            (make-leaf 'B 2)
            (make-code-tree 
                (make-leaf 'D 1)
                (make-leaf 'C 1)
            )
        )
    )
)

(define encoded-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(define decoded-message (decode encoded-message sample-tree))

; 2.68

(define (element-of-set? x set)
    (cond
        ((null? set) false)
        ((equal? x (car set)) true)
        (else
            (element-of-set? x (cdr set)))
    )
)

(define (encode-symbol symbol tree)
    (define (encode-symbol-inner tree)
        (if (leaf? tree) 
            null

            (let
                ((left (left-branch tree))
                 (right (right-branch tree)))

                (cond
                    ((element-of-set? symbol (symbols left)) 
                        (cons 0 (encode-symbol-inner left)))
                    ((element-of-set? symbol (symbols right))
                        (cons 1 (encode-symbol-inner right)))
                    (else
                        (error "Element not found -- encode-symbol-inner" symbol tree))
                )
            )
        )
    )

    (if (tree? tree)
        (encode-symbol-inner tree)
        (error "tree expected -- encode-symbol, got" tree)
    )
)


(equal? '(0) (encode-symbol 'C tree1))
(equal? '(1) (encode-symbol 'D tree1))

(equal? '(0)     (encode-symbol 'A sample-tree))
(equal? '(1 0)   (encode-symbol 'B sample-tree))
(equal? '(1 1 1) (encode-symbol 'C sample-tree))
(equal? '(1 1 0) (encode-symbol 'D sample-tree))

(define (encode message tree)
    (if (null? message)
        null
        (append 
            (encode-symbol (car message) tree)
            (encode (cdr message) tree)
        )
    )
)

(equal? encoded-message (encode decoded-message sample-tree))

; 2.69

(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs))
)

(define (successive-merge leaves)
    (cond
        ((null? leaves) null)
        ((null? (cdr leaves)) (car leaves))
        (else
            (let 
                ((new-node (make-code-tree (car leaves) (cadr leaves)))
                 (tail (cddr leaves)))
                (successive-merge (adjoint-set new-node tail))
            )
        )
    )
)

(define tree2 (generate-huffman-tree '((A 1) (B 10) (C 5) (D 1) (E 4) (F 3))))

(define decoded-2 '(A B C D A B C D C A F E B A B E))
(define encoded-2 (encode decoded-2 tree2))
(equal? decoded-2 (decode encoded-2 tree2))

; 2.70

(define lyrix-pairs '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))
(define lyrix-tree (generate-huffman-tree lyrix-pairs))

(define lyrix 
    '(GET A JOB
      SHA NA NA NA NA NA NA NA NA
      GET A JOB
      SHA NA NA NA NA NA NA NA NA
      WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
      SHA BOOM) 
)

(define enxoded-lyrix (encode lyrix lyrix-tree))
(display "bytes required for lyrix: ") (length enxoded-lyrix)

(define (log2 x)
    (/ (log x) (log 2))
)

(define (per-symbol symbols-len)
    (ceiling (log2 symbols-len))
)

(display "bytes required for lyrix if fixed-length: ") 
(* (per-symbol (length lyrix-pairs)) (length lyrix))


; 2.71

; with an alphabet made up with n symbols, 1 bit required for the most frequent
; symbol, (n - 1) for the least frequent

; 2.72

; for most frequent - O(1), for the least - O(n^2). Search is the most expensive
; part
