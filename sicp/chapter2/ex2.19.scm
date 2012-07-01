#lang scheme

; from 1.2.2

(define (count-change amount)
    (define (cc amount kinds-of-coins)
        (cond 
            ((= amount 0) 1)
            ((or (< amount 0) (= kinds-of-coins 0)) 0)
            (else 
                (+  (cc amount (- kinds-of-coins 1))
                    (cc (- amount (denomination kinds-of-coins))
                        kinds-of-coins
                    )
                )
            )
        )
    )

    (define (denomination kinds-of-coins)
        (cond 
            ((= kinds-of-coins 1) 1)
            ((= kinds-of-coins 2) 5)
            ((= kinds-of-coins 3) 10)
            ((= kinds-of-coins 4) 25)
            ((= kinds-of-coins 5) 50)
        )
    )
    (cc amount 5)

)

(count-change 100)

; 2.19

(define (count-change-list amount coins)
    (define (cc amount coins)
        (cond 
            ((= amount 0) 1)
            ((or (< amount 0) (null? coins)) 0)
            (else 
                (+ (cc (- amount (car coins)) coins)
                   (cc amount (cdr coins))))
        )
    )
    (cc amount coins)
)

(define us-coins (list 1 5 10 25 50))
(define uk-coins (list 0.5 1 2 5 10 20 50 100))
(count-change-list 100 us-coins)
(count-change-list 100 uk-coins)