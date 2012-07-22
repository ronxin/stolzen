#lang scheme 

(require rackunit)


(define balance 100)

(define (withdraw amount)
    (if (>= balance amount)
        (begin 
            (set! balance (- balance amount))
            balance)
        "Insufficient funds"
    )
)

(check-equal? 
    (withdraw 10)
    90)

(check-equal? 
    (withdraw 95)
    "Insufficient funds")

(define new-withdraw
    (let 
        ((balance 100))
        (lambda (amount)
            (if (>= balance amount)
                (begin 
                    (set! balance (- balance amount))
                    balance)
                "Insufficient funds")
        )
    )
)

(define (make-withdraw balance)
    (lambda (amount)
        (if (>= balance amount)
            (begin 
                (set! balance (- balance amount))
                balance)
            "Insufficient funds"
        )
    )
)

(check-equal? 
    ((make-withdraw 100) 95)
    5)

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

(check-equal? 
    (W1 50)
    50)

(check-equal? 
    (W2 70)
    30)

(check-equal? 
    (W2 40)
    "Insufficient funds")

(check-equal? 
    (W1 40)
    10)


(define (make-account balance)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
            balance)
            "Insufficient funds"
        )
    )

    (define (deposit amount)
        (set! balance (+ balance amount))
        balance
    )

    (define (dispatch m)
        (cond 
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request -- MAKE-ACCOUNT" m))
        )
    )

    dispatch
)

(define acc (make-account 100))

(check-equal? 
    ((acc 'withdraw) 50)
    50)
(check-equal? 
    ((acc 'withdraw) 60)
    "Insufficient funds")
(check-equal? 
    ((acc 'deposit) 40)
    90)
(check-equal? 
    ((acc 'withdraw) 60)
    30)


(define acc2 (make-account 100))

(check-equal? 
    ((acc2 'withdraw) 10) 
    90)