#lang scheme 

(require rackunit)

(define (make-account balance password)

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

    (define (correct-password? try)
        (eq? password try)
    )

    (define before-call-cops 7)

    (define (reset-cops-counter)
        (set! before-call-cops 7)
    )

    (define (call-cops?)
        (begin
            (set! before-call-cops (- before-call-cops 1))
            (<= before-call-cops 0))
    )

    (define (call-cops)
        "Cops are coming"
    )

    (define (dispatch pass m)
        (if (correct-password? pass)
            (begin 
                (reset-cops-counter)
                (cond 
                    ((eq? m 'withdraw) withdraw)
                    ((eq? m 'deposit) deposit)
                    (else (error "Unknown request -- MAKE-ACCOUNT" m))
                )
            )

            (if (call-cops?) (call-cops) "Invalid password")
        )
    )

    dispatch
)

(define acc (make-account 100 'password))

(check-equal? 
    ((acc 'password 'withdraw) 50)
    50)
(check-equal? 
    ((acc 'password 'withdraw) 60)
    "Insufficient funds")
(check-equal? 
    ((acc 'password 'deposit)  40)
    90)
(check-equal? 
    ((acc 'password 'withdraw) 60)
    30)
(check-equal? 
    (acc 'password2 'withdraw)
    "Invalid password")


(define acc2 (make-account 100 'password2))

(check-equal? 
    ((acc2 'password2 'withdraw) 10) 
    90)


(check-equal? 
    (acc2 'password 'withdraw)
    "Invalid password")
(check-equal? 
    (acc2 'password 'withdraw)
    "Invalid password")
(check-equal? 
    (acc2 'password 'withdraw)
    "Invalid password")
(check-equal? 
    (acc2 'password 'withdraw)
    "Invalid password")
(check-equal? 
    (acc2 'password 'withdraw)
    "Invalid password")
(check-equal? 
    (acc2 'password 'withdraw)
    "Invalid password")

(check-equal? 
    (acc2 'password 'withdraw)
    "Cops are coming")

