#lang scheme 

(require rackunit)

(define (make-account balance password)

    (define passwords (list password))

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

    (define (dispatch pass m)
        (if (correct-password? pass)
            (begin 
                (cond 
                    ((eq? m 'withdraw) withdraw)
                    ((eq? m 'deposit) deposit)
                    ((eq? m 'check-password) true)
                    (else (error "Unknown request -- MAKE-ACCOUNT" m))
                )
            )
            "Invalid password"
        )
    )

    dispatch
)

; 3.7

(define (make-joint account password joint-password)
    (define (decorator try-pass command)
        (if (eq? try-pass joint-password)
            (account password command)
            "Invalid password"
        )
    )

    (if (equal? (account password 'check-password) true)
        decorator
        "Invalid password"
    )
)


(define peter-acc (make-account 100 'open-sesame))

(check-equal? 
    ((peter-acc 'open-sesame 'withdraw) 50)
    50)

(check-equal? 
    (peter-acc 'password2 'withdraw)
    "Invalid password")

(define paul-acc
    (make-joint peter-acc 'open-sesame 'rosebud))


(check-equal? 
    (paul-acc 'open-sesame 'withdraw)
    "Invalid password")

(check-equal? 
    ((paul-acc 'rosebud 'withdraw) 15)
    35)

(check-equal? 
    ((peter-acc 'open-sesame 'withdraw) 15)
    20)


