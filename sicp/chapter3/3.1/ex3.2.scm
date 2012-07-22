#lang scheme

(require rackunit)


(define (make-monitored func)

    (define mf 0)

    (define (command? param)
        (and (not (null? param)) (symbol? (car param)) (null? (cdr param)))
    )

    (define get-command car)

    (define (dispatch command)
        (cond
            ((eq? command 'how-many-calls?) mf)
            ((eq? command 'reset-count) (set! mf 0) (void))
            (else
                (error "unknown message -- dispatch" command))
        )
    )
    
    (define (inc)
        (set! mf (+ mf 1))
    )

    (define (dispatcher . params)
        (if (command? params)
            (dispatch (get-command params))
            (begin (inc) (apply func params))
        )
    )

    dispatcher
)

(define s (make-monitored sqrt))

(check-equal? 
    (s 100)
    10)

(check-equal? 
    (s 'how-many-calls?)
    1)

(check-equal? 
    (s 100)
    10)

(check-equal? 
    (s 'how-many-calls?)
    2)

(s 'reset-count)

(check-equal? 
    (s 'how-many-calls?)
    0)
