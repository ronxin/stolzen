#lang scheme

(require racket/trace)

;
; Connector
; 

(define (inform-about-value constraint)
    (constraint 'I-have-a-value)
)

(define (inform-about-no-value constraint)
    (constraint 'I-lost-my-value)
)

(define (for-each-except exception procedure list)
    (define (loop items)
        (cond 
            ((null? items) 
                'done)
            ((eq? (car items) exception) 
                (loop (cdr items)))
            (else 
                (procedure (car items)) (loop (cdr items))))
    )

    (loop list)
)

(define (make-connector)
    (let 
        ((value false) (informant false) (constraints '()))

        (define (set-my-value newval setter)
            (cond 
                ((not (has-value? me-connector))
                    (set! value newval)
                    (set! informant setter)
                    (for-each-except setter inform-about-value constraints)
                )
                ((not (= value newval))
                    (error "Contradiction" (list value newval)))
                (else 
                    'ignored)
            )
        )

        (define (eq?debug a b)
            (eq? a b)
        )

        (trace eq?debug)

        (define (forget-my-value retractor)
            (if (eq?debug retractor informant)
                (begin 
                    (set! informant false)
                    (for-each-except retractor inform-about-no-value constraints))
                'not-same-informant
            )
        )

        (define (connect new-constraint)
            ; memq finds value in given list, returns true, if found, false otherwise
            (if (not (memq new-constraint constraints))
                (set! constraints (cons new-constraint constraints))
                (void)
            )

            (if (has-value? me-connector)
                (inform-about-value new-constraint)
                (void)
            )

            'done
        )

        (define (has-value-inner?)
            (if informant true false)
        )

        (define (get-value-inner)
            (if (has-value-inner?)
                value
                'no-value
            )
        )

        (define (me-connector request)
            (cond 
                ((eq? request 'has-value?) (has-value-inner?))
                ((eq? request 'value) (get-value-inner))
                ((eq? request 'set-value!) set-my-value)
                ((eq? request 'forget) forget-my-value)
                ((eq? request 'connect) connect)
                ((eq? request 'informant) informant)
                (else 
                    (error "Unknown operation -- CONNECTOR" request))
            )
        )

        (trace set-my-value)
        (trace forget-my-value)

        me-connector
    )
)

(define (has-value? connector)
    (connector 'has-value?)
)

(define (get-value connector)
    (connector 'value)
)

(define (set-value! connector new-value informant)
    ((connector 'set-value!) new-value informant)
)

(define (forget-value! connector retractor)
    ((connector 'forget) retractor)
)

(define (connect connector new-constraint)
    ((connector 'connect) new-constraint)
)


(trace forget-value!)
(trace set-value!)
(trace get-value)

;
; constraints
;

(define (adder a1 a2 sum)
    (define (process-new-value)
        (cond 
            ((and (has-value? a1) (has-value? a2))
                (set-value! sum
                    (+ (get-value a1) (get-value a2))
                    me-adder)
            )
            ((and (has-value? a1) (has-value? sum))
                (set-value! a2
                    (- (get-value sum) (get-value a1))
                    me-adder)
            )
            ((and (has-value? a2) (has-value? sum))
                (set-value! a1
                    (- (get-value sum) (get-value a2))
                    me-adder)
            )
        )
    )

    (define (process-forget-value)
        (forget-value! sum me-adder)
        (forget-value! a1 me-adder)
        (forget-value! a2 me-adder)
        (process-new-value)
    )

    (define (me-adder request)
        (cond 
            ((eq? request 'I-have-a-value)  
                (process-new-value))
            ((eq? request 'I-lost-my-value) 
                (process-forget-value))
            (else 
                (error "Unknown request -- ADDER" request))
        )
    )

    (connect a1 me-adder)
    (connect a2 me-adder)
    (connect sum me-adder)

    me-adder
)


(define (multiplier m1 m2 product)
    (define (process-new-value)
        (cond 
            ((or (and (has-value? m1) (= (get-value m1) 0))
                 (and (has-value? m2) (= (get-value m2) 0)))
                (set-value! product 0 me-multiplier)
            )
            ((and (has-value? m1) (has-value? m2))
                (set-value! product
                    (* (get-value m1) (get-value m2))
                    me-multiplier)
            )
            ((and (has-value? product) (has-value? m1))
                (set-value! m2
                    (/ (get-value product) (get-value m1))
                    me-multiplier)
            )
            ((and (has-value? product) (has-value? m2))
                (set-value! m1
                    (/ (get-value product) (get-value m2))
                    me-multiplier)
            )
        )
    )

    (define (process-forget-value)
        (forget-value! product me-multiplier)
        (forget-value! m1 me-multiplier)
        (forget-value! m2 me-multiplier)
        (process-new-value)
    )

    (define (me-multiplier request)
        (cond 
            ((eq? request 'I-have-a-value)
                (process-new-value))
            ((eq? request 'I-lost-my-value)
                (process-forget-value))
            (else
                (error "Unknown request -- MULTIPLIER" request))
        )
    )

    (connect m1 me-multiplier)
    (connect m2 me-multiplier)
    (connect product me-multiplier)

    me-multiplier
)

(define (constant value connector)
    (define (me-constant request)
        (error "Unknown request -- CONSTANT" request)
    )

    (connect connector me-constant)
    (set-value! connector value me-constant)

    me-constant
)

;
; constraint networks
;




(define (probe name connector)
    (define (print-probe value)
        (display "Probe: ")
        (display name)
        (display " = ")
        (display value)
        (newline)
    )

    (define (process-new-value)
        (print-probe (get-value connector))
    )

    (define (process-forget-value)
        (print-probe "?")
    )

    (define (me-probe request)
        (cond 
            ((eq? request 'I-have-a-value)
                (process-new-value))
            ((eq? request 'I-lost-my-value)
                (process-forget-value))
            (else
                (error "Unknown request -- PROBE" request))
        )
    )

    (connect connector me-probe)

    (void)
)


(provide (all-defined-out))

