#lang scheme

(require "agenda.scm")

;
; wire
;

(define (call-each procedures)
    (if (null? procedures)
        (void)
        (begin
            ((mcar procedures))
            (call-each (mcdr procedures)))
    )
)

(define (make-wire)
    (let 
        ((signal-value 0) (action-procedures null))
        
        (define (set-my-signal! new-value)
            (if (not (= signal-value new-value))
                (begin 
                    (set! signal-value new-value)
                    (call-each action-procedures))
                (void)
            )
        )
        
        (define (accept-action-procedure! proc)
            (set! action-procedures (mcons proc action-procedures))
            (proc)
        )
        
        (define (dispatch m)
            (cond 
                ((eq? m 'get-signal)  signal-value)
                ((eq? m 'set-signal!) set-my-signal!)
                ((eq? m 'add-action!) accept-action-procedure!)
                (else 
                    (error "Unknown operation -- WIRE" m)))
        )
        
        dispatch
    )
)

(define (get-signal wire)
    (wire 'get-signal)
)

(define (set-signal! wire new-value)
    ((wire 'set-signal!) new-value)
)

(define (add-action! wire proc)
    ((wire 'add-action!) proc)
)

;
; agenda
;


(define the-agenda (make-agenda))


(define (after-delay delay action)
    (add-to-agenda! 
        (+ delay (current-time the-agenda))
        action
        the-agenda)
)


(define (propagate)
    (if (empty-agenda? the-agenda)
        (void)
        (let 
            ((first-item (first-agenda-item the-agenda)))

            (first-item)
            (remove-first-agenda-item! the-agenda)
            (propagate)
        )
    )
)

(define (probe name wire)
    (add-action! wire
        (lambda ()        
            (display name)
            (display " ")
            (display (current-time the-agenda))
            (display "  New-value = ")
            (display (get-signal wire))
            (newline)
        )
    )
)

;
; gates
;

(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (or-gate in1 in2 output)
    (define (or-action-procedure)
        (let 
            ((new-value
                (logical-or (get-signal in1) (get-signal in2))))
        
            (after-delay or-gate-delay
                (lambda () (set-signal! output new-value)))
        )
    )

    (add-action! in1 or-action-procedure)
    (add-action! in2 or-action-procedure)
    (void)
)


(define (logical-or s1 s2)
    (cond
        ((and (= s1 1) (= s2 1)) 1)
        ((and (= s1 0) (= s2 1)) 1)
        ((and (= s1 1) (= s2 0)) 1)
        ((and (= s1 0) (= s2 0)) 0)
        (else
            (error "Invalid signals" (list s1 s2)))
    )
)

(define (and-gate in1 in2 output)
    (define (and-action-procedure)
        (let 
            ((new-value
                (logical-and (get-signal in1) (get-signal in2))))
        
            (after-delay and-gate-delay
                (lambda () (set-signal! output new-value)))
        )
    )

    (add-action! in1 and-action-procedure)
    (add-action! in2 and-action-procedure)
    (void)
)

(define (logical-and s1 s2)
    (cond
        ((and (= s1 1) (= s2 1)) 1)
        ((and (= s1 0) (= s2 1)) 0)
        ((and (= s1 1) (= s2 0)) 0)
        ((and (= s1 0) (= s2 0)) 0)
        (else
            (error "Invalid signals" (list s1 s2)))
    )
)

(define (inverter in1 out1)
    (define (invert-input)
        (let 
            ((new-value (logical-not (get-signal in1))))
            
            (after-delay inverter-delay
                (lambda () (set-signal! out1 new-value)))
        )
    )

    (add-action! in1 invert-input)
    (void)
)

(define (logical-not s)
    (cond 
        ((= s 0) 1)
        ((= s 1) 0)
        (else 
            (error "Invalid signal" s))
    )
)

;
; adder
;

(define (half-adder in1 in2 out1 out2)
    (let 
        ((im1 (make-wire)) (im2 (make-wire)))

        (or-gate in1 in2 im1)
        (and-gate in1 in2 out2)
        (inverter out2 im2)
        (and-gate im1 im2 out1)

        (void)
    )
)

(define (full-adder in1 in2 c-in sum c-out)
    (let 
        ((s  (make-wire))
         (c1 (make-wire))
         (c2 (make-wire)))
        
        (half-adder in2 c-in s c1)
        (half-adder in1 s sum c2)
        (or-gate c1 c2 c-out)
        (void)
    )
)


(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)


(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)

(propagate)

(set-signal! input-2 1)

(propagate)


; ex 3.31 
; http://wqzhang.wordpress.com/2009/07/28/sicp-exercise-3-31/
; Without running the procedure immediately after it is added to a wire, 
; the agenda table will be empty and no actions will be taken.
