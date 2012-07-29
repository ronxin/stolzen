#lang scheme

(require "constraints.scm")

(define (celsius-fahrenheit-converter c f)
    (let 
        ((u (make-connector))
         (v (make-connector))
         (w (make-connector))
         (x (make-connector))
         (y (make-connector)))

        (multiplier c w u)
        (multiplier v x u)
        (adder v y f)
        (constant 9 w)
        (constant 5 x)
        (constant 32 y)

        (void)
    )
)

(define C (make-connector))
(define F (make-connector))

(celsius-fahrenheit-converter C F)

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)


; The connectors communicate with the constraints by means of the procedures
; inform-about-value, which tells the given constraint that the connector has
; a value, and inform-about-no-value, which tells the constraint that the
; connector has lost its value.


(set-value! C 25 'user)
; Probe: Celsius temp = 25
; Probe: Fahrenheit temp = 77
; done

; (set-value! F 212 'user)
; Error! Contradiction (77 212)


(forget-value! C 'user)
; Probe: Celsius temp = ?
; Probe: Fahrenheit temp = ?
; done


; Now that F has no value, we are free to set it to 212:
(set-value! F 212 'user)
; Probe: Fahrenheit temp = 212
; Probe: Celsius temp = 100
; done
