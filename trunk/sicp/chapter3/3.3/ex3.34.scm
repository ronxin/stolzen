#lang scheme

(require rackunit)
(require "constraints.scm")

(define (squarer a b)
    (multiplier a a b)
)

(define in-square  (make-connector))
(define out-square (make-connector))

(probe "in-square" in-square)
(probe "out-square" out-square)

(squarer in-square out-square)

(set-value! in-square 5 'user)
; Probe: out-square = 25
; Probe: in-square = 5
;
; okay

(forget-value! in-square 'user)
; Probe: out-square = ?
; Probe: in-square = ?

(set-value! out-square 16 'user)
; Probe: out-square = 16
;
; expect: Probe: in-square = 4, but it does not appear
; http://community.schemewiki.org/?sicp-ex-3.34
