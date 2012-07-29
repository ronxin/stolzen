#lang scheme

(require "constraints.scm")


(define I1 (make-connector))
(define I2 (make-connector))
(define O (make-connector))

(probe "I1" I1)
(probe "I2" I2)
(probe "O" O)

(multiplier I1 I2 O)

(set-value! I1 2 'user)

(set-value! O 4 'user)

(newline)
(newline)

(O 'informant)
(I1 'informant)
(I2 'informant)

(forget-value! I2 'user)
; I2 cannot be reset by user

(forget-value! O 'user)
; O can be reset, and multiple also resets I2

(forget-value! I1 'user)
; I1 also can be reset by user
