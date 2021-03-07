#lang racket

; M-integer maps expressions to integer values
; (M-integer '(+ 3 5))   => 8
; (M-integer '(/ 9 3))   => 3
; (M-integer '(/ 8 3))   => 2
; (M-integer '(* (+ 4 3) (- 2 1))  => 7
; operators +, -, *, /, %
(define M-integer
  (lambda (expression)
    (cond
      ((number? expression) expression)
      ((eq? (operator expression) '+) (+ (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '-) (- (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '*) (* (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '/) (quotient (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '%) (remainder (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      (else (error 'bad-operator)))))

; ABSTRACTION
(define operator (lambda (expression) (cadr expression)))
(define leftoperand car)
(define rightoperand caddr)

