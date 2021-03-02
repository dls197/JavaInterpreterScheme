#lang racket
(require "simpleParser.rkt")
; A simple interpreter for Java
; 

; interpreter base, to run parser and pass to another function??
(define interpreter
  (lambda (filename)
    (parser filename)
    ))

; declare variable
(define var
  (lambda (varlis)
    (cond
      [(null? varlis) varlis])))

; reassign variables
(define assign
  (lambda (=lis)
    (cond
      [(null? =lis) =lis])))

; print (return as output) variable
(define return
  (lambda (returnlis)
    (cond
      [(null? returnlis) returnlis])))

; if conditional, checks a given conditional in car(cdr) and runs car(cdr(cdr)) if #t or car(cdr(cdr(cdr))) sublist if #f
(define if
  (lambda (iflis)
    (cond
      [(null? iflis) iflis])))

; if conditional, checks a given conditional in car(cdr) and runs first sublist if #t or second sublist if #f, then it checks the conditional to possibly run itself again
(define while
  (lambda (whilelis)
    (cond
      [(null? whilelis) whilelis])))

