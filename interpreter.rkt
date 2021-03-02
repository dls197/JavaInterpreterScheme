#lang racket
(require "simpleParser.rkt")

(define interpreter
  (lambda (filename)
    (parser filename)
    ))

(define var
  (lambda (varlis)
    (cond
      [(null? varlis) varlis])))

(define assign
  (lambda (=lis)
    (cond
      [(null? =lis) =lis])))

(define while
  (lambda (whilelis)
    (cond
      [(null? whilelis) whilelis])))

(define if
  (lambda (iflis)
    (cond
      [(null? iflis) iflis])))

(define return
  (lambda (returnlis)
    (cond
      [(null? returnlis) returnlis])))
