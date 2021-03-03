#lang racket
(require "simpleParser.rkt")
; A simple interpreter for Java
; 

; interpreter base, to run parser and pass to helper function interpret
(define interpreter
  (lambda (filename)
    (interpret (parser filename) '() '())
    ))

; interpreter helper function that takes a syntax tree Mstate Mvalue lists and breaks down to be processed using accumulation
(define interpret
  (lambda (tree Mstate Mvalue)
    (cond
      [(null? tree) tree]
      ;[(list? (car tree)) ((interpret (car tree) Mstate Mvalue) (interpret (cdr tree) Mstate Mvalue))] potentially for interpreting nested statements
      [(eq? 'var (caar tree)) (interpret (cdr tree) (var (car tree) Mstate Mvalue) Mvalue)]
      [(eq? '= (caar tree)) (interpret (cdr tree) Mstate (assign (car tree) Mstate Mvalue))]
      )))

; declare variable, if its null or already declared just return Mstate IMPORTANT
; otherwise add to Mstate and add null to Mvalue
(define var
  (lambda (declis Mstate Mvalue)
    (cond
      [(null? declis) Mstate]
      [(member? (cdr declis) Mstate) Mstate]    ; this is how to check if something is declared
      [else (cons (cdr declis) Mstate) (cons null Mvalue)])))

; assign variables, if null return Mvalue
; if not declared error, else assign value
(define assign
  (lambda (=lis Mstate Mvalue)
    (cond
      [(null? =lis) Mvalue]
      [(not (member? (cadr =lis) Mstate) (error "Variable not recognized: must declare before assign"))]  
      [else (set (caddr =lis) Mvalue (indexOf (cadr =lis) Mstate 0))])))

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

; member? if list contains atom, for checking if var has been declared or has value, if x is member of Mstate or Mvalue
(define member?
  (lambda (a lis)
    (cond
      [(null? lis) #f]
      [(eq? a (car lis)) #t]
      [else (member? a (cdr lis))])))

; indexOf gets the index of an atom in a list
(define indexOf
  (lambda (a lis i)
    (cond
      [(null? lis) -1]
      [(eq? a (car lis)) i]
      [else (indexOf a (cdr lis) (+ i 1))])))

; change vlaue at index in list
(define set
  (lambda (val lis i)
    (cond
      [(null? lis) lis]
      [(eq? 0 i) (cons val (cdr lis))]
      [else (cons (car lis) (set val (cdr lis) (- i 1)))])))