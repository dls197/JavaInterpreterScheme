#lang racket
(require "simpleParser.rkt")
; A simple interpreter for Java
; 

; interpreter base, to run parser and pass to helper function interpret
(define interpreter
  (lambda (filename)
    (interpret (parser filename) initstate)
    ))

; initial state
(define initstate '(()()))

(define Mstate
  (lambda (exp state)
    (cond
      [(null? exp) state]
      [(eq? (operator exp) 'var) (var exp state)]
      [(eq? (operator exp) '=) (assign exp state)]
      (else state))))

; assignedVal checks if a given variable has been assigned a value. If it has, the value is returned. If not, an error is returned
(define assignedVal
  (lambda (var state)
    (assignedVal-split var (car state) (cadr state))))     ;calls on assignedVal-split and passes in the variable, the list of declared variables, and the list of declared variables' values

; assignedVal-split is called on by assignedVal. It is given the state in a split form, meaning the two lists inside state are inserted into the function separately
(define assignedVal-split
  (lambda (var dlist alist)
    (cond
      [(null? dlist) (error 'variable-not-assigned)]
      [(eq? var (car dlist)) (car alist)]
      (else (assignedVal-split var (cdr dlist) (cdr alist))))))

(define Mstate_remove
  (lambda (var state)
    (cond
      [(null? state) '(()())]
      )))

(define Mvalue
  (lambda (expression state)
    (cond
      ((eq? expression 'true) #t)
      ((eq? expression 'false) #f)
      ((not (list? expression)) (assignedVal expression state))
      ((eq? (operator expression) 'and) (and (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state)))
      ((eq? (operator expression) '&&) (and (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state)))
      ((eq? (operator expression) 'or) (or (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state)))
      ((eq? (operator expression) '||) (or (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state)))
      ((eq? (operator expression) '==) (eq? (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state)))
      ((eq? (operator expression) '!=) (not (eq? (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state))))
      ((eq? (operator expression) '<) (< (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state)))
      ((eq? (operator expression) '>) (> (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state)))
      ((eq? (operator expression) '<=) (<= (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state)))
      ((eq? (operator expression) '>=) (>= (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state)))
      ((eq? (operator expression) '!) (not (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state)))
      ((number? expression) expression)
      ((eq? (operator expression) '+) (+ (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '-) (- (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '*) (* (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '/) (quotient (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '%) (remainder (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      (else (error 'bad-operator)))))


(define M-integer
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((not (list? expression)) (assignedVal expression state))
      ((eq? (operator expression) '+) (+ (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '-) (- (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '*) (* (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '/) (quotient (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      ((eq? (operator expression) '%) (remainder (M-integer (leftoperand expression) state) (M-integer (rightoperand expression) state)))
      (else (error 'bad-operator)))))


(define M-boolean
  (lambda (expression state)
    (cond
      ((eq? expression 'true) #t)
      ((eq? expression 'false) #f)
      ((not (list? expression)) (assignedVal expression state))
      ((eq? (operator expression) 'and) (and (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state)))
      ((eq? (operator expression) '&&) (and (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state)))
      ((eq? (operator expression) 'or) (or (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state)))
      ((eq? (operator expression) '||) (or (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state)))
      ((eq? (operator expression) '==) (eq? (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state)))
      ((eq? (operator expression) '!=) (not (eq? (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state))))
      ((eq? (operator expression) '<)) (< (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state))
      ((eq? (operator expression) '>)) (> (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state))
      ((eq? (operator expression) '<=)) (<= (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state))
      ((eq? (operator expression) '>=)) (>= (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state))
      ((eq? (operator expression) '!)) (not (M-boolean (leftoperand expression) state) (M-boolean (rightoperand expression) state))
      (else (error 'bad-operator)))))


; ABSTRACTION
(define operator (lambda (expression) (car expression)))
(define leftoperand cadr)
(define rightoperand caddr)

     
#|
before:
state
value

Mbool and Mint taken up in operations in value

after:
initstate

Mstate
update state with expression (declare and assign, depending on var or =, large cond with operator with operans, cond on oparand [][][][]see lecture[][])

Mvalue
tiggered by operator, does not update state just returns, no change var, extracts info from state by evaluting expressions

declare value with no value (var, mstate) ((x) ())
change value with a value (=, mstate) ((x) (1+1)
prepared value return as single int/bool (called whenever, mvalue) ((x) (2))

value of var = value of right hand side

Mint and Mbool are subsections of Mvalue


|#

; interpreter helper function that takes a syntax tree Mstate Mvalue lists and breaks down to be processed using accumulation
(define interpret
  (lambda (tree Mstate)
    (cond
      [(null? tree) tree]
      ;[(list? (car tree)) ((interpret (car tree) Mstate) (interpret (cdr tree) Mstate))] ;potentially for interpreting nested statements
      [(eq? 'var (caar tree)) (interpret (cdr tree) (var (car tree) Mstate))]
      [(eq? '= (caar tree)) (interpret (cdr tree) (assign (car tree) Mstate))]
      )))
      

; declare variable, if its null or already declared just return Mstate IMPORTANT
; otherwise add to Mstate and add null to Mvalue
(define var
  (lambda (declis state)
    (cond
      [(null? declis) state]
      [(member? (cdr declis) (car state)) state]    ; this is how to check if something is declared
      [else (cons (cadr declis) (car state)) (cons null (cdr state))])))

; assign variables, if null return Mvalue
; if not declared error, else assign value
(define assign
  (lambda (=lis Mstate)
    (cond
      [(null? =lis) (cdr Mstate)]
      [(not (member? (cadr =lis) (car Mstate)) (error "Variable not recognized: must declare before assign"))]  
      [else (set (caddr =lis) (cdr Mstate) (indexOf (cadr =lis) (cdr Mstate) 0))])))

; print (return as output) variable
(define return
  (lambda (returnlis state)
    (cond
      [(null? returnlis) returnlis]
      [else (Mvalue returnlis state)])))

; if conditional, checks a given conditional in car(cdr) and runs car(cdr(cdr)) if #t or car(cdr(cdr(cdr))) sublist if #f
(define if
  (lambda (iflis state)
    (cond
      [(null? iflis) '()]
      [(null? (cdr iflis)) '()]
      [(M-boolean (cadr iflis) state) (Mstate (caddr iflis) state)]
      [else (if (cdddr iflis) state)]
      )))

; if conditional, checks a given conditional in car(cdr) and runs first sublist if #t or second sublist if #f, then it checks the conditional to possibly run itself again
(define while
  (lambda (whilelis state)
    (cond
      [(null? whilelis) '()]
      [(null? (cdr whilelis)) '()]
      [(M-boolean (cadr whilelis) state) (while whilelis (Mstate (cddr whilelis) state))]
      )))

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

; change value at index in list
(define set
  (lambda (val lis i)
    (cond
      [(null? lis) lis]
      [(eq? 0 i) (cons val (cdr lis))]
      [else (cons (car lis) (set val (cdr lis) (- i 1)))])))