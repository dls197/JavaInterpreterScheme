#lang racket
(require "simpleParser.rkt")
; A simple interpreter for Java
; Daniel Schnabel, Anthony Testa, and Oliver Traben
; 3/8/2021

; interpreter base, to run parser and pass to helper function interpret
(define interpreter
  (lambda (filename)
    (interpret (parser filename) initstate)
    ))

; initial state
(define initstate '(()()()))

; return value
(define returnVal
  (lambda (state)
    (caddr state)))

; interpreter helper function that takes a syntax tree Mstate Mvalue lists and breaks down to be processed using accumulation
(define interpret
  (lambda (tree state)
    (cond
      [(null? tree) (returnVal state)]
      [(or (eq? 'var (caar tree)) (eq? '= (caar tree)) (eq? 'return (caar tree))) (interpret (cdr tree) (Mstate (car tree) state))]
      [(eq? 'if (caar tree)) (interpret (cdr tree) (if (car tree) state))]
      [(eq? 'while (caar tree)) (interpret (cdr tree) (while (car tree) state))]
      [else (Mvalue(car tree) state)]
      )))

(define if-interpret
  (lambda (tree state)
    (cond
      [(null? tree) state]
      [(or (eq? 'var (caar tree)) (eq? '= (caar tree)) (eq? 'return (caar tree))) (if-interpret (cdr tree) (Mstate (car tree) state))]
      [(eq? 'if (caar tree)) (if-interpret (cdr tree) (if (car tree) state))]
      [(eq? 'while (caar tree)) (cons (while (car tree) state) (if-interpret (cdr tree) state))]
      [else (Mvalue(car tree) state)]
      )))

(define Mstate
  (lambda (exp state)
    (cond
      [(null? exp) state]
      [(eq? (operator exp) 'var) (var exp state)]
      [(eq? (operator exp) '=) (assign exp state)]
      [(eq? (operator exp) 'return) (return exp state)]
      (else state))))

; assignedVal checks if a given variable has been assigned a value. If it has, the value is returned. If not, an error is returned
(define assignedVal
  (lambda (var state)
    (assignedVal-split var (car state) (cadr state) state)))     ;calls on assignedVal-split and passes in the variable, the list of declared variables, and the list of declared variables' values

; assignedVal-split is called on by assignedVal. It is given the state in a split form, meaning the two lists inside state are inserted into the function separately
(define assignedVal-split
  (lambda (var dlist alist originalState)
    (cond
      [(null? dlist) (error "Variable not recognized: must declare before assign")]
      [(and (null? (car alist)) (eq? (car dlist) var)) (error "Variable not assigned a value")]
      [(eq? var (car dlist)) (Mvalue (car alist) originalState)]
      (else (assignedVal-split var (cdr dlist) (cdr alist) originalState)))))

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
      ((eq? expression '#t) #t)
      ((eq? expression '#f) #f)
      ((number? expression) expression)
      ((not (list? expression)) (assignedVal expression state))
      ((eq? (operator expression) 'and) (and (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '&&) (and (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) 'or) (or (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '||) (or (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '==) (eq? (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '!=) (not (eq? (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state))))
      ((eq? (operator expression) '<) (< (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '>) (> (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '<=) (<= (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '>=) (>= (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '!) (not (Mvalue (leftoperand expression) state)))
      ((eq? (operator expression) '+) (+ (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((and (eq? (operator expression) '-) (eq? (length expression) 2)) (- (Mvalue (leftoperand expression) state)))
      ((eq? (operator expression) '-) (- (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '*) (* (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '/) (quotient (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '%) (remainder (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      (else (error 'bad-operator)))))

; ABSTRACTION
(define operator (lambda (expression) (car expression)))
(define leftoperand cadr)
(define rightoperand caddr)   

; declare variable, if its null or already declared just return Mstate IMPORTANT
; otherwise add to Mstate and add null to Mvalue
(define var
  (lambda (declis state)
    (cond
      [(null? declis) state]
      [(member? (cadr declis) (car state)) (error "Variable already declared")]    ; this is how to check if something is declared
      [(eq? (length declis) 3) (assign declis (var (cons (car declis) (cons (cadr declis) '())) state))]   ; =lis for assign takes '(= varname val) declis will have '(var varname val), however first atom is ignored in assign
      ;[(eq? (length declis) 3) (cons (cons (cadr declis) (car state)) (cons (assign declis state) (cadr state)))] ; if something is declared and assigned at the same time
      [else (append (cons (cons (cadr declis) (car state)) (cons (cons '() (cadr state)) '())) (cddr state))])))

; assign variables, if null return Mvalue
; if not declared error, else assign value
(define assign
  (lambda (=lis state)
    (cond
      [(null? =lis) (cdr state)]
      [(not (member? (cadr =lis) (car state))) (error "Variable not recognized: must declare before assign")]
      [else (setVal (cadr =lis) (Mvalue (caddr =lis) state) state)])))

; takes two atoms and the state, sets the value of a variable
; returns an updated state
(define setVal
  (lambda (var val state)
    (cond
      [else (append (cons (car state) (cons (setVal-split var val (car state) (cadr state) state) '())) (cddr state))])))     ;calls on setVal-split and passes in the variable, value, the list of declared variables, and the list of declared variables' values

; takes two atom and two lists, sets the variable equal to the value specified
; returns an updated state
; only called by setVal
(define setVal-split
  (lambda (var val dlist vlist originalState)
    (cond
      [(null? dlist) (error 'variable-not-declared)]
      [(eq? (length dlist) 1) (cons val (car vlist))]
      [(eq? var (car dlist)) (cons val (cdr vlist))]
      (else (cons (car vlist) (setVal-split var val (cdr dlist) (cdr vlist) originalState))))))

; takes a list and the state. returnlis should be everything start with "return" ex. (return x)
; returns the updated state, with the third element now being the evaluation of the second element in the returnlis ex. '((x)(2)6) with 6 being the return value
(define return
  (lambda (returnlis state)
    (cond
      [(null? returnlis) returnlis]
      [(eq? (Mvalue (cadr returnlis) state) '#t) (append (cons (car state) (cons (cadr state) '())) (cons 'true '()))]
      [(eq? (Mvalue (cadr returnlis) state) '#f) (append (cons (car state) (cons (cadr state) '())) (cons 'false '()))]
      [else (append (cons (car state) (cons (cadr state) '())) (cons (Mvalue (cadr returnlis) state) '()))])))

; takes a list and the state. If lis should be everything starting with "if" ex. (if (x > 10) (= y (+ y 2)) (...))
; returns the updated state that results from the while loop, or the original state if the iflis is null or there is no condition
; if the condition passes, if-interpret is called on the statement to be evaluated. See note on if-interpret for more info
(define if
  (lambda (iflis state)
    (cond
      [(null? iflis) state]
      [(null? (cdr iflis)) state]
      [(Mvalue (cadr iflis) state) (if-interpret (cons (caddr iflis) '()) (Mstate (caddr iflis) state))]
      [else (if-interpret (cdddr iflis) state)]
      )))

; takes a list and the state. The list should be everything starting with "while" ex. (while (x > 10) (...)...)
; returns the state that results from the while loop, or the original state if the whilelis is null
; If the condition in the whilelis passes, it calls while again with an updated state from Mstate that has executed the inner loop of the while
(define while
  (lambda (whilelis state)
    (cond
      [(null? whilelis) state]
      [(null? (cdr whilelis)) state]
      [(Mvalue (cadr whilelis) state) (while whilelis (Mstate (caddr whilelis) state))]
      [else state]
      )))

; takes an atom and a list
; return a boolean, whether the atom is in the list or not
(define member?
  (lambda (a lis)
    (cond
      [(null? lis) #f]
      [(eq? a (car lis)) #t]
      [else (member? a (cdr lis))])))