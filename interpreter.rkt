#lang racket
(require "simpleParser.rkt")
; A simple interpreter for Java
; Daniel Schnabel, Anthony Testa, and Oliver Traben
; 3/8/2021

; interpreter base, to run parser and pass to helper function interpret
(define interpreter
  (lambda (filename)
    (interpret (parser filename) initstate)))

; ABSTRACTION
(define operator (lambda (expression) (car expression)))
(define leftoperand cadr)
(define rightoperand caddr)

; gets the declaration list from the state
(define (getdlis state) (car state))

; gets the value list from the state
(define (getvlis state) (cadr state))

; gets the return value from the state
(define (getreturn state) (caddr state))

; gets the return value as a list from state
(define (getreturnlis state) (cddr state))

; getrs the value list and the return list from the state
(define (getvrlis state) (cdr state))

; initial state abstraction, the list format of the state
(define initstate '(()()()))

; return value abstraction, returns the third list (the list of return values) in the state
(define returnVal
  (lambda (state)
    (getreturn state)))

; END ABSTRACTION

; interpreter helper function that takes a syntax tree Mstate Mvalue lists and breaks down to be processed using accumulation
; each line of cond corresponds to a certain type of statement coming from the parser
(define interpret
  (lambda (tree state)
    (cond
      [(null? tree) (returnVal state)]
      [(or (eq? 'var (caar tree)) (eq? '= (caar tree)) (eq? 'return (caar tree))) (interpret (cdr tree) (Mstate (car tree) state))]
      [(eq? 'if (caar tree)) (interpret (cdr tree) (if (car tree) state))]
      [(eq? 'while (caar tree)) (interpret (cdr tree) (while (car tree) state))]
      [else (Mvalue(car tree) state)])))

; a version of the interpreter helper function used specifically by if statements,
; as if statements take in a subset of the larger parser tree,
; they are designed to reach a null tree before the rest of program and finished running (the usual state from which a null tree would be reached)
; as such, they cannot simply retrun the return value (returnVal) of the program, but must return the state as a whole so it can continue to be operated on
; this way, the rest of the program can recieve the proper state to continue processing the parser tree
(define if-interpret
  (lambda (tree state)
    (cond
      [(null? tree) state]
      [(or (eq? 'var (caar tree)) (eq? '= (caar tree)) (eq? 'return (caar tree))) (if-interpret (cdr tree) (Mstate (car tree) state))]
      [(eq? 'if (caar tree)) (if-interpret (cdr tree) (if (car tree) state))]
      [(eq? 'while (caar tree)) (cons (while (car tree) state) (if-interpret (cdr tree) state))]
      [else (Mvalue(car tree) state)])))

; Mstate takes an expression and a state and updates the state based on that expression
; The operator of this expression will be used to determine which helper function to call to update the state with
; a "var" will preform a declare; a "=" will preform an assignment; and a "return" will preform a return
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
    (assignedVal-split var (getdlis state) (getvlis state) state)))      ; calls on assignedVal-split and passes in the variable, the list of declared variables, and the list of declared variables' values

; assignedVal-split is called on by assignedVal. It is given the state in a split form, meaning the two lists inside state are inserted into the function separately
(define assignedVal-split
  (lambda (var dlist vlist originalState)
    (cond
      [(null? dlist) (error "Variable not recognized: must declare before assign")] ; Using a variable before it is declared
      [(and (null? (car vlist)) (eq? (car dlist) var)) (error "Variable not assigned a value")] ; Using a variable before it is assigned to a value, even though it is declared
      [(eq? var (car dlist)) (Mvalue (car vlist) originalState)]
      (else (assignedVal-split var (cdr dlist) (cdr vlist) originalState)))))

; Mvalue returns the value of a given expression (can either be an expression of integers or an expression of boolean values)
(define Mvalue
  (lambda (expression state)
    (cond
      ; true/false statements:
      ((eq? expression 'true) #t)
      ((eq? expression 'false) #f)
      ((eq? expression '#t) #t)
      ((eq? expression '#f) #f)
      ; numbers:
      ((number? expression) expression)
      ; variables:
      ;if the expression is not a list, not #t or #f, and not a number (indicated from the line above), then it must be a variable. Thus, assignedVal is called to get the variable's assigned value
      ((not (list? expression)) (assignedVal expression state))
      ; boolean operators:
      ((eq? (operator expression) 'and) (and (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '&&) (and (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) 'or) (or (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '||) (or (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '!) (not (Mvalue (leftoperand expression) state))) ; a unary operation
      ; comparison operators:
      ((eq? (operator expression) '==) (eq? (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '!=) (not (eq? (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state))))
      ((eq? (operator expression) '<) (< (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '>) (> (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '<=) (<= (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '>=) (>= (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ; mathetmatical operators:
      ((eq? (operator expression) '+) (+ (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((and (eq? (operator expression) '-) (eq? (length expression) 2)) (- (Mvalue (leftoperand expression) state))) ; a unary operation
      ((eq? (operator expression) '-) (- (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '*) (* (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '/) (quotient (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '%) (remainder (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ; error:
      (else (error 'bad-operator)))))

 
; declare variable, if its null just return state IMPORTANT
; if it has already been declared, produce a redefining error
; if the var is also an assign statement
; i.e. (length declis) = 3) then var is called again within an assign statement so the var will be added to the declaration list and the assigned value will be added to the value list
; otherwise, the var is added to Mstate in the declaration list
(define var
  (lambda (dlis state)
    (cond
      [(null? dlis) state]
      [(member? (cadr dlis) (getdlis state)) (error "Variable already declared")]    ; this is how to check if the var has been declared. If it has been, produce an error
      ; [see below] for if something is declared and assigned at the same time; =lis for assign takes '(= varname val) declis will have '(var varname val), however first atom is ignored in assign
      [(eq? (length dlis) 3) (assign dlis (var (cons (car dlis) (cons (cadr dlis) '())) state))] 
      [else (append (cons (cons (cadr dlis) (getdlis state)) (cons (cons '() (getvlis state)) '())) (getreturnlis state))])))

; assign variables, if null return state
; if the variable has not been declared yet, produce an error
; else, add the assigned value to the value list using the setVal helper method
(define assign
  (lambda (=lis state)
    (cond
      [(null? =lis) (getvrlis state)]
      [(not (member? (cadr =lis) (getdlis state))) (error "Variable not recognized: must declare before assign")]
      [else (setVal (cadr =lis) (Mvalue (caddr =lis) state) state)])))

; takes two atoms and the state, sets the value of a variable
; returns an updated state
(define setVal
  (lambda (var val state)
    (cond
      ; calls on setVal-split and passes in the variable, value, the list of declared variables, and the list of declared variables' values
      [else (append (cons (getdlis state) (cons (setVal-split var val (getdlis state) (getvlis state) state) '())) (getreturnlis state))])))

; takes two atom and two lists, sets the variable equal to the value specified
; the two lists are taken from the state, one being the list of declared variables and the other being the list of values for those variables. 
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
      [(eq? (Mvalue (cadr returnlis) state) '#t) (append (cons (getdlis state) (cons (getvlis state) '())) (cons 'true '()))]
      [(eq? (Mvalue (cadr returnlis) state) '#f) (append (cons (getdlis state) (cons (getvlis state) '())) (cons 'false '()))]
      [else (append (cons (getdlis state) (cons (getvlis state) '())) (cons (Mvalue (cadr returnlis) state) '()))])))

; takes a list and the state. If lis should be everything starting with "if" ex. (if (x > 10) (= y (+ y 2)) (...))
; returns the updated state that results from the while loop, or the original state if the iflis is null or there is no condition
; if the condition passes, if-interpret is called on the statement to be evaluated. See note on if-interpret for more info
(define if
  (lambda (iflis state)
    (cond
      [(null? iflis) state]
      [(null? (cdr iflis)) state]
      [(Mvalue (cadr iflis) state) (if-interpret (cons (caddr iflis) '()) (Mstate (caddr iflis) state))]
      [else (if-interpret (cdddr iflis) state)])))

; takes a list and the state. The list should be everything starting with "while" ex. (while (x > 10) (...)...)
; returns the state that results from the while loop, or the original state if the whilelis is null
; If the condition in the whilelis passes, it calls while again with an updated state from Mstate that has executed the inner loop of the while
(define while
  (lambda (whilelis state)
    (cond
      [(null? whilelis) state]
      [(null? (cdr whilelis)) state]
      [(Mvalue (cadr whilelis) state) (while whilelis (Mstate (caddr whilelis) state))]
      [else state])))

; takes an atom and a list
; returns a boolean, whether the atom is in the list or not
(define member?
  (lambda (a lis)
    (cond
      [(null? lis) #f]
      [(eq? a (car lis)) #t]
      [else (member? a (cdr lis))])))