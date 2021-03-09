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

; setVal sets the value of a given variable, errors if it has not been declared yet
(define setVal
  (lambda (var val state)
    (cond
      ;;[(eq? (length (cadr state)) 1) (cons (car state) (cons (setVal-split var val (car state) (cadr state) state) '()))]
      [else (append (cons (car state) (cons (setVal-split var val (car state) (cadr state) state) '())) (cddr state))])))     ;calls on setVal-split and passes in the variable, value, the list of declared variables, and the list of declared variables' values

; setVal-split is called on by setVal. It is given the state in a split form, meaning the two lists inside state are inserted into the function separately
(define setVal-split
  (lambda (var val dlist vlist originalState)
    (cond
      [(null? dlist) (error 'variable-not-declared)]
      [(eq? (length dlist) 1) (cons val (car vlist))]
      ;[(and (eq? (length vlist) 1) (eq? var (car dlist))) (cons (cons val '()) '())]
      [(eq? var (car dlist)) (cons val (cdr vlist))]
      (else (cons (car vlist) (setVal-split var val (cdr dlist) (cdr vlist) originalState))))))

; print (return as output) variable
(define return
  (lambda (returnlis state)
    (cond
      [(null? returnlis) returnlis]
      [(eq? (Mvalue (cadr returnlis) state) '#t) (append (cons (car state) (cons (cadr state) '())) (cons 'true '()))]
      [(eq? (Mvalue (cadr returnlis) state) '#f) (append (cons (car state) (cons (cadr state) '())) (cons 'false '()))]
      [else (append (cons (car state) (cons (cadr state) '())) (cons (Mvalue (cadr returnlis) state) '()))])))

; if conditional, checks a given conditional in car(cdr) and runs car(cdr(cdr)) if #t or car(cdr(cdr(cdr))) sublist if #f
(define if
  (lambda (iflis state)
    (cond
      [(null? iflis) state]
      [(null? (cdr iflis)) state]
      [(Mvalue (cadr iflis) state) (if-interpret (cons (caddr iflis) '()) (Mstate (caddr iflis) state))]
      [else (if-interpret (cdddr iflis) state)]
      )))

; if conditional, checks a given conditional in car(cdr) and runs first sublist if #t or second sublist if #f, then it checks the conditional to possibly run itself again
(define while
  (lambda (whilelis state)
    (cond
      [(null? whilelis) state]
      [(null? (cdr whilelis)) state]
      [(Mvalue (cadr whilelis) state) (while whilelis (Mstate (caddr whilelis) state))]
      [else state]
      )))

; member? if list contains atom, for checking if var has been declared or has value, if x is member of Mstate or Mvalue
(define member?
  (lambda (a lis)
    (cond
      [(null? lis) #f]
      [(eq? a (car lis)) #t]
      [else (member? a (cdr lis))])))