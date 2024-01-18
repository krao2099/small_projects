;;#lang racket
;;
;; interpA
;; CSC 417, Fall, 2023
;;

(define assert
  (lambda values
    (cond ((null? values)		     ; no args given
	   (error "Our custom assert function expects at least one arg"))
	  ((null? (cdr values))		     ; one arg: test for true value
	   (or (car values)
	       (error "Assertion failed")))
	  (#t
	   (or (equal? (car values) (cadr values)) ; two args: test for equality
	       (error "Assertion failed: ~S not equal to ~S" 
		      (car values) (cdr values)))))))

;; The definition of 'any' in Racket is not standard.  Here's a generic one:
(define any-are
  (lambda (predicate? ls)
    (cond ((null? ls)
	   #f)
	  ((predicate? (car ls))
	   (car ls))
	  (#t
	   (any-are predicate? (cdr ls))))))
	
;; The implementation of 'map' in Racket throws an error if all the lists are
;; not the same length.  Here's one that conforms to the more common
;; convention that 'map' stops as soon as one of its list args is null.
(define flexible-map
  (lambda (fn . lists)
    (let loop ((lists lists)    ;; The "named let" creates a local function
	       (results '()))
      (if (any-are null? lists)
	  (reverse results)
	  (loop (map cdr lists)
		(cons (apply fn (map car lists))
		      results))))))

(define desugar
  (lambda (form env)
    (cond ((not (list? form))
	   form)
	  ((null? form)
	   (list 'error "Desugar error: null list / empty expression"))
	  ((eq? (car form) 'let)
	   (desugar-let form env))
	  ((eq? (car form) 'fn)
	   (let ((arglist (cadr form))
		 (body (caddr form))
		 (env (caddr form)))
	     (list 'fn arglist (desugar body env) env)))
	  (#t 
	   form))))

(define desugar-let
  (lambda (letform env)
    (assert (and (list? letform) 
		 (eq? 'let (car letform))
		 (= 4 (length letform))))
    (let ((var (cadr letform))
	  (exp (caddr letform))
	  (body (cadddr letform)))
      ;; Make a function and make an application of it
      (let ((f (list 'fn 
		     (list var)
		     (desugar body env)
		     env)))
	(list f (desugar exp env))))))

(define cell? vector?)

;; Traditionally, global variables are named with stars
(define *initial-env*
  (list
   (cons 'eq (lambda (a b) (if (= a b) 1 0)))
   (cons 'mul *)
   (cons 'div /)
   (cons 'add +)
   (cons 'sub -)
   (cons 'v 5)
   (cons 'i 1)
   (cons 'x 10)
   (cons 'c 100)))

;; The built-in operators in our environment are Scheme procedures
(define built-in? procedure?)

(define name->value 
  (lambda (name value) 
    (cons name value)))

(define env-entry-maker name->value)

(define make-env
  (lambda ()
    (map env-entry-maker 
	 (map car *initial-env*)    ;; names
	 (map cdr *initial-env*)))) ;; initial values

(define lookup
  (lambda (key tbl)
    (and (not (null? tbl))
	 (let ((entry (car tbl)))
	   (if (eq? key (car entry))
	       (cdr entry)
	       (lookup key (cdr tbl)))))))

(define make-function
  (lambda (arglist body env)
    (list 'function arglist body env)))

(define function?
  (lambda (rator)
    (and (list? rator)
	 (eq? 'function (car rator)))))

(define uneval?
  (lambda (obj)
    (and (list? obj)
	 (eq? 'uneval (car obj)))))

(define error?
  (lambda (exp)
    (and (list? exp)
	 (eq? 'error (car exp)))))

(define display-obj
  (lambda (obj)
    (cond
     ;; Print 'unevaluated' data structures without the env
     ((uneval? obj)
      (for-each display 
		(list "Unevaluated exp: " (cadr obj) "\n")))
     ;; Print function data structures without the env
     ((function? obj)
      (for-each display 
		(list "User-defined function: " (cadr obj) (caddr obj) "\n")))
     ((error? obj)
      (for-each display 
		(list "Interpreter error: " (cadr obj) "\n" (caddr obj) "\n")))

     (#t
      ;; Else we have something easier to print
      (display obj)
      (newline)))))

(define extended-env
  (lambda (env identifiers values)
    (append (flexible-map env-entry-maker identifiers values) env)))

(define prep-operand
  (lambda (exp env) 
    (ev exp env)))

;; Run the user-defined function
(define apply-function 
  (lambda (rator rands env)
    (let ((arglist (cadr rator))
	  (body (caddr rator))
	  (function-env (cadddr rator))	     ; [[1]]
	  (rands (map (lambda (rand)
			(prep-operand rand env))
		      rands)))
      ;(for-each display-obj (cons "Arglist: " arglist))
      ;(for-each display-obj (cons "Operands: " rands))
      ;(for-each display (cons "Function env: " function-env))
      (ev body (extended-env function-env arglist rands))))) ; [[2]]

(define application?
  (lambda (exp)
    (and (list? exp)
	 (not (null? exp)))))

(define do-application
  (lambda (exp env)
    (let ((rator (ev (car exp) env))
	  (rands (cdr exp)))
      (cond 
       ((function? rator)
	(apply-function rator rands env))    ; [[3]]
       ((built-in? rator)
	(let ((rands (map (lambda (rand) (ev rand env)) rands)))
	  (let ((errs (any-are error? rands)))
	    (or errs
		(apply rator rands)))))	     ; [[4]]
       (#t
	(list 'error "Unhandled application type" exp))))))

(define do-if
  (lambda (rands env)
    (let ((condition (car rands))	     ; first elt
	  (then-exp (cadr rands))	     ; second elt
	  (else-exp (caddr rands)))	     ; third elt
      (let ((result (ev condition env)))
	(if (not (zero? result))	     ; Only zero is false
	    (ev then-exp env)
	    (ev else-exp env))))))

(define special-form?
  (lambda (exp)
    (and (list? exp)
	 (not (null? exp))
	 (memq (car exp) '(if fn alter)))))

;; Special forms are special because they do not evaluate all of
;; their parts.
(define do-special-form
  (lambda (exp env)
    (let ((keyword (car exp))
	  (rest (cdr exp)))
      (cond ((eq? keyword 'if)
	     (do-if rest env))
	    ((eq? keyword 'fn)
	     (let ((arglist (car rest))
		   (body (cadr rest)))
	       (make-function arglist body env))) ; [[5]]
	    (#t
	     (list 'error "Unknown special form" exp))))))

(define ev 
  (lambda (exp env)
    (cond ((or (error? exp)
	       (number? exp)
	       (built-in? exp)
	       (function? exp))
	   ;; These evaluate to themselves
	   exp)
	  ((symbol? exp)
	   (let ((item (lookup exp env)))
	     (cond ((not item)
		    (list 'error "No binding for this name" exp))
		   (#t 			     ; else just a value
		    item))))
	  ((uneval? exp)
	   (let ((exp (cadr exp))
		 (env (caddr exp)))
	     (ev exp env)))
	  ((special-form? exp)
	   (do-special-form exp env))
	  ((application? exp)
	   (do-application exp env))
	  ((built-in? exp)
	   exp)
	  (#t
	   (list 'error "Unhandled expression type" exp)))))

(define run
  (lambda (form)
    (let ((env (make-env)))
      (ev (desugar form env) env))))

;; -----------------------------------------------------------------------------
;; TESTS
;; -----------------------------------------------------------------------------

(define test
  (lambda ()
    (let ((ev1 (lambda (exp) (ev exp (make-env)))))
      (assert 5 (ev1 5))
      (assert * (ev1 'mul))
      (assert (function? (ev1 '(fn () 1))))
      (assert 1 (ev1 '((fn () 1) 1000)))
      (assert 1000 (ev1 '((fn (n) n) 1000)))
      (assert 27 (ev1 '((fn (k n) (mul k n)) 3 9)))
      (assert 25 (ev1 '((fn (n) (mul n n)) 5)))
      )))

(begin
  (display "Running tests...\n")
  (test)
  (display "All tests passed.\n"))
