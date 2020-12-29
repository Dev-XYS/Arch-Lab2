#lang racket

;; A program is like
#;(for i 0 16 1
    (for j 0 16 1
      (assign (O i j) (I i j))))

; The main simulator procedure.
(define (get-table-from-program prog)
  (get-table-from-stmt (make-empty-env) prog))

; Get computation table for a statement (e.g. for loop, assignment, etc.).
(define (get-table-from-stmt env stmt)
  (let [(stmt (map (lambda (name) (get-value-from-env env name)) stmt))]
    (cond ((assign? stmt) (get-table-from-assignment env stmt))
          ((let? stmt) (get-table-from-let env stmt))
          ((for? stmt) (get-table-from-for env stmt))
          ((parallel-for? stmt (get-table-from-parallel-for env stmt)))
          (else '()))))

; Get computation table for an assignment.
; Note that assignment is atomic, we only do variable substitution in it without
; parsing its sub-structures.
(define (get-table-from-assignment env asgmt)
  (let [(asgmt (deep-map (lambda (name) (get-value-from-env env name)) asgmt))]
    (list (make-cell (get-read-list-from-expr (@assignor asgmt))
                     (singleton (@assignee asgmt))
                     (get-computation-from-expr (@assignor asgmt))))))

; Get computation table for a let statement.
; Syntax of 'let':
; (let <name> <expr'> <stmt>)
; Note that expr' is different from an expression. The former can only contain
; bound variables.
(define (get-table-from-let _env let-stmt)
  (define env (hash-copy _env))
  (let [(name (@let-name let-stmt))
        (value (eval-expr env (@let-value let-stmt)))
        (body (@let-body let-stmt))]
    (add-binding-to-env env name value)
    (get-table-from-stmt env body)))

; Get computation table for a for loop.
; Important: env is callee-saved.
(define (get-table-from-for _env for-stmt)
  (define env (hash-copy _env))
  (let [(base (@base for-stmt))
        (limit (@limit for-stmt))
        (step (@step for-stmt))
        (body (@body for-stmt))]
    (define (for-helper ind)
      (if (>= ind limit)
          '()
          (begin
            (add-binding-to-env env (@index for-stmt) ind)
            (append (get-table-from-stmt
                     env
                     body)
                    (for-helper (+ ind step))))))
    (for-helper base)))

; Get computation table for a parallel loop.
; (Not implemented yet.)
(define (get-table-from-parallel-for env pfor-stmt)
  '())

; Evaluate an expression (required to contain only bound variables).
(define (eval-expr env expr)
  (cond
    ((value? expr) expr)
    ((variable? expr) (get-value-from-env env expr))
    ((+? expr) (+ (eval-expr env (@+fst expr)) (eval-expr env (@+snd expr))))
    ((*? expr) (* (eval-expr env (@*fst expr)) (eval-expr env (@*snd expr))))
    (else (error "Expression cannot be evaluated."))))

; Get computation list for an expression.
; For now we assume expressions use only MAC.
(define (get-computation-from-expr expr)
  '(MAC))

; Get read list of an expression.
(define (get-read-list-from-expr expr)
  (cond ((+? expr) (get-read-list-from-+ expr))
        ((*? expr) (get-read-list-from-* expr))
        ((value? expr) '())
        (else #|suppose that we got an array reference|#
         (get-read-list-from-ref expr))))

; Get read list of an addition expression.
(define (get-read-list-from-+ +expr)
  (append
   (get-read-list-from-expr (@+fst +expr))
   (get-read-list-from-expr (@+snd +expr))))

; Get read list of a multiplication expression.
(define (get-read-list-from-* *expr)
  (append
   (get-read-list-from-expr (@*fst *expr))
   (get-read-list-from-expr (@*snd *expr))))

; Get read list of an array reference.
(define (get-read-list-from-ref ref)
  (singleton ref))

;; Utility functions for statements.

; Check if a statement is an assignment.
(define (assign? stmt)
  (and (list? stmt) (eq? (car stmt) 'assign)))
; Check if a statement is a let.
(define (let? stmt)
  (and (list? stmt) (eq? (car stmt) 'let)))
; Check if a statement if a for loop.
(define (for? stmt)
  (and (list? stmt) (eq? (car stmt) 'for)))
; Check if a statement if a parallel for loop.
(define (parallel-for? stmt)
  (and (list? stmt) (eq? (car stmt) 'parallel-for)))

;; Utility functions for manipulating for loops.

(define (caddddr x)
  (cadddr (cdr x)))
(define (cadddddr x)
  (caddddr (cdr x)))

(define @index cadr)
(define @base caddr)
(define @limit cadddr)
(define @step caddddr)
(define @body cadddddr)

;; Utility functions for manipulating let statements.

(define @let-name cadr)
(define @let-value caddr)
(define @let-body cadddr)

;; Utility functions for an assignment.

; Get the variable which the assignment assigns to.
(define @assignee cadr)
; Get the right-value of the assignment.
(define @assignor caddr)

;; Utility functions for expressions.

; Check if the expression is addition.
(define (+? expr)
  (and (list? expr) (eq? (car expr) '+)))
; Get the augend.
(define @+fst cadr)
; Get the addend.
(define @+snd caddr)

; Check if the expression is multiplication.
(define (*? expr)
  (and (list? expr) (eq? (car expr) '+)))
; Get the augend.
(define @*fst cadr)
; Get the addend.
(define @*snd caddr)

; Check if an expression is a value (e.g. constant integers).
; If an expression is a value, it will not be added to read list.
; For now we assume that values can only be integers.
(define (value? x)
  (or (integer? x)))

; Check if an expression is a variable (symbol in Racket).
(define variable? symbol?)

;; Utility functions for manipulating environments.

; The empty environment.
(define make-empty-env make-hash)

; Add a binding to an environment.
(define (add-binding-to-env env name value)
  (hash-set! env name value))

; Return the value of a binding.
; If the binding does not exist, return the name.
(define (get-value-from-env env name)
  (if (hash-has-key? env name)
      (hash-ref env name)
      name))

;; Functions for manipulating tables and cells.

(define singleton list)
(define make-cell list)

(define @read car)
(define @write cadr)
(define @computation caddr)

;; Miscellaneous functions.

(define (deep-map f x)
  (cond
    ((null? x) '())
    ((list? x) (cons (deep-map f (car x)) (deep-map f (cdr x))))
    (else (f x))))

;; Tests

(define get get-table-from-program)

; Single assignment.
(get
 '(assign a (+ a b)))

; Single loop.
(get
 '(for i 0 16 2
    (assign (ref O (i)) (+ (ref I (i)) i))))

; Nested loop.
(get
 '(for i 0 16 4
    (for j 0 4 1
      (assign (O (i j)) (+ (O (i j)) (I (j i)))))))

; Empty loop.
(get
 '(for i 0 0 1
    (assign o i)))

; Nested loop with index dependence.
(get
 '(for i 0 8 1
    (for j 0 i 1
      (assign (O (i j)) (I (i j))))))

; Let.
(get
 '(let limit 10
    (let step 2
      (for i 0 limit step
        (assign (O i) (I i))))))
