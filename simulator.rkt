#lang racket

;; Several terms are used throughout the code.

;; A program is like
#;(for i 0 16 1
    (for j 0 16 1
      (assign (O i j) (I i j))))

; The main simulator procedure.
(define (get-table-from-program prog)
  (get-table-from-stmt (make-empty-env) prog))

; Get the computation table for a statement (e.g. for loop, assignment, etc.).
(define (get-table-from-stmt env stmt)
  (cond ((assign? stmt) (get-table-from-assignment env stmt))
        ((for? stmt) (get-table-from-for env stmt))
        ((parallel-for? stmt (get-table-from-parallel-for env stmt)))
        (else '())))

(define (get-table-from-assignment env asgmt)
  (let [(asgmt (deep-map (lambda (name) (get-value-from-env env name)) asgmt))]
    (list (make-cell (get-read-list-from-expr (@assignor asgmt))
                     (singleton (@assignee asgmt))
                     (get-computation-from-expr (@assignor asgmt))))))

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

(define (get-table-from-parallel-for env pfor-stmt)
  '())

; Get computation list for an expression.
(define (get-computation-from-expr expr)
  '(MAC))

;; Main procedures.

(define (get-read-list-from-expr expr)
  (cond ((+? expr) (get-read-list-from-+ expr))
        ((*? expr) (get-read-list-from-* expr))
        ((value? expr) '())
        (else #|suppose that we got an array reference|#
         (get-read-list-from-ref expr))))

(define (get-read-list-from-+ +expr)
  (append
   (get-read-list-from-expr (@+fst +expr))
   (get-read-list-from-expr (@+snd +expr))))

(define (get-read-list-from-* *expr)
  (append
   (get-read-list-from-expr (@*fst *expr))
   (get-read-list-from-expr (@*snd *expr))))

(define (get-read-list-from-ref ref)
  (singleton ref))

;; Utility functions for statements.

; Check if a statement is an assignment.
(define (assign? stmt)
  (and (list? stmt) (eq? (car stmt) 'assign)))
; Check if a statement if a for loop.
(define (for? stmt)
  (and (list? stmt) (eq? (car stmt) 'for)))
; Check if a statement if a parallel for loop.
(define (parallel-for? stmt)
  (and (list? stmt) (eq? (car stmt) 'parallel-for)))

;; Utility function for manipulating for loops.

(define (caddddr x)
  (cadddr (cdr x)))
(define (cadddddr x)
  (caddddr (cdr x)))

(define @index cadr)
(define @base caddr)
(define @limit cadddr)
(define @step caddddr)
(define @body cadddddr)

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

(get-table-from-program
 '(assign a (+ a b)))

(get-table-from-program
 '(for i 0 16 2
    (assign (ref O (i)) (+ (ref I (i)) i))))

(get-table-from-program
 '(for i 0 16 4
    (for j 0 4 1
      (assign (O (i j)) (+ (O (i j)) (I (j i)))))))
