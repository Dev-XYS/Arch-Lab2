#lang racket

(require racket/hash)

; Mutable linked list.
(require "mlist.rkt")

; A program is like
#;(for i 0 16 1
    (for j 0 16 1
      (assign (O i j) (I i j))))

;; Context free grammar of output:

; Table ->
#;(Quantum*)

; Quantum ->
#;(Cell*)

; Cell ->
#;(load-global-buffer (Data*))
#;(load-register-file (Data*))
#;(computation Computation)

;; Functions of name 'get-table-from-*' return a pair.
;; The first element of the list is the computation table.
;; The second is a list of all data involved.

; The main simulator procedure.
(define (get-table-from-program prog)
  (get-table-from-stmt empty-env prog))

; Get computation table for a statement (e.g. for loop, assignment, etc.).
(define (get-table-from-stmt env stmt)
  (let [(stmt (map (lambda (name) (get-value-from-env env name)) stmt))]
    (cond ((assign? stmt) (get-table-from-assignment env stmt))
          ((let? stmt) (get-table-from-let env stmt))
          ((for? stmt) (get-table-from-for env stmt))
          ((parallel-for? stmt) (get-table-from-parallel-for env stmt))
          ((@global-buffer? stmt) (get-table-from-@global-buffer env stmt))
          ((@register-file? stmt) (get-table-from-@register-file env stmt))
          (else (empty-mlist)))))

; Get computation table for an assignment.
; Note that assignment is atomic, we only do variable substitution in it without
; parsing its sub-structures.
(define (get-table-from-assignment env asgmt)
  (let* [(asgmt (deep-map (lambda (name) (get-value-from-env env name)) asgmt))
         (read-set (get-read-set-from-expr (@assignor asgmt)))
         (write-set (singleton-set (@assignee asgmt)))]
    (cons #|for the return format|#
     (mlist #|for the whole list|#
      (mlist #|for concurrency|#
       (make-cell read-set
                  write-set
                  (get-computation-from-expr (@assignor asgmt)))))
     (set-union read-set write-set))))

; Get computation table for a let statement.
; Syntax of 'let':
; (let <name> <expr'> <stmt>)
; Note that expr' is different from an expression. The former can only contain
; bound variables.
(define (get-table-from-let env let-stmt)
  (let [(name (@let-name let-stmt))
        (value (eval-expr env (@let-value let-stmt)))
        (body (@let-body let-stmt))]
    (get-table-from-stmt (env-modify-binding env name value) body)))

; Universal logic for processing for loop (sequential and parallel).
(define (universal-for-helper env for-stmt table-fold-func)
  (let [(index (@index for-stmt))
        (base (@base for-stmt))
        (limit (@limit for-stmt))
        (step (@step for-stmt))
        (body (@body for-stmt))
        (fold-func
         (lambda (first rest)
           (let [(tab-fst (car first))
                 (data-fst (cdr first))
                 (tab-snd (car rest))
                 (data-snd (cdr rest))]
             (cons #|for return format|#
              (table-fold-func tab-fst tab-snd)
              (set-union data-fst data-snd)))))]
    (define (for-helper ind)
      (if (>= ind limit)
          (cons #|for return format|# (empty-mlist) empty-data)
          (fold-func
           (get-table-from-stmt
            (env-modify-binding env index ind)
            body)
           (for-helper (+ ind step)))))
    (for-helper base)))

; Get computation table for a for loop.
(define (get-table-from-for env for-stmt)
  (universal-for-helper env for-stmt mappend))

; Get computation table for a parallel loop.
; (Not implemented yet.)
(define (get-table-from-parallel-for env pfor-stmt)
  (universal-for-helper
   env pfor-stmt
   (lambda (tab1 tab2)
     (mzip-map mappend tab1 tab2))))

; Get computation table for @global-buffer annotation.
(define (get-table-from-@global-buffer env stmt)
  (let* [(result (get-table-from-stmt env (@annot-body stmt)))
         (table (car result))
         (data (cdr result))]
    (cons #|for result format|#
     (mlcons #|append a global buffer load operation at the beginning of the table|#
      (mlist #|for concurrency|#
       `(global-buffer-load ,data))
      table)
     data)))

; Get computation table for @register-file annotation.
(define (get-table-from-@register-file env stmt)
  (let* [(result (get-table-from-stmt env (@annot-body stmt)))
         (table (car result))
         (data (cdr result))]
    (cons #|for result format|#
     (mlcons #|append a register file load operation at the beginning of the table|#
      (mlist #|for concurrency|#
       `(register-file-load ,data))
      table)
     data)))

; Evaluate an expression (required to contain only bound variables).
(define (eval-expr env expr)
  (cond
    ((value? expr) expr)
    ((variable? expr) (get-value-from-env env expr))
    ((+? expr) (+ (eval-expr env (@+fst expr)) (eval-expr env (@+snd expr))))
    ((*? expr) (* (eval-expr env (@*fst expr)) (eval-expr env (@*snd expr))))
    (else (error "Expression cannot be evaluated."))))

; Get computation list for an expression.
; If the computation pattern is a multiplication and then an addition (in that
; order), we denote it as 'MAC'. Otherwise, the operations are stored in a list.
(define (get-computation-from-expr expr)
  (define (helper expr)
    (cond
      ((+? expr) (append (get-computation-from-expr (@+fst expr))
                         (get-computation-from-expr (@+snd expr))
                         '(+)))
      ((*? expr) (append (get-computation-from-expr (@*fst expr))
                         (get-computation-from-expr (@*snd expr))
                         '(*)))
      (else '())))
  (let [(computation (helper expr))]
    (if (equal? computation '(* +))
        '(MAC)
        computation)))

; Get read list of an expression.
(define (get-read-set-from-expr expr)
  (cond ((+? expr) (get-read-set-from-+ expr))
        ((*? expr) (get-read-set-from-* expr))
        ((value? expr) empty-data)
        (else #|suppose that we got an array reference|#
         (get-read-set-from-ref expr))))

; Get read list of an addition expression.
(define (get-read-set-from-+ +expr)
  (set-union
   (get-read-set-from-expr (@+fst +expr))
   (get-read-set-from-expr (@+snd +expr))))

; Get read list of a multiplication expression.
(define (get-read-set-from-* *expr)
  (set-union
   (get-read-set-from-expr (@*fst *expr))
   (get-read-set-from-expr (@*snd *expr))))

; Get read list of an array reference.
(define (get-read-set-from-ref ref)
  (set ref))

;; Utility functions for statements.

; Check if a statement is an assignment.
(define (assign? stmt)
  (and (list? stmt) (eq? (car stmt) 'assign)))
; Check if a statement is a let.
(define (let? stmt)
  (and (list? stmt) (eq? (car stmt) 'let)))
; Check if a statement is a for loop.
(define (for? stmt)
  (and (list? stmt) (eq? (car stmt) 'for)))
; Check if a statement is a parallel for loop.
(define (parallel-for? stmt)
  (and (list? stmt) (eq? (car stmt) 'parallel-for)))

; Check if a statement is a @global-buffer annotation.
(define (@global-buffer? stmt)
  (and (list? stmt) (eq? (car stmt) '@global-buffer)))
; Check if a statement is a @register-file annotation.
(define (@register-file? stmt)
  (and (list? stmt) (eq? (car stmt) '@register-file)))

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
  (and (list? expr) (eq? (car expr) '*)))
; Get the augend.
(define @*fst cadr)
; Get the addend.
(define @*snd caddr)

;; Utility functions for annotations.

; Get the body of an annotation.
(define @annot-body cadr)

; Check if an expression is a value (e.g. constant integers).
; If an expression is a value, it will not be added to read list.
; For now we assume that values can only be integers.
(define (value? x)
  (or (integer? x)))

; Check if an expression is a variable (symbol in Racket).
(define variable? symbol?)

;; Utility functions for manipulating environments.

; The empty environment.
(define empty-env (make-immutable-hash))

; Add a binding to an environment.
(define (env-modify-binding env name value)
  (hash-set env name value))

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

(define (tab-annot-sequential? table)
  (and (list? table) (eq? (car table) 'sequential)))
(define (tab-annot-parallel? table)
  (and (list? table) (eq? (car table) 'parallel)))

(define @tab-annot-body cadr)

(define empty-data (set))

(define (singleton-set x)
  (set x))

;; Miscellaneous functions.

(define (deep-map f x)
  (cond
    ((null? x) '())
    ((list? x) (cons (deep-map f (car x)) (deep-map f (cdr x))))
    (else (f x))))

(define (zip-map f x y)
  (cond
    ((null? x) y)
    ((null? y) x)
    (else (cons (f (car x) (car y)) (zip-map f (cdr x) (cdr y))))))

; Used for mutable linked lists.
(define (mzip-map f x y)
  (cond
    ((mnull? x) y)
    ((mnull? y) x)
    (else (mlcons (f (mfirst x) (mfirst y)) (mzip-map f (mrest x) (mrest y))))))

(define (hash-force-union hs1 hs2)
  (hash-union
   hs1 hs2
   #:combine/key (lambda (k v1 v2)
                   (if (equal? v1 v2)
                       v1
                       (error "hash-force-union: value not equal.")))))

;; Module.

(provide get-table-from-program)

;; Tests.

(require "testcases.rkt")

(define (get prog)
  (deep-map
   (lambda (x)
     (if (hash? x)
         (hash-keys x)
         x))
   (car (get-table-from-program prog))))

(set! get get-table-from-program)

; Run the tests.
#;(begin
    (get single-assignment)
    (get single-loop)
    (get nested-loop)
    (get empty-loop)
    (get index-dependence)
    (get simple-let)
    (get let-with-calculation)
    (get parallel-for-inner)
    (get parallel-for-outer)
    (get MAC-computation)
    (get simple-annotation)
    (get full-annotation)
    (get complex-annotation)
    (get multiple-parallel-with-annotation))

#;(define test
    (get
     '(for b 0 1 1
        (for k 0 3 1
          (for c 0 3 1
            (for y 0 64 1
              (for x 0 64 1
                (for fy 0 3 1
                  (for fx 0 3 1
                    (let ix (+ x fx)
                      (let iy (+ y fy)
                        (assign (O b k x y) (+ (O b k x y) (* (I b c ix iy) (W k c fx fy)))))))))))))))
