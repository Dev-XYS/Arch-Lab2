#lang racket

(require racket/hash)

;; Hardware configuration.

(define $energy-MAC 0.075)

; All 'analyze-*' procedures have a return value of format
#;(data cycles global-buffer-size register-file-size)

; Main procedure.
(define (analyze table)
  (cond
    ((@global-buffer? table) (analyze-@global-buffer table))
    ((@register-file? table) (analyze-@register-file table))
    ((sequential? table) (analyze-sequential table))
    ((parallel? table) (analyze-parallel table))
    (else #|assume that we have an access pattern|#
     (analyze-computation table))))

; Analyze a table with an @global-buffer annotation as root element.
(define (analyze-@global-buffer table)
  (let [(result (analyze (@body table)))]
    (modify-global-buffer-size
     result
     (get-total-size (@result-data result)))))

; Analyze a table with an @register-file annotation as root element.
(define (analyze-@register-file table)
  (let [(result (analyze (@body table)))]
    (modify-register-file-size
     result
     (get-total-size (@result-data result)))))

; Analyze a table with parallel computation.
(define (analyze-parallel table)
  (universal-loop-helper
   (@body table)
   (get-fold-func hash-force-union max max max +)
   (make-result empty-data 0 0 0 0)))

; Analyze a table with sequential computation.
(define (analyze-sequential table)
  (universal-loop-helper
   (@body table)
   (get-fold-func hash-force-union + max max +)
   (make-result empty-data 0 0 0 0)))

; Process a list of tables using a fold function.
(define (universal-loop-helper tables fold-func id)
  (if (null? tables)
      id
      (fold-func
       (analyze (car tables))
       (universal-loop-helper (cdr tables) fold-func id))))

; Get a lambda from fold functions of the four dimensions.
(define (get-fold-func
         data-fold
         cycles-fold
         buf-size-fold
         reg-size-fold
         energy-fold)
  (lambda (result1 result2)
    (make-result
     (data-fold (@result-data result1) (@result-data result2))
     (cycles-fold (@result-cycles result1) (@result-cycles result2))
     (buf-size-fold
      (@result-global-buffer-size result1)
      (@result-global-buffer-size result2))
     (reg-size-fold
      (@result-register-file-size result1)
      (@result-register-file-size result2))
     (energy-fold
      (@result-energy result1)
      (@result-energy result2)))))

; Analyze an computation pattern.
(define (analyze-computation pattern)
  (let [(read (@read pattern))
        (write (@write pattern))
        (computation (@computation pattern))]
    (make-result
     (get-hash-set-from-list (append read write))
     1 #|now we assume any computation uses 1 cycle|#
     0
     0
     $energy-MAC)))

;; Utility functions.

(define (@global-buffer? table)
  (and (list? table) (eq? (car table) '@global-buffer)))
(define (@register-file? table)
  (and (list? table) (eq? (car table) '@register-file)))
(define (sequential? table)
  (and (list? table) (eq? (car table) 'sequential)))
(define (parallel? table)
  (and (list? table) (eq? (car table) 'parallel)))

(define @body cadr)

(define @read car)
(define @write cadr)
(define @computation caddr)

(define (get-hash-set-from-list list)
  (if (null? list)
      (make-immutable-hash)
      (let [(hs (get-hash-set-from-list (cdr list)))]
        (hash-set hs (car list) #t))))

(define (hash-force-union hs1 hs2)
  (hash-union
   hs1 hs2
   #:combine/key (lambda (k v1 v2)
                   (if (equal? v1 v2)
                       v1
                       (error "hash-force-union: value not equal.")))))

;; Functions for manipulating results.

(define make-result list)

(define (caddddr x)
  (cadddr (cdr x)))

(define @result-data car)
(define @result-cycles cadr)
(define @result-global-buffer-size caddr)
(define @result-register-file-size cadddr)
(define @result-energy caddddr)

(define empty-data (make-immutable-hash))

; Modify the global buffer size dimension of a result, leaving others unchanged.
(define (modify-global-buffer-size result buf-size)
  (let [(data (@result-data result))
        (cycles (@result-cycles result))
        (reg-size (@result-register-file-size result))
        (energy (@result-energy result))]
    (make-result data cycles buf-size reg-size energy)))

; Modify the register file size dimension of a result, leaving others unchanged.
(define (modify-register-file-size result reg-size)
  (let [(data (@result-data result))
        (cycles (@result-cycles result))
        (buf-size (@result-global-buffer-size result))
        (energy (@result-energy result))]
    (make-result data cycles buf-size reg-size energy)))

; Get total size of a data collection.
; (Data is from a hash set.)
(define (get-total-size data)
  (hash-count data))

; Convert a result to pretty format.
(define (pretty-format result)
  (cons (get-total-size (car result)) (cdr result)))

;; Tests

(require "simulator.rkt")
(define get get-table-from-program)

(require "testcases.rkt")

(analyze (get single-assignment))
(analyze (get single-loop))
(analyze (get nested-loop))
(analyze (get empty-loop))

(analyze (get parallel-for-inner))
(analyze (get parallel-for-outer))

(analyze (get simple-annotation))
(analyze (get complex-annotation))

(pretty-format (analyze (get real-convolution1)))
