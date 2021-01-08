#lang racket

; Mutable linked list.
(require "mlist.rkt")

; The result is in the format
#;(cycles
   main-memory-count
   global-buffer-count
   register-file-count
   computation-count
   buf-size
   reg-size)

;; Hardware configurations.

(define $energy-main-memory 200.0)
(define $global-buffer
  '((4096 6.0)
    (8192 9.0)
    (16384 13.5)
    (32768 20.25)
    (65536 30.375)))
(define $register-file
  '((2 0.03)
    (4 0.06)
    (8 0.12)
    (16 0.24)
    (32 0.48)
    (64 0.96)))
(define $energy-MAC 0.075)

; Analyze a table.
; Important: a table is a mutable list.
(define (analyze table buf regs)
  (define fold-result (make-result-fold + + + + + max max))
  (if (mnull? table)
      '(0 0 0 0 0 0 0)
      (let* [(first (mfirst table))
             (result (analyze-quantum first buf regs))]
        (fold-result
         result
         (analyze (mrest table) buf regs)))))

; Analyze a time quantum (an element of a table).
(define (analyze-quantum quantum buf regs)
  (define fold-result (make-result-fold max + + + + max max))
  (define (quantum-helper cells id)
    (if (mnull? cells)
        '(0 0 0 0 0 0 0)
        (fold-result (analyze-cell (mfirst cells) id buf regs)
                     (quantum-helper (mrest cells) (+ id 1)))))
  (if (quantum-register-file-load? quantum) #|register file load is special|#
      (analyze-register-file-load-quantum quantum regs)
      (quantum-helper quantum 0)))

; Analyze a cell (an element of a time quantum).
(define (analyze-cell cell id buf regs)
  (cond
    ((global-buffer-load? cell) (analyze-global-buffer-load cell buf))
    #;((register-file-load? cell) (analyze-register-file-load cell id regs))
    (else (analyze-computation cell))))

; Analyze a global buffer load operation.
(define (analyze-global-buffer-load cell buf)
  (let* [(data-set (@cell-data cell))
         (data-count (set-count data-set))
         (fetch-count
          (- data-count
             (set-count (set-intersect data-set buf))))]
    (set-set! buf data-set)
    (list
     0
     fetch-count
     fetch-count
     0
     0
     data-count
     0)))

; Analyze a quantum consisting solely of register file loads.
(define (analyze-register-file-load-quantum quantum regs)
  (define fold-result (make-result-fold max + + + + max max))
  ; The return value format is
  #;(cons result data)
  (define (helper quantum id regs)
    (if (mnull? quantum)
        (cons '(0 0 0 0 0 0 0) (set))
        (let* [(first (mfirst quantum))
               (data (@cell-data first))
               (reg (vector-ref regs id))
               (fetch (set-subtract data reg))
               (fetch-count (set-count fetch))
               (rest-return (helper (mrest quantum) (+ id 1) regs))
               (rest-result (car rest-return))
               (rest-data (cdr rest-return))]
          (cons #|helper return format|#
           (fold-result
            (list #|result format|#
             0
             0
             0
             fetch-count
             0
             0
             (set-count data))
            rest-result)
           (set-union data rest-data)))))
  (let* [(return (helper quantum 0 regs))
         (result (car return))
         (data (cdr return))]
    (fold-result
     (car return)
     (list 0 0 (set-count data) (@result-reg-count result) 0 0 0))))

; Analyze an atomic computation.
(define (analyze-computation cell)
  (let [(access-count (+ (set-count (@cell-read cell))
                         (set-count (@cell-write cell))))]
    (list
     1 #|assume all computation use MAC with 1 cycle|#
     0
     0
     access-count
     1
     0
     0)))

(define (make-result-fold c-fold mc-fold bc-fold rc-fold ce-fold b-fold r-fold)
  (lambda (result1 result2)
    (list
     (c-fold (@result-cycles result1) (@result-cycles result2))
     (mc-fold (@result-mem-count result1) (@result-mem-count result2))
     (bc-fold (@result-buf-count result1) (@result-buf-count result2))
     (rc-fold (@result-reg-count result1) (@result-reg-count result2))
     (ce-fold (@result-com-count result1) (@result-com-count result2))
     (b-fold (@result-buf-size result1) (@result-buf-size result2))
     (r-fold (@result-reg-size result1) (@result-reg-size result2)))))

;; For memory hierarchy.

(define (make-empty-global-buffer)
  (mutable-set))

(define (make-empty-register-files count)
  (for/vector ((_ count)) (mutable-set)))

;; Hardware configuration lookup and calculation.

(define (choose-hardware buf-size reg-size mem-count buf-count reg-count com-count)
  (define (hw-helper hws size)
    (let [(hw (car hws))]
      (if (>= (@hardware-size hw) size)
          hw
          (hw-helper (cdr hws) size))))
  (let* [(buf (hw-helper $global-buffer buf-size))
         (reg (hw-helper $register-file reg-size))
         (mem-energy (* $energy-main-memory mem-count))
         (buf-energy (* (@hardware-energy buf) buf-count))
         (reg-energy (* (@hardware-energy reg) reg-count))
         (com-energy (* $energy-MAC com-count))]
    (list #|for hardware result|#
     (@hardware-size buf)
     (@hardware-size reg)
     mem-energy
     buf-energy
     reg-energy
     com-energy
     (+ mem-energy buf-energy reg-energy com-energy))))

;; Utility functions.

(define (caddddr x)
  (cadddr (cdr x)))
(define (cadddddr x)
  (caddddr (cdr x)))
(define (caddddddr x)
  (cadddddr (cdr x)))

(define (quantum-register-file-load? quantum)
  (eq? (car (mfirst quantum)) 'register-file-load))

(define (global-buffer-load? cell)
  (eq? (car cell) 'global-buffer-load))

; For computation cells.
(define @cell-read car)
(define @cell-write cadr)
(define @cell-computation caddr)

; For load cells.
(define @cell-data cadr)

; For results.
(define @result-cycles car)
(define @result-mem-count cadr)
(define @result-buf-count caddr)
(define @result-reg-count cadddr)
(define @result-com-count caddddr)
(define @result-buf-size cadddddr)
(define @result-reg-size caddddddr)

(define (set-set! set1 set2)
  (set-clear! set1)
  (set-union! set1 set2))

; For hardware.
(define @hardware-size car)
(define @hardware-energy cadr)

; For hardware results.
(define @hw-result-buf-size car)
(define @hw-result-reg-size cadr)
(define @hw-result-mem-energy caddr)
(define @hw-result-buf-energy cadddr)
(define @hw-result-reg-energy caddddr)
(define @hw-result-com-energy cadddddr)
(define @hw-result-total-energy caddddddr)

;; Tests.

(require "simulator2.rkt")
(require "testcases.rkt")

(define print-width 8)
(define pw 12)

(define (pretty-print result hw-result)
  (displayln (format "      Cycles: ~a" (~r #:min-width pw (@result-cycles result))))
  (displayln (format "  Mem Access: ~a" (~r #:min-width pw (@result-mem-count result))))
  (displayln (format "  Buf Access: ~a" (~r #:min-width pw (@result-buf-count result))))
  (displayln (format "  Reg Access: ~a" (~r #:min-width pw (@result-reg-count result))))
  (displayln (format "    PE Usage: ~a" (~r #:min-width pw (@result-com-count result))))
  (displayln (format "     Min Buf: ~a words" (~r #:min-width pw (@result-buf-size result))))
  (displayln (format "     Min Reg: ~a words" (~r #:min-width pw (@result-reg-size result))))
  (displayln (format "  Chosen Buf: ~a words" (~r #:min-width pw (@hw-result-buf-size hw-result))))
  (displayln (format "  Chosen Reg: ~a words" (~r #:min-width pw (@hw-result-reg-size hw-result))))
  (displayln (format "  Mem Energy: ~a pJ" (~r #:min-width pw #:precision '(= 1) (@hw-result-mem-energy hw-result))))
  (displayln (format "  Buf Energy: ~a pJ" (~r #:min-width pw #:precision '(= 1) (@hw-result-buf-energy hw-result))))
  (displayln (format "  Reg Energy: ~a pJ" (~r #:min-width pw #:precision '(= 1) (@hw-result-reg-energy hw-result))))
  (displayln (format "   PE Energy: ~a pJ" (~r #:min-width pw #:precision '(= 1) (@hw-result-com-energy hw-result))))
  (displayln (format "Total Energy: ~a pJ" (~r #:min-width pw #:precision '(= 1) (@hw-result-total-energy hw-result)))))

;; Basic tests.

#;(analyze-cell
   `(,(set 1 2) ,(set 3 4) (MAC))
   0
   (make-empty-global-buffer)
   (make-empty-register-files 16))

#;(analyze-quantum
   (mlist `(,(set 1 2) ,(set 3 4) (MAC)))
   (make-empty-global-buffer)
   (make-empty-register-files 16))

;; Real tests.

(define (get prog)
  (car (get-table-from-program prog)))

(define (full-sim prog)
  (let* [(result (analyze (get prog) (make-buf) (make-regs)))
         (hw-result (choose-hardware (@result-buf-size result)
                                     (@result-reg-size result)
                                     (@result-mem-count result)
                                     (@result-buf-count result)
                                     (@result-reg-count result)
                                     (@result-com-count result)))]
    (pretty-print result hw-result)))

(define make-buf make-empty-global-buffer)

(define (make-regs)
  (make-empty-register-files 16))

#;(begin
    (analyze (get single-loop) (make-buf) (make-regs))
    (analyze (get naive-convolution1) (make-buf) (make-regs)))

;; Module.

(provide full-sim)
