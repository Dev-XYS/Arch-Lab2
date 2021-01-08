#lang racket

;; Mutable linked list.

; A mutable linked list is in the format
#;(mcons list last)

(define (empty-mlist)
  (mcons '() '()))

(define (singleton-mlist x)
  (let [(last (mcons x '()))]
    (mcons last last)))

(define (mlist . xs)
  (cond
    ((null? xs) (empty-mlist))
    ((null? (cdr xs)) (singleton-mlist (car xs)))
    (else (let [(rest (apply mlist (cdr xs)))]
            (set-mcar! rest (mcons (car xs) (mcar rest)))
            rest))))

(define (mlcons x mlist)
  (let* [(rest (mcar mlist))
         (new (mcons x rest))]
    (mcons new
           (if (null? rest)
               new
               (mcdr mlist)))))

(define (mfirst mlist)
  (mcar (mcar mlist)))

(define (mrest mlist)
  (let [(rest (mcdr (mcar mlist)))]
    (mcons rest (if (null? rest) '() (mcdr mlist)))))

(define (mappend mlist1 mlist2)
  (if (mnull? mlist1)
      (set-mcar! mlist1 (mcar mlist2))
      (set-mcdr! (mcdr mlist1) (mcar mlist2)))
  (if (mnull? mlist2)
      (void)
      (set-mcdr! mlist1 (mcdr mlist2)))
  mlist1)

(define (mnull? mlist)
  (null? (mcar mlist)))

(provide (all-defined-out))