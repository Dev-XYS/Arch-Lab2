#lang racket

; Single assignment.
(define single-assignment
  '(assign a (+ a b)))

; Single loop.
(define single-loop
  '(for i 0 16 2
     (assign (ref O (i)) (+ (ref I (i)) i))))

; Nested loop.
(define nested-loop
  '(for i 0 16 4
     (for j 0 4 1
       (assign (O (i j)) (+ (O (i j)) (I (j i)))))))

; Empty loop.
(define empty-loop
  '(for i 0 0 1
     (assign o i)))

; Nested loop with index dependence.
(define index-dependence
  '(for i 0 4 1
     (for j 0 i 1
       (assign (O (i j)) (I (i j))))))

; Let.
(define simple-let
  '(let limit 10
     (let step 2
       (for i 0 limit step
         (assign (O i) (I i))))))

; Let with simple calculation.
(define let-with-calculation
  '(for i 0 16 2
     (let ind (* i i)
       (assign (O ind) (I ind)))))

; Parallel for in the inner loop.
(define parallel-for-inner
  '(for i 0 4 1
     (parallel-for j 0 4 1
       (assign (O (i j)) (I (i j))))))

; Parallel for in the outer loop.
(define parallel-for-outer
  '(parallel-for i 0 4 1
     (for j 0 4 1
       (assign (O (i j)) (I (i j))))))

; Compuation using MAC.
(define MAC-computation
  '(for i 0 4 1
     (assign (O i) (+ (O i) (* (I i) (I i))))))

; Very simple annotation.
(define simple-annotation
  '(for i 0 2 1
     (@global-buffer
      (assign (O i) (I i)))))

; Program with annotation.
(define full-annotation
  '(@global-buffer
    (for i 0 4 1
      (@register-file
       (assign (O i) (+ (O i) (* (I i) (I i))))))))

; More complex program with annotation.
(define complex-annotation
  '(for i 0 2 1
     (@global-buffer
      (parallel-for j 0 2 1
        (@register-file
         (for k 0 2 1
           (assign (O i) (+ (O i) (* (I j) (I k))))))))))

; Program with multiple parallel-for and annotation.
(define multiple-parallel-with-annotation
 '(for i 0 2 1
    (@global-buffer
     (parallel-for j 0 2 1
       (parallel-for k 0 2 1
         (@register-file
          (assign (O i) (+ (O i) (* (I j) (I k))))))))))

(define real-convolution1
  '(for ox_0 0 4 1
     (for oy_0 0 4 1
       (@global-buffer
        (parallel-for ox_1 0 4 1
          (parallel-for oy_1 0 4 1
            (for ic 0 3 1
              (for wx 0 3 1
                (for wy 0 3 1
                  (let ix (+ (+ (* ox_0 4) ox_1) wx)
                    (let iy (+ (+ (* oy_0 4) oy_1) wy)
                      (@register-file
                       (assign (O ix iy) (+ (O ix iy) (* (I ic ix iy) (W ic wx wy))))))))))))))))

(provide (all-defined-out))
