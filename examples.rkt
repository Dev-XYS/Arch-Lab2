#lang racket

(require "analyzer2.rkt")

; Output stationary (X|Y)
#;(full-sim
   '(for b 0 1 1
      (for y 0 64 1
        (@global-buffer
         (for x 0 64 1
           (for k 0 3 1
             (parallel-for fy 0 3 1
               (parallel-for fx 0 3 1
                 (@register-file
                  (for c 0 3 1
                    (let ix (+ x fx)
                      (let iy (+ y fy)
                        (assign (O b k x y)
                                (+ (O b k x y)
                                   (* (I b c ix iy)
                                      (W k c fx fy))))))))))))))))

; Weight stationary 1 (F_X|F_Y)
#;(full-sim
   '(for b 0 1 1
      (for fy 0 3 1
        (for fx 0 3 1
          (for k 0 3 1
            (for c 0 3 1
              (@global-buffer
               (for y 0 64 1
                 (for x1 0 64 16
                   (parallel-for x2 0 16 1
                     (let x (+ x1 x2)
                       (@register-file
                        (let ix (+ x fx)
                          (let iy (+ y fy)
                            (assign (O b k x y)
                                    (+ (O b k x y)
                                       (* (I b c ix iy)
                                          (W k c fx fy))))))))))))))))))

; Row stationary (F_Y|Y)
#;(full-sim
   '(for b 0 1 1
      (for fy 0 3 1
        (@global-buffer
         (for y 0 64 1
           (for k 0 3 1
             (for c 0 3 1
               (for x1 0 64 16
                 (parallel-for x2 0 16 1
                   (let x (+ x1 x2)
                     (@register-file
                      (for fx 0 3 1
                        (let ix (+ x fx)
                          (let iy (+ y fy)
                            (assign (O b k x y)
                                    (+ (O b k x y)
                                       (* (I b c ix iy)
                                          (W k c fx fy))))))))))))))))))

; Weight stationary 2 (C|K)
#;(full-sim
   '(for b 0 1 1
      (for c 0 3 1
        (for k 0 3 1
          (for y 0 64 1
            (@global-buffer
             (for x 0 64 1
               (parallel-for fy 0 3 1
                 (parallel-for fx 0 3 1
                   (@register-file
                    (let ix (+ x fx)
                      (let iy (+ y fy)
                        (assign (O b k x y)
                                (+ (O b k x y)
                                   (* (I b c ix iy)
                                      (W k c fx fy))))))))))))))))

; Dilated convolution.
#;(full-sim
   '(let dilation-rate 2
      (for b 0 1 1
        (for k 0 3 1
          (@global-buffer
           (for c 0 3 1
             (for y 0 64 1
               (for x 0 64 1
                 (parallel-for fy 0 3 1
                   (parallel-for fx 0 3 1
                     (@register-file
                      (let ix (+ x (* fx dilation-rate))
                        (let iy (+ y (* fy dilation-rate))
                          (assign (O b k x y)
                                  (+ (O b k x y)
                                     (* (I b c ix iy)
                                        (W k c fx fy)))))))))))))))))
