#lang racket

(require "plot.rkt" flomat)

(define (simulate-projectile h-0 v-0 g n duration)
  (let* ((delta (/ duration n))
         (coefficients (generate-coefficients-matrix n))
         (-delta^2g (- (* g (expt delta 2))))
         (results (times
                   (inv coefficients)
                   (matrix
                    (build-list
                     n
                     (λ (x)
                       (cond
                         ((zero? x) h-0)
                         ((eq? x 1) (+ (* delta v-0) h-0))
                         (else -delta^2g))))))))
    (build-list
     n
     (λ (y)
       (vector (* y delta)
               (ref results y 0))))))

(define (generate-coefficients-matrix n)
  (stack
   (augment (ones 1) (zeros 1 (sub1 n)))
   (augment (zeros 1) (ones 1) (zeros 1 (- n 2)))
   (sub
    (.+
     (.- (zeros n n)
         (.+ (eye n n 1) (eye n n 1)))
     (.+ (eye n n 0)
         (eye n n 2)))
    0 0 (- n 2) n)))