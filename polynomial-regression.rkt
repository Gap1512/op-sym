#lang racket

(require "plot.rkt" flomat)

(define (build-list-with-range n min max proc)
  (let ((step (/ (- max min) (sub1 n))))
    (build-list n (λ (x) (proc (+ min (* x step)))))))

(define (pseudo-inverse-from-list list order)
  (pinv
   (matrix
    (build-list
     (length list)
     (λ (i)
       (let ((xi (list-ref list i)))
         (build-list
          order
          (λ (j)
            (expt xi j)))))))))

(define (polynomial-regression-with-pseudo-inverse list-x list-y order)
  (times
   (pseudo-inverse-from-list list-x (add1 order))
   (matrix list-y)))

(define (polynomial coefficients)
  (let ((order (size coefficients)))
    (lambda (x)
      (apply
       +
       (build-list
        order
        (λ (n)
          (* (ref coefficients n 0)
             (expt x n))))))))

(define (compare-polynomial-regression-with-pseudo-inverse fn order min max samples)
  (let* ((list-x (build-list-with-range samples min max identity))
         (list-y (build-list-with-range samples min max fn))
         (polynomial-regression
          (polynomial
           (polynomial-regression-with-pseudo-inverse list-x list-y order))))
    (plot
     (list
      (function fn min max #:color "blue" #:label "Original")
      (function polynomial-regression min max #:label "Regression")))))

(define (benchmark x)
  (+ (* 10
        (exp (* -0.5 x))
        (sin (* 2 pi x)))
     (* 0.1 (random))))