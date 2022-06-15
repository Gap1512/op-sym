#lang racket

(require "plot.rkt" flomat)

(define (build-list-with-range n min max proc)
  (let ((step (/ (- max min) (sub1 n))))
    (build-list n (λ (x) (proc (+ min (* x step)))))))

(define (pseudo-inverse-from-list list order)
  (pinv (hat-x list order)))

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

(define (compare-polynomial-regression
         fn order min max samples
         (polynomial-regression-from-lists polynomial-regression-with-pseudo-inverse))
  (let* ((list-x (build-list-with-range samples min max identity))
         (list-y (build-list-with-range samples min max fn))
         (polynomial-regression
          (polynomial
           (polynomial-regression-from-lists list-x list-y order))))
    (plot
     (list
      (function fn min max #:color "blue" #:label "Original")
      (function polynomial-regression min max #:label "Regression")))))

(define (m-transpose-times-n m n)
  (times
   (transpose m)
   n))

(define (hat-x list order)
  (matrix
   (build-list
    (length list)
    (λ (i)
      (let ((xi (list-ref list i)))
        (build-list
         order
         (λ (j)
           (expt xi j))))))))

(define (polynomial-regression-with-inverse list-x list-y order)
  (let ((hat-x (hat-x list-x (add1 order))))
    (times (inv (m-transpose-times-n hat-x hat-x))
           (m-transpose-times-n hat-x (matrix list-y)))))
    

(define (benchmark x)
  (+ (* 10
        (exp (* -0.5 x))
        (sin (* 2 pi x)))
     (* 0.1 (random))))