#lang racket

(provide delta n-choose-k slope)

(define (delta (x 0))
  (let ((precision (expt 2.0 -30)))
  (if (zero? x)
      precision
      (* (expt 2 (round (log (abs x) 2))) precision))))

(define (n-choose-k n k)
  (if (zero? k) 1
      (/ (* n (n-choose-k (sub1 n) (sub1 k))) k)))

(define (slope fn a1 a2)
  (/ (- (fn a2) (fn a1)) (- a2 a1)))
