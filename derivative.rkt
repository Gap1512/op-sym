#lang racket

(define (delta x)
  (let ((precision (expt 2.0 -36)))
  (if (zero? x)
      precision
      (* x precision))))

(define (slope fn a1 a2)
  (/ (- (fn a2) (fn a1)) (- a2 a1)))

(define (derivative fn a)
  (let ((delta (delta a)))
    (slope fn (- a delta) (+ a delta))))

(define (derivative-+ fn a)
    (slope fn a (+ a (delta a))))

(define (derivative-- fn a)
  (slope fn (- a (delta a)) a))