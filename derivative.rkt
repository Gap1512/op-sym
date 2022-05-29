#lang racket

(require  "commons.rkt")

(define (derivative fn a)
  (let ((delta (delta a)))
    (slope fn (- a delta) (+ a delta))))

(define (derivative-+ fn a)
    (slope fn a (+ a (delta a))))

(define (derivative-- fn a)
  (slope fn (- a (delta a)) a))

(define (partial-derivative fn a with-respect-to (derivative derivative))
  (derivative 
   (λ (x)
     (apply fn 
      (append (take a with-respect-to)
              (list x)
              (drop a (+ with-respect-to 1)))))
   (list-ref a with-respect-to)))

(define (second-derivative fn a)
  (let ((delta (delta a)))
    (/ (+ (- (fn (+ a delta))
             (* 2 (fn a)))
          (fn (- a delta)))
       (expt delta 2))))

(define (higher-derivative fn a order)
  (let ((delta (expt 2 (round (/ -30 order))))
        (order+1 (add1 order)))
    (letrec ((aux-sum (λ (k)
                        (* (expt (- 1) (+ k order))
                           (n-choose-k order k)
                           (fn (+ a (* k delta))))))
             (aux-sum-rec (λ (k sum)
                            (if (equal? k order+1) sum
                                (aux-sum-rec (add1 k) (+ sum (aux-sum k)))))))
      (exact->inexact (/ (aux-sum-rec 0 0)
                         (expt delta order))))))

(define (benchmark fn)
  (map (λ (benchmark) (fn (first benchmark) (second benchmark)))
       (list
        (list (λ (x) (expt x 2)) 4)
        (list (λ (x) (+ (* 2 x) 3)) 3)
        (list (λ (x) (- (* 3 x))) 1)
        (list (λ (x) (- (expt x 2) (* 3 x))) 2)
        (list (λ (x) (- (expt x 2) 4)) 0)
        (list (λ (x) (/ 1 x)) 2)
        (list (λ (x) (/ 1 x)) 5)
        (list (λ (x) (+ (- (expt x 2) (* 3 x)) 4)) 6))))