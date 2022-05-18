#lang racket

(define (delta (x 0))
  (let ((precision (expt 2.0 -30)))
  (if (zero? x)
      precision
      (* (expt 2 (round (log (abs x) 2))) precision))))

(define (slope fn a1 a2)
  (/ (- (fn a2) (fn a1)) (- a2 a1)))

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
