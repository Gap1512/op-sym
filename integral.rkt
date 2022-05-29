#lang racket

(require  "commons.rkt")

(provide
 left-integral
 right-integral
 middle-integral
 trapezoid-integral
 integral
 simpson-1/3-rule
 simpson-3/8-rule
 gaussian-quadrature
 gaussian-quadrature-n2
 gaussian-quadrature-n3
 gaussian-quadrature-n4)

(define (left-integral fn a b n)
  (integral fn a b n 0))

(define (right-integral fn a b n)
  (integral fn a b n 1))

(define (middle-integral fn a b n)
  (integral fn a b n 0.5))

(define (trapezoid-integral fn a b n)
  (integral fn a b n 0 #t))

(define (integral fn a b (n 1000) (initial 0.5) (trapezoid #f))
  (let ((h (/ (- b a) n))
        (trapezoid-factor (if trapezoid 1 0)))
    (letrec ((sum-rec
              (λ (i sum)
                (if (>= i n)
                    sum
                    (sum-rec (add1 i)
                             (+ sum
                                (* h
                                   (/ (+ (fn
                                          (+ a (* (+ i initial) h)))
                                         (fn
                                          (+ a (* (+ i trapezoid-factor initial) h))))
                                      2))))))))
      (exact->inexact (sum-rec 0 0)))))

(define (simpson-1/3-rule fn a b)
  (* (/ (- b a) 6)
     (+ (fn a)
        (* 4 (fn (/ (+ a b) 2)))
        (fn b))))

(define (simpson-3/8-rule fn a b)
  (* (/ (- b a) 8)
     (+ (fn a)
        (* 3 (fn (/ (+ (* 2 a) b) 3)))
        (* 3 (fn (/ (+ a (* 2 b)) 3)))
        (fn b))))

(define (gaussian-quadrature fn a b wi xi)
  (let ((b-a/2 (/ (- b a) 2)))
    (* b-a/2
       (apply +
              (map (λ (w x)
                     (* w
                        (fn (+ (* b-a/2 x)
                               (/ (+ a b) 2)))))
                   wi xi)))))

(define (gaussian-quadrature-n2 fn a b)
  (let ((root (/ 1 (sqrt 3))))
    (gaussian-quadrature fn a b
                         (list 1 1)
                         (list (- root) root))))

(define (gaussian-quadrature-n3 fn a b)
  (let ((root (sqrt 3/5)))
    (gaussian-quadrature fn a b
                         (list 8/9 5/9 5/9)
                         (list 0 (- root) root))))

(define (gaussian-quadrature-n4 fn a b)
  (let ((root1 (sqrt (- 3/7 (* 2/7 (sqrt 6/5)))))
        (w1 (/ (+ 18 (sqrt 30)) 36))
        (root2 (sqrt (+ 3/7 (* 2/7 (sqrt 6/5)))))
        (w2 (/ (- 18 (sqrt 30)) 36)))
    (gaussian-quadrature fn a b
                         (list w1 w1 w2 w2)
                         (list (- root1) root1 (- root2) root2))))