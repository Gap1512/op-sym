#lang racket

(require "plot.rkt" racket/fixnum)

(define random%
  (class object% (super-new)
    (init (seed (current-milliseconds)))
    (define this-seed seed)
    (define current-number this-seed)
    (define/public (generate-number) current-number)
    (define/public (generate-uniform-or-normalized-number (normalized? #f))
      (if normalized?
          (generate-box-muller-transformed-number)
          (generate-number)))
    (define/public (generate-box-muller-transformed-number)
      (let* ((u1 (generate-number))
             (u2 (generate-number))
             (common (sqrt (* -2 (if (zero? u1) 0 (log u1)))))
             (angle (* 2 pi u2)))
        (list
         (* common (cos angle))
         (* common (sin angle)))))
    (define/public (get-seed) this-seed)
    (define/public (plot-bitmap width height (normalized? #f))
      (flomap->bitmap
       (build-flomap 1 width height
                     (λ (_0 _1 _2)
                       (let ((value
                              (generate-uniform-or-normalized-number normalized?)))
                         (if (list? value) (car value) value))))))
    (define/public (plot-histogram samples bins (normalized? #f))
      (let ((list (build-list samples
                            (λ (_)
                              (generate-uniform-or-normalized-number normalized?)))))
        (plot
         (discrete-histogram
          (list->histogram
           (if (list? (car list)) (apply append list) list)
           bins)))))))

(define lcg%
  (class random%
    (init a c m (seed (current-milliseconds)))
    (super-new)
    (define this-m
      (if (< 0 m)
          m
          (raise 'm-must-be-positive)))
    (define this-a
      (if (< 0 a m)
          a
          (raise 'a-must-be-between-0-and-m)))
    (define this-c
      (if (and (<= 0 c) (< c m))
          c
          (raise 'c-must-be-between-0-and-m)))
    (define this-seed
      (if (and (<= 0 seed) (< seed m))
          seed
          (modulo seed m)))
    (define current-number this-seed)
    (define/override (generate-number)
      (set! current-number
            (modulo (+ (* this-a current-number) this-c)
                    this-m))
      (/ current-number this-m))))

(define wichman-hill%
  (class random%
    (init (s1 (modulo (current-milliseconds) 30269))
          (s2 (modulo (current-milliseconds) 30307))
          (s3 (modulo (current-milliseconds) 30323)))
    (super-new)
    (define this-s1 s1)
    (define this-s2 s2)
    (define this-s3 s3)
    (define this-seed s1)
    (define/override (generate-number)
      (set! this-s1 (modulo (* 171 this-s1)
                            30269))
      (set! this-s2 (modulo (* 172 this-s2)
                            30307))
      (set! this-s3 (modulo (* 170 this-s3)
                            30323))
      (let ((r (+ (/ this-s1 30269.0)
                  (/ this-s2 30307.0)
                  (/ this-s3 30323.0))))
        (- r (truncate r))))))

(define native%
  (class random%
    (super-new)
    (define/override (generate-number)
      (random))))