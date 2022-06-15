#lang racket

(require "plot.rkt")

(define (random-walk initial-value new-state-from-last-one should-stop?)
  (do ((i 1 (add1 i))
       (results
        (list (vector 0 initial-value))
        (cons (vector i (new-state-from-last-one (car results))) results)))
    ((should-stop? (car results))
     (reverse results))))

(define (move-bug step direction actual-position) 
  (apply (if (equal? direction 'left) + -) (list actual-position step)))

(define (move-bug-randomly step probability-left actual-position)
  (match-define (vector x y) actual-position)
  (move-bug step
            (if (> (random) probability-left)
                'left
                'right)
            y))

(define (is-dead? min max actual-position)
  (match-define (vector x y) actual-position)
  (not (<= min y max)))

(define (random-walk-bug min max initial-position step probability-left)
  (random-walk
   initial-position
   (curry move-bug-randomly step probability-left)
   (curry is-dead? min max)))

(define (plot-random-walk-bug min max initial-position step probability-left)
  (plot (lines (random-walk-bug min max initial-position step probability-left))))

(define (multiple-random-walk-bug min max initial-position step probability-left times #:key key)
  (build-list
      times
      (λ (n)
        (key
         (random-walk-bug min max initial-position step probability-left)))))

(define (plot-histogram-random-walk-bug min max initial-position step probability-left times bins)
  (plot
   (discrete-histogram
    (list->histogram
     (multiple-random-walk-bug min max initial-position step probability-left times #:key length)
     bins))))

(define (rate-of-successful-rescue min max initial-position step probability-left samples rescue-time)
  (/
   (apply
    +
    (multiple-random-walk-bug
     min max initial-position step probability-left samples
     #:key (λ (simulation)
             (if (<= (length simulation)
                     rescue-time)
                 0 1)))) samples))