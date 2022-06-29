#lang racket

(require "plot.rkt")

(provide simulate)

(define (simulate initial-value new-state-from-last-one should-stop?)
  (do ((i 1 (add1 i))
       (results
        (list (vector 0 initial-value))
        (cons (vector i (new-state-from-last-one (car results))) results)))
    ((should-stop? (car results) i)
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

(define (is-dead? min max actual-position _)
  (match-define (vector x y) actual-position)
  (not (<= min y max)))

(define (random-walk-bug min max initial-position step probability-left)
  (simulate
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

(define (black-red-green-roulette bet options house-advantage)
  (let ((result (random 1 (add1 options))))
    (if (and (> result house-advantage)
             (or
              (and (even? result)
                   (equal? bet 'black))
              (and (odd? result)
                   (equal? bet 'red))))
        'win
        'lose)))

(define (place-bet)
  (if (< (random) 0.5) 'black 'red))

(define (play-roulette options house-advantage actual-cash)
  (match-define (vector bet-number amount) actual-cash)
  ((if (equal?
        (black-red-green-roulette
         (place-bet)
         options
         house-advantage)
        'win)
       add1 sub1)
   amount))

(define (stop-playing? min max actual-cash _)
  (match-define (vector bet-number amount) actual-cash)
  (not (<= min amount max)))

(define (gambler-ruin min max options house-advantage initial-cash)
  (simulate
   initial-cash
   (curry play-roulette options house-advantage)
   (curry stop-playing? min max)))

(define (multiple-gambler-ruin min max options house-advantage initial-cash times #:key key)
  (build-list
      times
      (λ (n)
        (key
         (gambler-ruin min max options house-advantage initial-cash)))))

(define (probability-of-doubling-money options house-advantage initial-cash samples)
  (/
   (apply
    +
    (multiple-gambler-ruin
     0 (* 2 initial-cash) options house-advantage initial-cash samples
     #:key (λ (simulation)
             (if (equal? (vector-ref (last simulation) 1) -1)
                 0 1))))
   samples))