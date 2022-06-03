#lang racket

(require plot images/flomap)

(provide plot discrete-histogram list->histogram flomap->bitmap flomap-normalize build-flomap)

(define (list->histogram list n)
  (letrec ((min-value (apply min list))
           (max-value (apply max list))
           (step (/ (- max-value min-value) n))
           (aux (λ (actual hash)
                  (if (empty? actual)
                      hash
                      (aux (cdr actual)
                           (hash-update hash (round (/ (car actual) step)) add1 0))))))
    (hash-map (aux list (hash))
              (λ (key value)
                (vector key value)))))