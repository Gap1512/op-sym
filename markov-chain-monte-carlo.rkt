#lang racket

(require csv-reading "plot.rkt" "random-walk.rkt")

(define TEMPERATURE-STEP 1)

(define make-sensor-data-csv-reader
  (make-csv-reader-maker
   '((separator-chars            #\;)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))

(define (call-with-next-row path fn (ignore-n-lines 0))
  (call-with-input-file path
    (λ (input-port)
      (let ((next-row-fn (make-sensor-data-csv-reader input-port)))
        (for ((_ ignore-n-lines)) (next-row-fn))
        (fn next-row-fn)))))

(define (update-markov-chain markov-chain actual-state next-state fn)
  (hash-update
   markov-chain actual-state
   (lambda (actual-state-hash)
     (hash-update actual-state-hash next-state fn 0))
   (hash next-state 0)))

(define (absolute->percentual-markov-chain markov-chain)
  (make-immutable-hash
   (hash-map
    markov-chain
    (λ (actual-state state-hash)
      (let ((total (apply + (hash-values state-hash))))
        (cons
         actual-state
         (make-immutable-hash
          (hash-map
           state-hash
           (λ (next-state count)
             (cons next-state (exact->inexact (/ count total))))))))))))

(define (reduce-csv-data tolerance process-line-fn ignore-plot? next-fn)
  (letrec ((reducer (λ (last-measure last-state markov-chain iteration plot-data)
                      (let ((next (next-fn)))
                        (if (empty? (cdr next))
                            (let ((result (absolute->percentual-markov-chain markov-chain)))
                              (if ignore-plot? result (list plot-data result)))
                            (let* ((new-measure (or (process-line-fn next) last-measure))
                                   (difference (- new-measure last-measure))
                                   (new-state (cond
                                                ((<= (abs difference) tolerance) 'maintain)
                                                ((negative? difference) 'decrease)
                                                (else 'increase)))
                                   (new-markov-chain
                                    (update-markov-chain
                                     markov-chain
                                     last-state
                                     new-state
                                     add1)))
                              (reducer
                               new-measure new-state new-markov-chain
                               (add1 iteration)
                               (cons (vector iteration new-measure) plot-data)))))))
           (base-markov-chain (hash 'maintain 0
                                    'decrease 0
                                    'increase 0))
           (full-markov-chain (hash 'maintain base-markov-chain
                                    'decrease base-markov-chain
                                    'increase base-markov-chain)))
    (reducer 0 'maintain full-markov-chain 0 '())))

(define (random-next-state-from-markov-chain markov-chain actual-state)
  (letrec ((cumulative-subtraction
            (λ (value probabilities)
              (let ((new-value (- value (cdar probabilities))))
                (if (or (<= new-value 0)
                        (empty? (cdr probabilities)))
                    (caar probabilities)
                    (cumulative-subtraction new-value
                                            (cdr probabilities)))))))
    (cumulative-subtraction
     (random)
     (hash->list (hash-ref markov-chain (vector-ref actual-state 1))))))
                  
    

(define (average-temperature path (ignore-plot? #t))
  (call-with-next-row
   path
   (curry reduce-csv-data TEMPERATURE-STEP
          (λ (str) (string->number (third str))) ignore-plot?)
   11))

(define (plot-average-temperature path)
  (plot
   (lines
    (first
     (average-temperature path #f)))))

(define (simulate-relative-temperature initial-state max-iteration path)
  (simulate
   initial-state
   (curry random-next-state-from-markov-chain (average-temperature path))
   (λ (_ iteration) (> iteration max-iteration))))

(define (simulate-absolute-temperature initial-state initial-temperature max-iteration path)
  (letrec ((cumulative-sum
            (λ (value values res)
              (if (empty? values)
                  res
                  (let ((new-value
                         ((case (vector-ref (car values) 1)
                            ((maintain) identity)
                            ((increase) (λ (x) (+ x TEMPERATURE-STEP)))
                            ((decrease) (λ (x) (- x TEMPERATURE-STEP))))
                          value)))
                    (cumulative-sum
                     new-value
                     (cdr values)
                     (cons (vector (vector-ref (car values) 0) new-value) res)))))))
    (cumulative-sum
     initial-temperature
     (simulate-relative-temperature initial-state max-iteration path)
     '())))

(define (plot-simulate-absolute-temperature initial-state initial-temperature max-iteration path)
  (plot
   (lines
    (simulate-absolute-temperature initial-state initial-temperature max-iteration path))))