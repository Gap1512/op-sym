#lang racket

(require 2htdp/universe 2htdp/image
         (planet williams/science/ode-initval))

(define DEFAULT-PEN (pen "red" 3 "solid" "butt" "bevel"))

(define DEFAULT-WIDTH 200)

(define DEFAULT-HEIGHT 200)

(define DEFAULT-BACKGROUND
  (rectangle DEFAULT-WIDTH DEFAULT-HEIGHT "solid" "white"))

(define (draw-pendulum
         base-x base-y length-1 theta-1 length-2 theta-2
         (background DEFAULT-BACKGROUND)
         (pen DEFAULT-PEN))
  (let* ((end-x-1 (+ base-x (* (sin theta-1) length-1)))
         (end-y-1 (+ base-y (* (cos theta-1) length-1)))
         (end-x-2 (+ end-x-1 (* (sin theta-2) length-2)))
         (end-y-2 (+ end-y-1 (* (cos theta-2) length-2))))
    (add-line
     (add-line background base-x base-y end-x-1 end-y-1 pen)
     end-x-1 end-y-1 end-x-2 end-y-2 pen)))

(define (double-pendulum-func t y dy params)
  (match-define (vector theta-1 w-1 theta-2 w-2) y)
  (let* ((g (first params))
         (mass-1 (second params))
         (mass-2 (third params))
         (l1 (third params))
         (l2 (third params))
         (denominator-on-both-equations
          (+ (* 2 mass-1) mass-2 (- (* mass-2 (cos (- (* 2 theta-1) (* 2 theta-2)))))))
         (delta-theta (- theta-1 theta-2))
         (sin-delta-theta (sin delta-theta))
         (cos-delta-theta (cos delta-theta))
         (w1^2 (expt w-1 2))
         (w2^2 (expt w-2 2))
         (sum-m (+ mass-1 mass-2)))
    (vector-set! dy 0 w-1)
    (vector-set!
     dy 1
     (/
      (+
       (- (* g (+ (* 2 mass-1) mass-2) (sin theta-1)))
       (- (* g mass-2 (sin (- theta-1 (* 2 theta-2)))))
       (- (* 2 sin-delta-theta mass-2
             (+ (* w2^2 l2)
                (* w1^2 l1 cos-delta-theta)))))
      (* l1 denominator-on-both-equations)))
    (vector-set! dy 2 w-2)
    (vector-set!
     dy 3
     (/
      (* 2 sin-delta-theta
         (+ (* w1^2 l1 sum-m)
            (* g sum-m (cos theta-1))
            (* w2^2 l2 mass-2 cos-delta-theta)))
      (* l2 denominator-on-both-equations)))))

(struct double-pendulum-simulation (theta-1 w-1 theta-2 w-2))
 
(define (generate-double-pendulum-simulation
         gravity mass-1 mass-2 length-1 length-2
         initial-theta-1 initial-theta-2
         initial-w-1 initial-w-2
         initial-time final-time time-step)
  (let* ((system
          (make-ode-system
           double-pendulum-func #f 4 (list gravity mass-1 mass-2 length-1 length-2)))
         (current-time initial-time)
         (y (vector initial-theta-1 initial-w-1
                    initial-theta-2 initial-w-2))
         (dy-in (make-vector 4)))
    (ode-system-function-eval system current-time y dy-in)
    (do ((step (make-ode-step rk4-ode-type 4))
         (dy-out (make-vector 4))
         (y-err (make-vector 4))
         (theta-1-values '() (cons (vector current-time (vector-ref y 0)) theta-1-values))
         (w-1-values '() (cons (vector current-time (vector-ref y 1)) w-1-values))
         (theta-2-values '() (cons (vector current-time (vector-ref y 2)) theta-2-values))
         (w-2-values '() (cons (vector current-time (vector-ref y 3)) w-2-values)))
      ((>= current-time final-time)
       (double-pendulum-simulation theta-1-values w-1-values theta-2-values w-2-values))
      (ode-step-apply step current-time time-step y y-err dy-in dy-out system)
      (vector-set! dy-in 0 (vector-ref dy-out 0))
      (vector-set! dy-in 1 (vector-ref dy-out 1))
      (vector-set! dy-in 2 (vector-ref dy-out 2))
      (vector-set! dy-in 3 (vector-ref dy-out 3))
      (set! current-time (+ current-time time-step)))))

(define (animate-double-pendulum-simulation
         base-x base-y gravity
         mass-1 mass-2
         length-1 length-2
         initial-theta-1 initial-theta-2
         initial-w-1 initial-w-2
         initial-time final-time time-step
         (background DEFAULT-BACKGROUND)
         (pen DEFAULT-PEN))
  (match-define (double-pendulum-simulation
                 theta-1-values w-1-values theta-2-values w-2-values)
    (generate-double-pendulum-simulation
     gravity mass-1 mass-2 length-1 length-2
     initial-theta-1 initial-theta-2
     initial-w-1 initial-w-2
     initial-time final-time time-step))
  (run-movie
   time-step
   (map
    (λ (theta-1 theta-2)
      (draw-pendulum
       base-x base-y
       length-1 (vector-ref theta-1 1)
       length-2 (vector-ref theta-2 1)
       background pen))
    theta-1-values
    theta-2-values))
  #t)
