#lang racket

(require 2htdp/universe 2htdp/image
         (planet williams/science/ode-initval)
         (only-in rnrs/base-6 mod))

(define DEFAULT-PEN (pen "red" 3 "solid" "butt" "bevel"))

(define DEFAULT-RADIUS 4)

(define DEFAULT-WIDTH 300)

(define DEFAULT-HEIGHT 300)

(define DEFAULT-BACKGROUND
  (rectangle DEFAULT-WIDTH DEFAULT-HEIGHT "solid" "white"))

(define (draw-pendulum
         base-x base-y
         length-1 theta-1 mass-1
         length-2 theta-2 mass-2
         (background DEFAULT-BACKGROUND)
         (pen DEFAULT-PEN)
         (base-radius DEFAULT-RADIUS))
  (let* ((end-x-1 (+ base-x (* (sin theta-1) length-1)))
         (end-y-1 (+ base-y (* (cos theta-1) length-1)))
         (end-x-2 (+ end-x-1 (* (sin theta-2) length-2)))
         (end-y-2 (+ end-y-1 (* (cos theta-2) length-2)))
         (radius-1 (* mass-1 base-radius))
         (radius-2 (* mass-2 base-radius)))
    (underlay/xy
     (underlay/xy
      (add-line
       (add-line background base-x base-y end-x-1 end-y-1 pen)
       end-x-1 end-y-1 end-x-2 end-y-2 pen)
      (- end-x-1 radius-1) (- end-y-1 radius-1)
      (circle radius-1 "solid" "black"))
     (- end-x-2 radius-2) (- end-y-2 radius-2)
     (circle radius-2 "solid" "black"))))

(define (double-pendulum-func t y dy params)
  (match-define (vector theta-1 w-1 theta-2 w-2) y)
  (let* ((g (first params))
         (mass-1 (second params))
         (mass-2 (third params))
         (length-1 (third params))
         (length-2 (third params))
         (denominator-on-both-equations
          (+ (* 2 mass-1) mass-2 (- (* mass-2 (cos (- (* 2 theta-1) (* 2 theta-2)))))))
         (delta-theta (- theta-1 theta-2))
         (sin-delta-theta (sin delta-theta))
         (cos-delta-theta (cos delta-theta))
         (w-1^2 (expt w-1 2))
         (w-2^2 (expt w-2 2))
         (sum-m (+ mass-1 mass-2)))
    (vector-set! dy 0 w-1)
    (vector-set!
     dy 1
     (/
      (+
       (- (* g (+ (* 2 mass-1) mass-2) (sin theta-1)))
       (- (* g mass-2 (sin (- theta-1 (* 2 theta-2)))))
       (- (* 2 sin-delta-theta mass-2
             (+ (* w-2^2 length-2)
                (* w-1^2 length-1 cos-delta-theta)))))
      (* length-1 denominator-on-both-equations)))
    (vector-set! dy 2 w-2)
    (vector-set!
     dy 3
     (/
      (* 2 sin-delta-theta
         (+ (* w-1^2 length-1 sum-m)
            (* g sum-m (cos theta-1))
            (* w-2^2 length-2 mass-2 cos-delta-theta)))
      (* length-2 denominator-on-both-equations)))))

(struct double-pendulum-simulation (theta-1 w-1 theta-2 w-2))
 
(define (generate-double-pendulum-simulation
         gravity mass-1 mass-2 length-1 length-2
         initial-theta-1 initial-theta-2
         initial-w-1 initial-w-2
         initial-time duration frame-time)
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
      ((>= current-time duration)
       (double-pendulum-simulation
        (reverse theta-1-values) (reverse w-1-values)
        (reverse theta-2-values) (reverse w-2-values)))
      (ode-step-apply step current-time frame-time y y-err dy-in dy-out system)
      (vector-set! dy-in 0 (vector-ref dy-out 0))
      (vector-set! dy-in 1 (vector-ref dy-out 1))
      (vector-set! dy-in 2 (vector-ref dy-out 2))
      (vector-set! dy-in 3 (vector-ref dy-out 3))
      (set! current-time (+ current-time frame-time)))))

(struct interaction-state
  (status current-theta-1 current-theta-2 theta-1-values theta-2-values))

(define (render-state base-x base-y length-1 length-2 mass-1 mass-2
                      background pen base-radius state)
  (draw-pendulum
   base-x base-y
   length-1 (vector-ref (interaction-state-current-theta-1 state) 1) mass-1
   length-2 (vector-ref (interaction-state-current-theta-2 state) 1) mass-2
   background pen base-radius))

(define (next-state state)
  (match-define
    (interaction-state
     status current-theta-1 current-theta-2 theta-1-values theta-2-values)
    state)
  (cond
    ((eq? status 'running)
     (interaction-state
      status (car theta-1-values) (car theta-2-values)
      (cdr theta-1-values) (cdr theta-2-values)))
    (else state)))

(define (done? state)
  (and
   (eq? (interaction-state-status state) 'running)
   (or (empty? (interaction-state-theta-1-values state))
       (empty? (interaction-state-theta-2-values state)))))

(define (change-status gravity mass-1 mass-2 length-1 length-2
                       duration frame-time theta-step
                       state pressed-key)
  (match-define
    (interaction-state
     status current-theta-1 current-theta-2 theta-1-values theta-2-values)
    state)
  (cond
    ((key=? pressed-key " ")
     (begin
       (match-define (double-pendulum-simulation
                      theta-1-values-from-simulation w-1-values-from-simulation
                      theta-2-values-from-simulation w-2-values-from-simulation)
         (generate-double-pendulum-simulation
          gravity mass-1 mass-2 length-1 length-2
          (vector-ref current-theta-1 1)
          (vector-ref current-theta-2 1)
          0.0 0.0 0.0 duration frame-time))
       (interaction-state 'running current-theta-1 current-theta-2
                          theta-1-values-from-simulation
                          theta-2-values-from-simulation)))
    ((key=? pressed-key "1")
     (interaction-state status
                        (add-step-to-theta current-theta-1 theta-step)
                        current-theta-2
                        theta-1-values theta-2-values))
    ((key=? pressed-key "2")
     (interaction-state status
                        current-theta-1
                        (add-step-to-theta current-theta-2 theta-step)
                        theta-1-values theta-2-values))
    (else state)))

(define (add-step-to-theta current-theta step)
  (vector 0.0 (mod (+ step (vector-ref current-theta 1)) (* 2 pi))))

(define (interactive-double-pendulum-simulation
         base-x base-y gravity
         mass-1 mass-2
         length-1 length-2
         duration frame-time theta-step
         (background DEFAULT-BACKGROUND)
         (pen DEFAULT-PEN)
         (base-radius DEFAULT-RADIUS))
  (big-bang (interaction-state 'configuring #(0.0 0.0) #(0.0 0.0) '() '())
    (to-draw (curry render-state
                    base-x base-y length-1 length-2 mass-1 mass-2
                    background pen base-radius))
    (on-tick next-state frame-time)
    (on-key (curry change-status gravity mass-1 mass-2
                   length-1 length-2 duration frame-time theta-step))
    (stop-when done?)))