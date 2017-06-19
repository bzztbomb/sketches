#lang racket

(require metapict)
(require "glob.rkt")

(define (random-pt-from pt min-distance)
  (for/fold ([test-pt pt])
            ([i (in-naturals)])
            #:break (> (dist test-pt pt) min-distance)
    (random-pt)))

(define (random-float) (/ (random 1000) 1000))
  
(define (random-glob w h)
  (define base-radius (* (min w h) 0.45))
  (define base-degree (* 2 pi (random-float)))
  (define pt1 (pt@ base-radius base-degree))
  (define pt2 (pt@ base-radius (+ base-degree pi)))
  (define axis-len (exact-floor (dist pt1 pt2)))
  (define (radius-random)
    (random (quotient axis-len 4) (quotient axis-len 3)))
  (define r1 (radius-random))
  (define r2 (radius-random))
  (define axis-middle (med 0.5 pt1 pt2))
  (define (make-d) (pt+ axis-middle (pt@ (quotient axis-len 15) (* 2 pi (random-float)))))
  (build-glob (make-color* 255.0 128.5 0.25) pt1 r1 pt2 r2
              (make-d) (make-d) (random-float) (random-float) (random-float) (random-float)))

(draw-glob (random-glob 100 200))
