#lang racket

(require metapict)
(require "glob.rkt")

(define (reference-glob)
  (define g (build-glob (make-color* 255.0 128.5 0.25) (pt 0 0) 2 (pt 20 0) 8
                        (pt 12 -12) (pt 4 -13) 0.5 0.65 0.75 0.45))
  (draw-glob g))


(define (random-pt)
  (pt (random 100) (random 100)))

(define (random-pt-from pt min-distance)
  (for/fold ([test-pt pt])
            ([i (in-naturals)])
            #:break (> (dist test-pt pt) min-distance)
    (random-pt)))

(define (random-float) (/ (random 1000) 1000))
  
(define (random-glob)
  (define pt1 (random-pt))
  (define pt2 (random-pt-from pt1 20))
  (define axis-len (exact-floor (dist pt1 pt2)))
  (define (radius-random)
    (random (quotient axis-len 4) (quotient axis-len 3)))
  (define r1 (radius-random))
  (define r2 (radius-random))
  (define axis-middle (med 0.5 pt1 pt2))
  (define (make-d) (pt+ axis-middle (pt@ (quotient axis-len 10) (* 2 pi (random-float)))))
  (build-glob (make-color* 255.0 128.5 0.25) pt1 r1 pt2 r2
              (make-d) (make-d) (random-float) (random-float) (random-float) (random-float)))

(draw-glob (random-glob))
