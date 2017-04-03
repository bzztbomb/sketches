#lang racket

; Generate "brush strokes"

(require images/flomap)
(require racket/draw)

;(define stroke (make-flomap* 100 100 #(1.0 1.0 1.0 1.0)))

(define (random-color) (make-object color% (random 255) (random 255) (random 255) 1.0))

(define (random-float) (/ (random 1000) 1000))

(define (simple-stroke width height)
  (flomap-trim
   (flomap-rotate
    (flomap-resize
     (draw-flomap (lambda (fm-dc)
                    (send fm-dc set-pen "black" 1 'transparent)
                    (send fm-dc set-brush (random-color) 'solid)
                    (send fm-dc draw-ellipse 10 10 30 30)
                    (send fm-dc draw-polygon (list (make-object point% 10 25) (make-object point% 40 25) (make-object point% 25 75)))                              
                    )
                  width height) #f height) (* (random-float) pi 2))))

(define (b) (flomap->bitmap (simple-stroke 100 100)))
