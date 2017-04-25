; Paints images with a brush and a reference image.

#lang racket
(require images/flomap)
(require racket/draw)
(require racket/file)
(require racket/flonum)
(require "flomap-utils.rkt")
(require optimization-coach)

(define (flvector-comp-to-byte v c)
  (exact-floor (* (flvector-ref v c) 255.0)))
  
(define (flvector->color v)
  (make-object color% (flvector-comp-to-byte v 1) (flvector-comp-to-byte v 2) (flvector-comp-to-byte v 3)))

(define (random-float) (/ (random 1000) 1000))

(define (make-even val)
  (if (= (modulo val 2) 1)
      (add1 val)
      val))

(define (even-flomap fm color)
  (let-values ([(width height) (flomap-size fm)])
    (if (or (zero? width) (zero? height))
        (make-flomap* 1 1 color)
        (flomap-resize fm (make-even width) (make-even height)))))
    
(define (simple-stroke width height color)
  (even-flomap (flomap-trim
                (flomap-rotate    
                 (flomap-resize
                  (flomap-trim (draw-flomap (lambda (fm-dc)
                                              (send fm-dc set-pen "black" 1 'transparent)
                                              (send fm-dc set-brush (flvector->color color) 'solid)
                                              (send fm-dc draw-ellipse 10 10 30 30)
                                              (send fm-dc draw-polygon (list (make-object point% 10 25) (make-object point% 40 25) (make-object point% 25 75)))
                                              )
                                            100 100)) #f height) (* (random-float) pi 2))) color))

(define (dumb-crop fm width height brush x y)
  (let-values ([(brushw brushh) (flomap-size brush)]
               [(tw th) (flomap-size fm)])
    (let* ([hw (exact-floor (/ brushw 2))]
           [hh (exact-floor (/ brushh 2))]
           [start-x (max (- hw x) 0)]
           [start-y (max (- hh y) 0)]
           [end-x (if (> (+ x hw) width)
                      (- width start-x)
                      tw)]
           [end-y (if (> (+ y hh) height)
                      (- height start-y)
                      th)])
      (subflomap fm start-x start-y end-x end-y))))

(define (subregion-error reference-image working-image brush-size x y)
  (let ([startx (exact-floor (- x brush-size))]
        [starty (exact-floor (- y brush-size))]
        [endx (exact-floor (+ x brush-size))]
        [endy (exact-floor (+ y brush-size))])
    (error (subflomap reference-image startx starty endx endy)
           (subflomap working-image startx starty endx endy))))

(define (gen-new-image reference-image working-image brush-size)
  ; Find location of lowest error
  (let-values ([(errorc errorx errory) (max-error-position reference-image working-image)]
               [(width height) (flomap-size reference-image)])
    ; (printf "pos ~a ~a\n" errorx errory)
    (let* ([subregion-size (* (max width height) brush-size)]
          [original-error (subregion-error reference-image working-image subregion-size errorx errory)]
          [color (flomap-ref* reference-image errorx errory)])
      (let-values ([(final-image final-brush-size)
                    (for/fold ([new-image #f] [adjusted-brush-size brush-size])
                              ([i (in-naturals)])
                      #:break (and new-image (< (subregion-error reference-image new-image subregion-size errorx errory) original-error))
                      (let* ([brush-width (exact-floor (* width adjusted-brush-size))]
                             [brush-height (exact-floor (* height adjusted-brush-size))]
                             [brush (simple-stroke brush-width brush-height color)])
                        (let-values ([(bw bh) (flomap-size brush)])
                          (values 
                           (dumb-crop (flomap-pin working-image errorx errory brush (exact-floor (/ bw 2)) (exact-floor (/ bh 2))) width height brush errorx errory)
                           (/ adjusted-brush-size 2)))))]) final-image))))
     
(define (create-work-image filename reference-image)
  (let ([start-file (path-replace-extension filename ".start")])
    (if (file-exists? start-file)
        (bitmap->flomap (read-bitmap start-file))
        (let-values ([(width height) (flomap-size reference-image)])
          (make-flomap 4 width height 0)))))

(define (gen-images filename)
  (let ([reference-image (bitmap->flomap (read-bitmap filename))])
    (let-values ([(width height) (flomap-size reference-image)])
      (let* (
             [working-image (create-work-image filename reference-image)]
             [image working-image]
             [ret `()]
             )
        (for ([i 100])
          (for ([j 100])
            (set! image (gen-new-image reference-image image 0.1)))
          (send (flomap->bitmap image) save-file (string-append (path->string (path-replace-extension filename "")) (number->string i) ".png") 'png)
          (set! ret (cons (flomap->bitmap image) ret))
          )
        ret))))

(define (do-it)
  (let* ([path "input/"]
         [files (find-files (lambda (file)
                              (or (file-exists? file) (string=? path (path->string file))))
                            (string->path path) #:skip-filtered-directory? #t)]
         [images (filter (lambda (file)
                           (or (path-has-extension? file ".png")
                               (path-has-extension? file ".jpg"))) files)])
    (for-each (lambda (file)
                (gen-images file)) images)))