; Attempt to paint images with smaller versions of themselves


; Outside loop to define brush size
; Compute current lowest error location
; Get color
; Create brush with color
; Paint it!

#lang racket
(require images/flomap)
(require racket/draw)
(require racket/file)
(require racket/flonum)

(define (max-position xs)
  (define-values (mx mxpos)
    (for/fold ((mx #f) (mxpos #f)) (((c pos) (in-indexed xs)))
      (if (or (not mx) (> c mx))
          (values c pos)
          (values mx mxpos)))) mxpos)

(define (index->coords c w idx)  
  (values (modulo idx c) (modulo (exact-floor (/ idx c)) w) (exact-floor (/ idx (* w c)))))

(define (max-error-position reference-image working-image)
  (let-values ([(width height) (flomap-size reference-image)])
    (let* ([error-values (flomap-values (fm- reference-image working-image))]
           [position (max-position error-values)])
      (index->coords (flomap-components reference-image) width position))))

(define (flvector-comp-to-byte v c)
  (exact-floor (* (flvector-ref v c) 255.0)))
  
(define (flvector->color v)
  (make-object color% (flvector-comp-to-byte v 1) (flvector-comp-to-byte v 2) (flvector-comp-to-byte v 3)))

(define (random-float) (/ (random 1000) 1000))

(define (simple-stroke width height color)
  (flomap-trim
   (flomap-rotate
    (flomap-resize
     (flomap-trim (draw-flomap (lambda (fm-dc)
                    (send fm-dc set-pen "black" 1 'transparent)
                    (send fm-dc set-brush color 'solid)
                    (send fm-dc draw-ellipse 10 10 30 30)
                    (send fm-dc draw-polygon (list (make-object point% 10 25) (make-object point% 40 25) (make-object point% 25 75)))
                    )
                  100 100)) #f height) (* (random-float) pi 2))))

(define (gen-new-image reference-image working-image brush-size)
  ; Find location of lowest error
  (let-values ([(errorc errorx errory) (max-error-position reference-image working-image)]
               [(width height) (flomap-size reference-image)])
    (let* ([color (flvector->color (flomap-ref* reference-image errorx errory))]
           [brush-width (exact-floor (* width brush-size))]
           [brush-height (exact-floor (* height brush-size))]
           [brush (simple-stroke brush-width brush-height color)])
      (print (list errorx errory color))
      (print "\n")
      (flomap-cc-crop (flomap-pin working-image errorx errory brush (/ brush-width 2) (/ brush-height 2)) width height))))
      
(define (gen-new-image-2 reference-image working-image iterations brush-size)
  (let ([image working-image])
    (for ([i iterations])
      (set! image (gen-new-image reference-image image brush-size)))
    image))

(define (gen-images filename)
  (let ([reference-image (bitmap->flomap (read-bitmap filename))])
    (let-values ([(width height) (flomap-size reference-image)])
      (let* (
             [working-image (make-flomap 4 width height 0)]
             [image working-image]
             [ret `()]
             )
        (for ([i 2])
          (set! image (gen-new-image-2 reference-image image 50 0.1))
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
