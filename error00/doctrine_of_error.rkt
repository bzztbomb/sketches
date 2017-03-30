; Attempt to paint images with smaller versions of themselves

#lang racket
(require images/flomap)
(require racket/draw)
(require racket/file)

(define (error2 fm1 fm2)
  (for/fold ([len 0])
            ([value (flomap-values (fmsqr (fm- fm1 fm2)))])
    (+ len value)))

(define (error-for-location reference-image working-image pixel-image x y)
  (error2 (flomap-pin working-image x y pixel-image) reference-image))

(define (gen-new-image reference-image working-image pixel-image)
  (let-values ([(width height) (flomap-size reference-image)])
  (let ([lowest-error (error2 working-image reference-image)]
        [x-pos 0]
        [y-pos 0]
        [curr-image working-image])
    (for ([i 100])
      (let* ([x (random width)]
             [y (random height)]
             [new-image (flomap-cc-crop (flomap-pin working-image x y pixel-image) width height)]
             [new-error (error2 new-image reference-image)])
        (if (< new-error lowest-error)
            (begin
              (set! x-pos x)
              (set! y-pos y)
              (set! curr-image new-image)
              (set! lowest-error new-error))
            (void))
        ))
  curr-image)))

(define (gen-new-image-2 reference-image working-image pixel-image iterations)
  (let ([image working-image])
    (for ([i iterations])
      (set! image (gen-new-image reference-image image pixel-image)))
    image))


(define (gen-images filename)
  (let ([reference-image (bitmap->flomap (read-bitmap filename))])
    (let-values ([(width height) (flomap-size reference-image)])
      (let* ([working-image (make-flomap 4 width height 0)]
             [pixel-image (flomap-scale reference-image 0.1)]
             [ret `()]
             [image working-image])
        (for ([i 5])
          (set! image (gen-new-image-2 reference-image image pixel-image 50))
          (set! ret (cons (flomap->bitmap image) ret))
          )
        ret))))

(define (save-images images prefix)
  (let ([i 0])
    (for-each (lambda (image)
              (send image save-file (string-append prefix (number->string i) ".png") 'png)
                (set! i (+ i 1)))
              images)
    ))

(define (do-it)
  (let* ([path "/Users/brianr/projects/sketches/error00/input"]
        [files (find-files (lambda (file)
                             (or (file-exists? file) (string=? path (path->string file))))
                             (string->path path) #:skip-filtered-directory? #t)]
        [images (filter (lambda (file)
                          (or (path-has-extension? file ".png")
                              (path-has-extension? file ".jpg"))) files)])
    (for-each (lambda (file)
                (save-images (gen-images file) (path->string (path-replace-extension file "")))) images)))
