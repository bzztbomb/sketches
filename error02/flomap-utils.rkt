#lang typed/racket

(require images/flomap)
(require racket/flonum)

(define (subsq [val1 : Flonum] [val2 : Flonum])  
    (fl* (fl- val1 val2) (fl- val1 val2)))
  
(define fmsubsq (inline-flomap-lift2 'fmsubsq subsq))

(define (ten-percent)
  (> 2 (random (* 1024 300))))

(define (error [fm1 : flomap] [fm2 : flomap])
  (for/fold ([len : Flonum 0.0])
            ([value (in-flvector (flomap-values (fmsubsq fm1 fm2)))])
    (+ len value)))

(define (max-position [xs : FlVector])
  (define-values (mx mxpos)
    (for/fold ([mx : Flonum -100000.0]
               [mxpos : Integer 0])
              ((([c : Flonum] [pos : Integer])
                (in-indexed (in-flvector xs))))
      (cond
        [(or (not mx) (> c mx)) (values c pos)]
        [(= c mx) (if (ten-percent)
                      (values c pos)
                      (values mx mxpos))]
        [else (values mx mxpos)]))) mxpos)

(define (index->coords [c : Integer] [w : Integer] [idx : Integer]) 
  (values (modulo idx c) (modulo (exact-floor (/ idx c)) w) (exact-floor (/ idx (* w c)))))

(define (max-error-position [reference-image : flomap] [working-image : flomap])
  (let-values ([(width height) (flomap-size reference-image)])
    (let* ([error-values (flomap-values (fmsubsq reference-image working-image))]
           [position (max-position error-values)])
      (index->coords (flomap-components reference-image) width position))))

(provide error)
(provide max-error-position)


