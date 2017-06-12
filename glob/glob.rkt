#lang racket

; An implementation of "Globs" as described by Andrew Glassner
; http://www.imaginary-institute.com/resources/TechNote11/TechNote11.html

(provide build-glob draw-glob)

(require (prefix-in pict3d: pict3d))
(require metapict)
(require metapict/structs)

(struct glob (
              ; Input values
              color c0 r0 c1 r1 D dp a b ap bp
              ; Computed values
              U V E0 E0p E1 E1p F0 F0p F1 F1p minx maxx miny maxy))

(define (build-glob color c0 r0 c1 r1 D dp a b ap bp)
  (define U (vec-normalized (pt- c1 c0)))
  (define V (vec (vec-y U) (- (vec-x U))))
  (define e0 (get-tangent-point c0 D r0 'right))
  (define e1 (get-tangent-point c1 D r1 'left))
  (define e0p (get-tangent-point c0 dp r0 'left))
  (define e1p (get-tangent-point c1 dp r1 'right))
  (define f0 (vec-lerp e0 D a))
  (define f1 (vec-lerp e1 D b))
  (define f0p (vec-lerp e0p dp ap))
  (define f1p (vec-lerp e1p dp bp))
  (define points (list e0 f0 f1 e1 e1p f1p f0p e0p))
  (define minx (min (vec-x (argmin vec-x points)) (- (pt-x c0) r0) (- (pt-x c1) r1)))
  (define maxx (max (vec-x (argmax vec-x points)) (+ (pt-x c0) r0) (+ (pt-x c1) r1)))
  (define miny (min (vec-y (argmin vec-y points)) (- (pt-y c0) r0) (- (pt-y c1) r1)))
  (define maxy (max (vec-y (argmax vec-y points)) (+ (pt-y c0) r0) (+ (pt-y c1) r1)))  
  (glob color c0 r0 c1 r1 D dp a b ap bp U V e0 e0p e1 e1p f0 f0p f1 f1p minx maxx miny maxy)
  )

(define (draw-glob g)
  (with-window (window (glob-minx g) (glob-maxx g) (glob-miny g) (glob-maxy g))
    (define neck-bezs (list
                       (vbez (glob-E0 g) (glob-F0 g) (glob-F1 g) (glob-E1 g))
                       (vbez (glob-E1 g) (glob-E1 g) (glob-E1 g) (glob-E1p g))
                       (vbez (glob-E1p g) (glob-F1p g) (glob-F0p g) (glob-E0p g))
                       (vbez (glob-E0p g) (glob-E0p g) (glob-E0p g) (glob-E0 g))                       
                       )
      )
    (define neck (curve: 't neck-bezs))
    (cc-superimpose
     (color (glob-color g)
            (fill neck))
     (color (glob-color g)
            (fill (circle (glob-c0 g) (glob-r0 g))
                  (circle (glob-c1 g) (glob-r1 g))
                  )))
  ))
                 
(define (get-tangent-point center outsidePt radius side)
  (define S (vec-normalized (pt- outsidePt center)))
  (define T (vec (vec-y S) (- (vec-x S))))
  (define dist-outside (dist center outsidePt))
  (define pb (sqrt (- (sqr dist-outside) (sqr radius))))
  (define beta (atan pb radius))
  (define uscl (* radius (cos beta)))
  (define vscl (* radius (sin beta)))
  (define p0 (vec (+ (pt-x center) (* uscl (vec-x S)) (* vscl (vec-x T)))
                  (+ (pt-y center) (* uscl (vec-y S)) (* vscl (vec-y T)))))
  (define p1 (vec (- (+ (pt-x center) (* uscl (vec-x S))) (* vscl (vec-x T)))
                  (- (+ (pt-y center) (* uscl (vec-y S))) (* vscl (vec-y T)))))
  (define dp0 (vec- p0 (pt->vec center)))
  (define dp1 (vec- p1 (pt->vec center)))
  
  (define Sdir (vec->dir S))
  (define (get-sign v) (pict3d:dir-dz (pict3d:dir-cross Sdir (vec->dir v))))  
  (define p0sgn (get-sign dp0))
  (define p1sgn (get-sign dp1))
  (cond
    ; Both positive or negative, same side of line!
    [(> (* p0sgn p1sgn) 0) p0]
    ; Side right
    [(eq? side 'right) (if (> p0sgn 0)
                           p0
                           p1)]
    [else (if (< p0sgn 0)
              p0
              p1)]))

(define (vec-normalized v)
  (vec* (/ 1 (norm v)) v))

(define (pt->vec p)
  (if (vec? p)
      p
      (vec (pt-x p) (pt-y p))))

(define (vec->dir v)
  (pict3d:dir (vec-x v) (vec-y v) 0))

(define (vec-lerp x y amt)
  (vec+ x (vec* amt (vec- (pt->vec y) (pt->vec x)))))

(define (vbez a b c d)
  (bez (vec->pt a) (vec->pt b) (vec->pt c) (vec->pt d)))

(module+ test
  (require rackunit)
  (require racket/struct)
  (check-equal? (get-tangent-point (pt 0 0) (pt 5 1) 1 'left) (vec 0.3846153846153846 -0.9230769230769231))
  (check-equal? (get-tangent-point (pt 0 0) (pt 5 1) 1 'right) (vec -2.7755575615628914e-17 1.0))
  (define tglob (build-glob (make-color* 255.0 128.5 0.25) (pt 0 0) 2 (pt 10 0) 4
                            (pt 2 2) (pt 3 3) 0.5 0.65 0.75 0.45))
  (check-equal? (color->list (glob-color tglob)) (color->list (make-color* 255.0 128.5 0.25 1.0)))
  (check-equal? (glob-c0 tglob) (pt 0 0))
  (check-equal? (glob-r0 tglob) 2)
  (check-equal? (glob-c1 tglob) (pt 10 0))
  (check-equal? (glob-r1 tglob) 4)
  (check-equal? (glob-D tglob) (pt 2 2))
  (check-equal? (glob-dp tglob) (pt 3 3))
  (check-equal? (glob-a tglob) 0.5)
  (check-equal? (glob-b tglob) 0.65)
  (check-equal? (glob-ap tglob) 0.75)
  (check-equal? (glob-bp tglob) 0.45)
  (check-equal? (glob-U tglob) (vec 1 0))
  (check-equal? (glob-V tglob) (vec 0 -1))
  (check-equal? (glob-E0 tglob) (vec -2.220446049250313e-16 1.9999999999999998))
  (check-equal? (glob-E0p tglob) (vec 1.9138857955913138 -0.5805524622579802))
  (check-equal? (glob-E1 tglob) (vec 8.966012064815056 3.864048259260225))
  (check-equal? (glob-E1p tglob) (vec 6.728122614122512 -2.301047233714139))
  (check-equal? (glob-F0 tglob) (vec 0.9999999999999998 2.0))
  (check-equal? (glob-F0p tglob) (vec 2.7284714488978286 2.104861884435505))
  (check-equal? (glob-F1 tglob) (vec 4.438104222685269 2.652416890741079))
  (check-equal? (glob-F1p tglob) (vec 5.050467437767382 0.08442402145722339))  
  )


