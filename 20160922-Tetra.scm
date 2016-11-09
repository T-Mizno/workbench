;;
;; Demo : to use gl.simple.viewer
;;  2011/01/15
;;

(use gl)
(use gl.glut)
(use gl.simple.viewer)

(require "./myzLinear.scm")
(use myzLinear)

;(use util.list)
(use srfi-42) ; do-ec

(define *WinWidth* 500)
(define *WinHeight* 500)

(define (setWindowSize aw ah)
  (set! *WinWidth* aw)
  (set! *WinHeight* ah))

;unit x
(define *u_x_max* 5.5)
(define *u_x_min* -5.5)

;unit y
(define *u_y_max* 5)
(define *u_y_min* -5)

;unit z
(define *u_z_max* 5)
(define *u_z_min* -5)

;real x
(define *r_x_max* 10.5)
(define *r_x_min* -10.5)

;real y
(define *r_y_max* 10.5)
(define *r_y_min* -10.5)

;real z
(define *r_z_max* 10.5)
(define *r_z_min* -10.5)

(define (r2u r r_max r_min u_max u_min)
  (+ (* (/ (- r r_min) (- r_max r_min))  (- u_max u_min) ) u_min)
)

(define (r2u-3v r u)
  (set u 0 0 (r2u (at r 0 0) *r_x_max* *r_x_min* *u_x_max* *u_x_min*))
  (set u 1 0 (r2u (at r 1 0) *r_y_max* *r_y_min* *u_y_max* *u_y_min*))
  (set u 2 0 (r2u (at r 2 0) *r_z_max* *r_z_min* *u_z_max* *u_z_min*))
)

(define (my-gl-vertex x y z)
  (gl-vertex
   (r2u x *r_x_max* *r_x_min* *u_x_max* *u_x_min*)
   (r2u y *r_y_max* *r_y_min* *u_y_max* *u_y_min*)
   (r2u z *r_z_max* *r_z_min* *u_z_max* *u_z_min*)
   )
)

(define (my-gl-vertex-3v v)
    (my-gl-vertex (at v 0 0) (at v 1 0) (at v 2 0))
  )

(define (iter f ls)
  (cond
    ((null? ls)) ; do nothing
    ((pair? ls)
     (f (car ls))
     (iter f (cdr ls)))
    (else (error "Cannot iterate over a non-list"))))

(define (myDrawString x y z s)
  (gl-raster-pos
   (r2u x *r_x_max* *r_x_min* *u_x_max* *u_x_min*)
   (r2u y *r_y_max* *r_y_min* *u_y_max* *u_y_min*)
   (r2u z *r_z_max* *r_z_min* *u_z_max* *u_z_min*))
  (iter (lambda (c) (glut-bitmap-character GLUT_BITMAP_8_BY_13 (char->integer c))) (string->list s))
  )

(define (myDrawString-4w-offset w x y z s)
  (let ((r (multi *X* w)))
    (gl-raster-pos
     (r2u (+ (at r 0 0) x) *r_x_max* *r_x_min* *u_x_max* *u_x_min*)
     (r2u (+ (at r 1 0) y) *r_y_max* *r_y_min* *u_y_max* *u_y_min*)
     (r2u (+ (at r 2 0) z) *r_z_max* *r_z_min* *u_z_max* *u_z_min*))
    (iter (lambda (c) (glut-bitmap-character GLUT_BITMAP_8_BY_13 (char->integer c))) (string->list s))
    )
)

(define *AXLENGTH* 7)
(define *X1* (newMatrixFromList- 0 2 0 0 (list (* -1 *AXLENGTH*) (* -1 *AXLENGTH*) *AXLENGTH*)))
(define *X2* (newMatrixFromList- 0 2 0 0 (list  *AXLENGTH* *AXLENGTH* *AXLENGTH*)))
(define *X3* (newMatrixFromList- 0 2 0 0 (list (* -1 *AXLENGTH*) *AXLENGTH* (* -1 *AXLENGTH*))))
(define *X4* (newMatrixFromList- 0 2 0 0 (list *AXLENGTH*  (* -1 *AXLENGTH*) (* -1 *AXLENGTH*))))
(define *X*  (newMatrixFromList- 0 3 0 3
                                 (list (at *X1* 0 0) (at *X2* 0 0) (at *X3* 0 0) (at *X4* 0 0)
                                       (at *X1* 1 0) (at *X2* 1 0) (at *X3* 1 0) (at *X4* 1 0)
                                       (at *X1* 2 0) (at *X2* 2 0) (at *X3* 2 0) (at *X4* 2 0)
                                       1  1  1  1)))
(define *InvX* 
  (let* ( (b1 (newMatrixFromList- 0 3 0 0 '(1 0 0 0)))
          (b2 (newMatrixFromList- 0 3 0 0 '(0 1 0 0)))
          (b3 (newMatrixFromList- 0 3 0 0 '(0 0 1 0)))
          (b4 (newMatrixFromList- 0 3 0 0 '(0 0 0 1)))
          (w1 (ref (gauss *X* b1) 5))
          (w2 (ref (gauss *X* b2) 5))
          (w3 (ref (gauss *X* b3) 5))
          (w4 (ref (gauss *X* b4) 5)))
    (newMatrixFromList- 0 3 0 3
                        (list (at w1 0 0) (at w2 0 0) (at w3 0 0) (at w4 0 0)
                              (at w1 1 0) (at w2 1 0) (at w3 1 0) (at w4 1 0)
                              (at w1 2 0) (at w2 2 0) (at w3 2 0) (at w4 2 0)
                              (at w1 3 0) (at w2 3 0) (at w3 3 0) (at w4 3 0)))))

(define (my-gl-weight-4w w)
  (let ((v (multi *X* w)))
    (my-gl-vertex-3v v)))

(define (my-gl-weight w1 w2 w3 w4)
  (let ((w (newMatrixFromList- 0 3 0 0 (list w1 w2 w3 w4))))
    (my-gl-weight-4w w)))

(define (drawTetrahedron)
  (gl-color 1.0 1.0 1.0 1.0)
;  (gl-color 0.0 0.0 0.0 1.0)
  (gl-line-width 2.0)
  (gl-begin* GL_LINES
             (begin
               ; X1 - X2
               (my-gl-vertex-3v *X1*)
               (my-gl-vertex-3v *X2*)
               ; X2 - X3
               (my-gl-vertex-3v *X2*)
               (my-gl-vertex-3v *X3*)
               ; X3 - X1
               (my-gl-vertex-3v *X3*)
               (my-gl-vertex-3v *X1*)
               ; X3 - X4
               (my-gl-vertex-3v *X3*)
               (my-gl-vertex-3v *X4*)
               ; X4 - X1
               (my-gl-vertex-3v *X4*)
               (my-gl-vertex-3v *X1*)
               ; X4 - X2
               (my-gl-vertex-3v *X4*)
               (my-gl-vertex-3v *X2*)
	       )
	     )
  (myDrawString (- (at *X1* 0 0) 2) (at *X1* 1 0) (+ (at *X1* 2 0) 1) "X1")
  (myDrawString (+ (at *X2* 0 0) 1) (+ (at *X2* 1 0) 1)(+ (at *X2* 2 0) 1) "X2")
  (myDrawString (- (at *X3* 0 0) 2) (at *X3* 1 0) (- (at *X3* 2 0) 2) "X3")
  (myDrawString (+ (at *X4* 0 0) 1) (at *X4* 1 0)  (at *X4* 2 0)"X4")
  )

(define *PCM4* (newMatrix 0 3 0 3))
(define *EVector*  (newMatrix 0 3 0 0))
(define *EValue* 4)
(define *A1* (newMatrix 0 3 0 3))
(define *A2* (newMatrix 0 3 0 3))
(define *A3* (newMatrix 0 3 0 3))
(define *A4* (newMatrix 0 3 0 3))

(define (setEV)
  (let ((ex (powerMethod *PCM4* 100)))
    (set! *EValue* (car ex))
    (set! *EVector* (car (cdr ex)))
    (normalizeColumn! *EVector* 0)
    (print "C.I. = " (/ (- *EValue* 3.0) 2.0))
    )
)

(define *MODE* 0)
(define *PLANE* 0)

(define (keyboard key x y)
  (cond ((= key 27) (exit 0))
        ((= key 116) (set! *MODE* (remainder (+ *MODE* 1) 7)))
        ((= key 112) (set! *PLANE* (remainder (+ *PLANE* 1) 2)))
        (else (print "key:" key)))
  (glut-post-redisplay))


(define (readMat)
  (call-with-input-file "./20160922-matP.scm" (lambda (iPort) (eval (read iPort) (interaction-environment))))
  (setEV)
  (set! *A1* (nthColumn *PCM4* 0))
  (set! *A2* (nthColumn *PCM4* 1))
  (set! *A3* (nthColumn *PCM4* 2))
  (set! *A4* (nthColumn *PCM4* 3))
  (normalizeColumn! *A1* 0)
  (normalizeColumn! *A2* 0)
  (normalizeColumn! *A3* 0)
  (normalizeColumn! *A4* 0)
  )

(define (drawIntersectionX12)
  (let ((w (newMatrixFromList- 0 3 0 0(list (/ 1 (+ 1 (at *PCM4* 1 0))) (/ 1 (+ 1 (at *PCM4* 0 1))) 0 0))))
    (if (= *PLANE* 0)
        (begin
          (gl-color '#f32(1.0 0.0 0.0 0.2))
          (gl-begin* GL_POLYGON
                     (begin
                       (my-gl-vertex-3v *X4*)
                       (my-gl-weight-4w w)
                       (my-gl-vertex-3v *X3*)
                       )
                     )))
    ))
(define (drawIntersectionX12Frame)
  (let ((w (newMatrixFromList- 0 3 0 0(list (/ 1 (+ 1 (at *PCM4* 1 0))) (/ 1 (+ 1 (at *PCM4* 0 1))) 0 0))))
    (gl-color '#f32(1.0 0.0 0.0 1.0))
    (gl-line-width 0.5)
    (gl-begin* GL_LINE_STRIP
               (begin
               (my-gl-vertex-3v *X4*)
               (my-gl-weight-4w w)
               (my-gl-vertex-3v *X3*)
               ))
    (myDrawString-4w-offset w -2 0 2 (x->string (list  1 ":" (at *PCM4* 1 0))))
))

(define (drawIntersectionX23)
  (let ((w (newMatrixFromList- 0 3 0 0 (list 0 (/ 1 (+ 1 (at *PCM4* 2 1))) (/ 1 (+ 1 (at *PCM4* 1 2))) 0))))
    (if (= *PLANE* 0)
        (begin
          (gl-color '#f32(0.0 1.0 0.0 0.2))
          (gl-begin* GL_POLYGON
                     (begin
                       (my-gl-vertex-3v *X1*)
                       (my-gl-weight-4w w)
                       (my-gl-vertex-3v *X4*)
                       )
                     )))
    ))

(define (drawIntersectionX23Frame)
  (let ((w (newMatrixFromList- 0 3 0 0 (list 0 (/ 1 (+ 1 (at *PCM4* 2 1))) (/ 1 (+ 1 (at *PCM4* 1 2))) 0))))
    (gl-color '#f32(0.0 1.0 0.0 1.0))
    (gl-line-width 0.5)
    (gl-begin* GL_LINE_STRIP
               (begin
                 (my-gl-vertex-3v *X1*)
                 (my-gl-weight-4w w)
                 (my-gl-vertex-3v *X4*)
                 ))
    (myDrawString-4w-offset w -2 1 0 (x->string (list  1 ":" (at *PCM4* 2 1))))
    ))

(define (drawIntersectionX34)
  (let ((w (newMatrixFromList- 0 3 0 0 (list 0 0 (/ 1 (+ 1 (at *PCM4* 3 2))) (/ 1 (+ 1 (at *PCM4* 2 3)))))))
    (if (= *PLANE* 0)
        (begin
          (gl-color '#f32(0.0 0.0 1.0 0.2))
          (gl-begin* |GL_TRIANGLES|
                     (begin
                       (my-gl-vertex-3v *X1*)
                       (my-gl-vertex-3v *X2*)
                       (my-gl-weight-4w w)
                       )
                     )))
))
(define (drawIntersectionX34Frame)
  (let ((w (newMatrixFromList- 0 3 0 0 (list 0 0 (/ 1 (+ 1 (at *PCM4* 3 2))) (/ 1 (+ 1 (at *PCM4* 2 3)))))))
    (gl-color '#f32(0.0 0.0 1.0 1.0))
    (gl-line-width 0.5)
    (gl-begin* GL_LINE_STRIP
               (begin
                 (my-gl-vertex-3v *X1*)
                 (my-gl-vertex-3v *X2*)
                 (my-gl-weight-4w w)
                 (my-gl-vertex-3v *X1*)
                 ))
    (myDrawString-4w-offset w 0 0 0 (x->string (list  1 ":" (at *PCM4* 3 2))))
))

(define (drawIntersectionX31)
  (let ((w (newMatrixFromList- 0 3 0 0 (list (/ 1 (+ 1 (at *PCM4* 2 0))) 0 (/ 1 (+ 1 (at *PCM4* 0 2))) 0))))
    (if (= *PLANE* 0)
        (begin
          (gl-color '#f32(1.0 1.0 0.0 0.2))
          (gl-begin* |GL_TRIANGLES|
                     (begin
                       (my-gl-vertex-3v *X2*)
                       (my-gl-weight-4w w)
                       (my-gl-vertex-3v *X4*)
                       )
                     )))
))
(define (drawIntersectionX31Frame)
  (let ((w (newMatrixFromList- 0 3 0 0 (list (/ 1 (+ 1 (at *PCM4* 2 0))) 0 (/ 1 (+ 1 (at *PCM4* 0 2))) 0))))
    (gl-color '#f32(1.0 1.0 0.0 1.0))
    (gl-line-width 0.5)
    (gl-begin* GL_LINE_STRIP
               (begin
                 (my-gl-vertex-3v *X2*)
                 (my-gl-weight-4w w)
                 (my-gl-vertex-3v *X4*)
                 (my-gl-vertex-3v *X2*)
                 ))
    (myDrawString-4w-offset w -3 -1 0 (x->string (list  1 ":" (at *PCM4* 2 0))))
))

(define (drawIntersectionX42)
  (let ((w (newMatrixFromList- 0 3 0 0 (list 0 (/ 1 (+ 1 (at *PCM4* 3 1))) 0 (/ 1 (+ 1 (at *PCM4* 1 3)))))))
    (if (= *PLANE* 0)
        (begin
          (gl-color '#f32(0.0 1.0 1.0 0.2))
          (gl-begin* |GL_TRIANGLES|
                     (begin
                       (my-gl-vertex-3v *X1*)
                       (my-gl-vertex-3v *X3*)
                       (my-gl-weight-4w w)
                       )
                     )))
))

(define (drawIntersectionX42Frame)
  (let ((w (newMatrixFromList- 0 3 0 0 (list 0 (/ 1 (+ 1 (at *PCM4* 3 1))) 0 (/ 1 (+ 1 (at *PCM4* 1 3)))))))
    (gl-color '#f32(0.0 1.0 1.0 1.0))
    (gl-line-width 0.5)
    (gl-begin* GL_LINE_STRIP
               (begin
                 (my-gl-vertex-3v *X1*)
                 (my-gl-vertex-3v *X3*)
                 (my-gl-weight-4w w)
                 (my-gl-vertex-3v *X1*)
                 ))
    (myDrawString-4w-offset w 0 0 0 (x->string (list  1 ":" (at *PCM4* 3 1))))
))

(define (drawIntersectionX41)
  (let ((w (newMatrixFromList- 0 3 0 0 (list (/ 1 (+ 1 (at *PCM4* 3 0))) 0 0 (/ 1 (+ 1 (at *PCM4* 0 3)))))))
    (if (= *PLANE* 0)
        (begin
          (gl-color '#f32(1.0 0.0 1.0 0.2))
          (gl-begin* |GL_TRIANGLES|
                     (begin
                       (my-gl-vertex-3v *X2*)
                       (my-gl-weight-4w w)
                       (my-gl-vertex-3v *X3*)
                       )
                     )))
))
(define (drawIntersectionX41Frame)
  (let ((w (newMatrixFromList- 0 3 0 0 (list (/ 1 (+ 1 (at *PCM4* 3 0))) 0 0 (/ 1 (+ 1 (at *PCM4* 0 3)))))))
    (gl-color '#f32(1.0 0.0 1.0 1.0))
    (gl-line-width 0.5)
    (gl-begin* GL_LINE_STRIP
               (begin
                 (my-gl-vertex-3v *X2*)
                 (my-gl-weight-4w w)
                 (my-gl-vertex-3v *X3*)
                 (my-gl-vertex-3v *X2*)
                 ))
          (myDrawString-4w-offset w -2 -2 0 (x->string (list  1 ":" (at *PCM4* 3 0))))
))

(define (drawIntersections)
;  (gl-color 0.7 0.7 0.7 0.2)
;  (gl-line-width 1.0)
  (cond ((= *MODE* 0)
         (drawIntersectionX12Frame)
         (drawIntersectionX23Frame)
         (drawIntersectionX34Frame)
         (drawIntersectionX31Frame)
         (drawIntersectionX42Frame)
         (drawIntersectionX41Frame)
         (drawIntersectionX12)
         (drawIntersectionX23)
         (drawIntersectionX31)
         (drawIntersectionX34)
         (drawIntersectionX42)
         (drawIntersectionX41)
         )
        ((= *MODE* 1)
         (drawIntersectionX12Frame)
         (drawIntersectionX12))
        ((= *MODE* 2)
         (drawIntersectionX23Frame)
         (drawIntersectionX23))
        ((= *MODE* 3)
         (drawIntersectionX34Frame)
         (drawIntersectionX34))
        ((= *MODE* 4)
         (drawIntersectionX31Frame)
         (drawIntersectionX31))
        ((= *MODE* 5)
         (drawIntersectionX42Frame)
         (drawIntersectionX42))
        ((= *MODE* 6)
         (drawIntersectionX41Frame)
         (drawIntersectionX41))
        )
)

(define (drawAs)
 (gl-color '#f32(1.0 1.0 1.0 1.0))
 (gl-line-width 0.5)
 (myDrawString-4w-offset *A1* 0 0 0 (x->string "r1"))
 (gl-begin* GL_LINE_STRIP
            (begin
              (my-gl-weight-4w *A1*)
              (my-gl-weight-4w *A2*)
              (my-gl-weight-4w *A3*)
              (my-gl-weight-4w *A1*)
              ))
 (gl-begin* GL_LINE_STRIP
            (begin
              (my-gl-weight-4w *A1*)
              (my-gl-weight-4w *A3*)
              (my-gl-weight-4w *A4*)
              (my-gl-weight-4w *A1*)
              ))
 (gl-begin* GL_LINE_STRIP
            (begin
              (my-gl-weight-4w *A2*)
              (my-gl-weight-4w *A3*)
              (my-gl-weight-4w *A4*)
              (my-gl-weight-4w *A2*)
              ))
 (gl-begin* GL_LINE_STRIP
            (begin
              (my-gl-weight-4w *A1*)
              (my-gl-weight-4w *A4*)
              (my-gl-weight-4w *A2*)
              (my-gl-weight-4w *A1*)
              ))
)
(define (drawEV)
  (gl-color '#f32(1.0 1.0 1.0 1.0))
;  (gl-color '#f32(0.0 0.0 0.0 1.0))
;  (gl-begin* GL_POINTS
;             (begin
;               (my-gl-weight-4w *EVector*)
;               ))
  (gl-line-width 2.0)
  (let ((er (multi *X* *EVector*))
        (d 1))
    (gl-begin* GL_LINES
               (begin
                 (my-gl-vertex (at er 0 0) (at er 1 0) (at er 2 0))
                 (my-gl-vertex (+ (at er 0 0) d) (at er 1 0) (at er 2 0))
                 (my-gl-vertex (at er 0 0) (at er 1 0) (at er 2 0))
                 (my-gl-vertex (- (at er 0 0) d) (at er 1 0) (at er 2 0))
                 (my-gl-vertex (at er 0 0) (at er 1 0) (at er 2 0))
                 (my-gl-vertex (at er 0 0) (+ (at er 1 0) d) (at er 2 0))
                 (my-gl-vertex (at er 0 0) (at er 1 0) (at er 2 0))
                 (my-gl-vertex (at er 0 0) (- (at er 1 0) d) (at er 2 0))
                 (my-gl-vertex (at er 0 0) (at er 1 0) (at er 2 0))
                 (my-gl-vertex (at er 0 0) (at er 1 0) (+ (at er 2 0)d))
                 (my-gl-vertex (at er 0 0) (at er 1 0) (at er 2 0))
                 (my-gl-vertex (at er 0 0) (at er 1 0) (- (at er 2 0)d))
                 )))
)

(define (myTest)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (drawEV)
  (drawAs)
  (drawTetrahedron)
  (drawIntersections)
)

(define (init)
  (gl-enable |GL_BLEND|)
  (gl-disable GL_CULL_FACE)
  (gl-blend-func |GL_SRC_ALPHA| |GL_ONE_MINUS_SRC_ALPHA|)
;  (gl-blend-func |GL_SRC_ALPHA| |GL_ONE|)
;  (gl-shade-model |GL_FLAT|)
  (gl-clear-color 0.0 0.0 0.0 0.0)
;  (gl-clear-color 1.0 1.0 1.0 1.0)
  )

(define (main args)
  (readMat)
  (print "\n\n\nMatrix")
  (stdout *PCM4*)
  (glut-init args)
  (glut-init-display-mode (logior |GLUT_SINGLE| |GLUT_RGB|))
;  (simple-viewer-display (lambda () (glut-wire-sphere 2.0 10 8)))
  (simple-viewer-grid #f)
  (simple-viewer-axis #f)
  (simple-viewer-display myTest)
  (simple-viewer-window 'demo)
  (init)
  (glut-keyboard-func keyboard)
  (simple-viewer-run)
  0)
