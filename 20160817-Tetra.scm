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
  (let ((u (newMatrix 0 2 0 0)))
    (r2u-3v v u)
    (my-gl-vertex (at u 0 0) (at u 1 0) (at u 2 0))
    )
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

(define *AXLENGTH* 15)
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
  (gl-color 0.5 0.5 0.5 0.0)
  (gl-line-width 1.0)
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
  (myDrawString 0 0 0 "Test")
  )

(define *PCM4* (newMatrix 0 3 0 3))

(define (readMat)
  (call-with-input-file "./20160818-matP.scm" (lambda (iPort) (eval (read iPort) (interaction-environment))))
;  (setEigenVector)
  )

(define (drawIntersections)
  (gl-color 0.5 0.5 0.5 0.0)
  (gl-line-width 1.0)
  (gl-begin* GL_LINE_STRIP
             (begin
               ; X4 - X12 - X3 - X4
               (my-gl-vertex-3v *X4*)
               (my-gl-weight (/ 1 (+ 1 (at *PCM4* 1 0))) (/ 1 (+ 1 (at *PCM4* 0 1))) 0 0)
               (my-gl-vertex-3v *X3*)
               (my-gl-vertex-3v *X4*)
               ; X1 - X23 - X4 - X1
               (my-gl-vertex-3v *X1*)
               (my-gl-weight 0 (/ 1 (+ 1 (at *PCM4* 2 1))) (/ 1 (+ 1 (at *PCM4* 1 2))) 0)
               (my-gl-vertex-3v *X4*)
               (my-gl-vertex-3v *X1*)
               ; X1 - X2 - X34 - X1
               (my-gl-vertex-3v *X1*)
               (my-gl-vertex-3v *X2*)
               (my-gl-weight 0 0 (/ 1 (+ 1 (at *PCM4* 3 2))) (/ 1 (+ 1 (at *PCM4* 2 3))))
               (my-gl-vertex-3v *X1*)
               ; X2 - X3 - X41 - X2
               (my-gl-vertex-3v *X2*)
               (my-gl-vertex-3v *X3*)
               (my-gl-weight (/ 1 (+ 1 (at *PCM4* 0 3))) 0 0 (/ 1 (+ 1 (at *PCM4* 3 0))))
               (my-gl-vertex-3v *X2*)
	       )
	     )
  )


(define (myTest)
  (drawTetrahedron)
  (drawIntersections)
  (gl-color 0.5 0.5 0.5 0.0)
  (gl-line-width 1.0)
)
(define (grid3 v)
  #f
  )

(define (main args)
  (readMat)
  (glut-init args)
;  (simple-viewer-display (lambda () (glut-wire-sphere 2.0 10 8)))
  (simple-viewer-grid #f)
  (simple-viewer-axis #f)
  (simple-viewer-display myTest)
  (simple-viewer-window 'demo)
  (simple-viewer-run)
  0)
