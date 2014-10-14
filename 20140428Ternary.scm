(use gl)
(use gl.glut)

(require "./myzLinear.scm")
(use myzLinear)

(define (iter f ls)
  (cond
   ((null? ls)) ; do nothing
   ((pair? ls)
    (f (car ls))
    (iter f (cdr ls)))
   (else (error "Cannot iterate over a non-list"))))

(define *WinWidth* 500)
(define *WinHeight* 500)


(define matEx1 (newMatrixFromList- 0 2 0 2 '( ;; id
 1      1    1
 1      1    1
 1      1    1
)))
(define matEx2 (newMatrixFromList- 0 2 0 2 '(
  1     1/2  3
  2     1    1/5
  1/3   5    1
 )))
(define matEx3 (newMatrixFromList- 0 2 0 2 '(
 1    1/2    1/3
 2    1      2/3
 3    3/2    1
 )))
(define matEx4 (newMatrixFromList- 0 2 0 2 '( ; test
 1 2 1/10
 1/2 1   1/2
 10 2 1
)))
(define matEx5 (newMatrixFromList- 0 2 0 2 '(
 1 1/2 30
 2 1   60
 1/30 1/60 1
)))


(define (setWindowSize aw ah)
  (set! *WinWidth* aw)
  (set! *WinHeight* ah))

(define matX (newMatrix 0 1 0 2))
(define X (newMatrix 0 2 0 2))
(define x1 (newMatrix 0 1 0 0))
(define x2 (newMatrix 0 1 0 0))
(define x3 (newMatrix 0 1 0 0))

(define pickedPoint3 (newMatrix 0 2 0 0))

(define eigenValue 0)
(define eigenVector (newMatrix 0 1 0 0))

(define (setX)
  (set matX 0 0 (* 0.1 *WinWidth*))
  (set matX 1 0 (* 0.1 *WinHeight*))
  (set matX 0 1 (* 0.9 *WinWidth*))
  (set matX 1 1 (* 0.1 *WinHeight*))
  (set matX 0 2 (* 0.5 *WinWidth*))
  (set matX 1 2 (+ (/ (* (sqrt 3) (* 0.8 *WinWidth*)) 2.0) (* 0.1 *WinHeight*)))

  (matFill X 1)
  (copyTo matX X)

  (set x1 0 0 (at matX 0 0))
  (set x1 1 0 (at matX 1 0))
  (set x2 0 0 (at matX 0 1))
  (set x2 1 0 (at matX 1 1))
  (set x3 0 0 (at matX 0 2))
  (set x3 1 0 (at matX 1 2))
  )


(define matP matEx1)

(define (gl-vertex-lis ls)
  (gl-vertex (car ls) (car (cdr ls))))
(define (gl-vertex-vec2 vec)
  (gl-vertex (at vec 0 0) (at vec 1 0)))
(define (gl-vertex-vec3 vec)
  (let ((nVec (copy vec)))
    (normalizeColumn! nVec 0)
    (gl-vertex-vec2 (vec3ToXvec nVec))))

(define (u2r-y ay) (+ (* ay -1) *WinHeight*))
(define (u2r-x ax) ax)

(define (r2u-x ax) ax)
(define (r2u-y ay) (+ (* ay -1) * *WinHeight* ))

(define (vec3ToXvec vec)
  (multi matX vec))

(define (setEigenVector)
  (let ((ex (powerMethod matP 100)))
    (set! eigenValue (car ex))
    (set! eigenVector (car (cdr ex)))
    (print "C.I. = " (/ (- eigenValue 3.0) 2.0))
    )
  )

(define (init)
  (setX)
  (setEigenVector)
  (copyTo eigenVector pickedPoint3)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-shade-model GL_SMOOTH)
  )

(define (drawTriangle)
  (gl-color 1.0 1.0 1.0 0.0)
  (gl-line-width 2.0)
  (gl-begin GL_LINE_STRIP)
  (gl-vertex-vec2 x1)
  (gl-vertex-vec2 x2)
  (gl-vertex-vec2 x3)
  (gl-vertex-vec2 x1)
  (gl-end)
  )

(define (drawLines)
  (let ( (d1 (addMat (multiScalar (/ 1.0 (+ 1 (at matP 0 1))) (subMat x2 x1)) x1))
	 (d2 (addMat (multiScalar (/ 1 (+ 1 (at matP 1 2))) (subMat x3 x2)) x2))
	 (d3 (addMat (multiScalar (/ 1 (+ 1 (at matP 0 2))) (subMat x3 x1)) x1)))
    (gl-color 0.5 1.0 1.0 0.0)
    (gl-line-width 1.0)
    (gl-begin GL_LINES)
    (gl-vertex-vec2 d1)
    (gl-vertex-vec2 x3)
    (gl-vertex-vec2 x1)
    (gl-vertex-vec2 d2)
    (gl-vertex-vec2 x2)
    (gl-vertex-vec2 d3)
    (gl-end)
    (drawString (- (at d1 0 0) 15) (- (at d1 1 0) 15) (x->string (list 1 ":" (at matP 0 1))))
    (drawString (+ (at d2 0 0) 5) (- (at d2 1 0) 0) (x->string (list 1 ":" (at matP 1 2))))
    (drawString (- (at d3 0 0) 70) (- (at d3 1 0) 0) (x->string (list 1 ":" (at matP 0 2))))
    )
  )

(define (drawInnerTriangle)
  (let ( (p1 (addMat (multiScalar (/ (at matP 0 2) (+ (* (at matP 0 1) (at matP 0 2)) (at matP 0 1) (at matP 0 2))) (subMat x2 x1))
		     (multiScalar (/ (at matP 0 1) (+ (* (at matP 0 1) (at matP 0 2)) (at matP 0 1) (at matP 0 2))) (subMat x3 x1)))
	     )
	 (p2 (addMat (multiScalar (/ (at matP 1 2) (+ (* (at matP 1 0) (at matP 1 2)) (at matP 1 0) (at matP 1 2))) (subMat x1 x2))
		     (multiScalar (/ (at matP 1 0) (+ (* (at matP 1 0) (at matP 1 2)) (at matP 1 0) (at matP 1 2))) (subMat x3 x2)))
	     )
	 (p3 (addMat (multiScalar (/ (at matP 2 0) (+ (* (at matP 2 1) (at matP 2 0)) (at matP 2 1) (at matP 2 0))) (subMat x2 x3))
		     (multiScalar (/ (at matP 2 1) (+ (* (at matP 2 1) (at matP 2 0)) (at matP 2 1) (at matP 2 0))) (subMat x1 x3)))
	     )
	 )
    (gl-color 0.0 0.3 0.3 0.0)
    (gl-line-width 1.0)
    (gl-begin GL_POLYGON)
    (gl-vertex-vec2 (addMat p1 x1))
    (gl-vertex-vec2 (addMat p2 x2))
    (gl-vertex-vec2 (addMat p3 x3))
    (gl-end)
    )
  )

(define (plotEigenVector)
  (let ( (eigen (vec3ToXvec eigenVector))
	 (offsetx (newMatrixFromList- 0 1 0 0 '(10 0)))
	 (offsety (newMatrixFromList- 0 1 0 0 '(0 10))))
    (gl-color 1.0 0.5 0.5 0.0)
    (gl-line-width 2.0)
    (gl-begin GL_LINES)
    (gl-vertex-vec2 (addMat eigen offsetx))
    (gl-vertex-vec2 (subMat eigen offsetx))
    (gl-vertex-vec2 (addMat eigen offsety))
    (gl-vertex-vec2 (subMat eigen offsety))
    (gl-end)
    (drawString (+ (at eigen 0 0) 5) (+ (at eigen 1 0) 5) "EigenVector")
    )
  )

(define (putPoints)
  (let ((p1 (nthColumn matP 0))
	(p2 (nthColumn matP 1))
	(p3 (nthColumn matP 2))
	)
    (gl-color 1.0 0.0 0.0 0.0)
    (gl-point-size 10.0)
    (gl-begin GL_POINTS)
    (gl-vertex-vec3 pickedPoint3)
    (gl-end)
    )
  )

(define (disp)
  (gl-clear GL_COLOR_BUFFER_BIT)
  (drawInnerTriangle)
  (drawTriangle)
  (drawAxisNames)
  (drawLines)
  (putPoints)
  (plotEigenVector)
  (gl-flush)
  )

(define (pickUpPoint button state x y)
  (when (and (= button GLUT_LEFT_BUTTON) (= state GLUT_DOWN))
	(let* ( (b (newMatrixFromList- 0 2 0 0 (list (u2r-x x) (u2r-y y) 1)))
		(w (ref (gauss X b) 5)))
	  (copyTo w pickedPoint3)
	  (print (list "[w1, w2, w3] = [" (at w 0 0) "," (at w 1 0) "," (at w 2 0) "]"))
	  )
	)
  (glut-post-redisplay)
  )

(define (drawString x y s)
  (gl-raster-pos x y)
  (iter (lambda (c) (glut-bitmap-character GLUT_BITMAP_8_BY_13 (char->integer c))) (string->list s))
  )

(define (drawAxisNames)
  (drawString (- (at matX 0 0) 20) (- (at matX 1 0) 15) "X1")
  (drawString (+ (at matX 0 1) 10)  (- (at matX 1 1) 15) "X2")
  (drawString (at matX 0 2) (+ (at matX 1 2) 10) "X3")
  )


(define (reshape w h)
  (gl-viewport 0 0 w h)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (glu-ortho-2d 0.0 w 0.0 h)
  (gl-matrix-mode GL_MODELVIEW)
  (setWindowSize w h)
  (setX)
  )

(define (resetMatP)
  (call-with-input-file "./20140428matP.scm" (lambda (iPort) (eval (read iPort) (interaction-environment))))
  (setEigenVector)
  )

(define (keyboard key x y)
  (cond ((= key 27) (exit 0))
	((= key 114) (begin (print "load from file")(resetMatP)(glut-post-redisplay)))
	((= key 49) (begin (set! matP matEx1) (setEigenVector)(glut-post-redisplay)))
	((= key 50) (begin (set! matP matEx2) (setEigenVector)(glut-post-redisplay)))
	((= key 51) (begin (set! matP matEx3) (setEigenVector)(glut-post-redisplay)))
	((= key 52) (begin (set! matP matEx4) (setEigenVector)(glut-post-redisplay)))
	((= key 53) (begin (set! matP matEx5) (setEigenVector)(glut-post-redisplay)))
	(else (print "key:" key))))


(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_SINGLE GLUT_RGB))
  (glut-init-window-size *WinWidth* *WinHeight*)
  (glut-init-window-position 100 100)
  (glut-create-window *program-name*)
  (init)
  (glut-display-func disp)
  (glut-mouse-func pickUpPoint)
  (glut-reshape-func reshape)
  (glut-keyboard-func keyboard)
  (glut-main-loop)
  0)
