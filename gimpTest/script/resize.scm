(define (my-max x y)
  (if (> x y) x y))

(define (resize filename outputfilename)
	(let* ((image0 (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
	       (image (begin
			(gimp-edit-copy-visible image0)
			(car (gimp-edit-paste-as-new))))
          	(drawable (car (gimp-image-get-active-layer image)))
		(new-width 900)
		(new-height 900)
		(new-length 900)
		(cur-width (car (gimp-image-width image)))
		(cur-height (car (gimp-image-height image)))
;		(ratio      (my-min (/ new-width cur-width) (/ new-height cur-height)))
		(ratio      (/ new-length (my-max cur-width cur-height)))
		(width      (* ratio cur-width))
		(height     (* ratio cur-height))
		)
	        (gimp-image-scale-full image width height 2)
		(gimp-file-save RUN-NONINTERACTIVE image drawable outputfilename outputfilename)
     		(gimp-image-delete image0)
     		(gimp-image-delete image)
		))


