(

(define (my-test)
  (let*
    (image (gimp_xcf_load 0 "test.xcf" "test.xcf"))
  )
  (set!  theImage (car (gimp-image-duplicate image)))
  (set!  theLayer (car (gimp-image-get-active-layer theImage)))
  (gimp-image-set-filename theImage "testNew.jpg")
  (file-jpeg-save
    1 theImage theLayer "testNew.jpg" "testNew.jpg" 1 0 0 0 "" 0 0 0 0)
)

(gimp-quit 0)
)