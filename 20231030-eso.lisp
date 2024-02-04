;;; ----------------------------------------------------
;;; Object like ECMAScript's Object
;;; 属性が見つからない場合は 'NOT-EXIST を返す。(NIL を値として持っている場合と区別する。)
;;; 値として NIL を格納する時には内部的に 'ESO-NIL という別名を格納しておいて、取り出すときに NIL を返す。

(defun make-esobject ()
    (let ((ht (make-hash-table :test #'equal)))
        (setf (gethash "esobject-id" ht) (gensym))
        (setf (gethash "is-esobject" ht) (= 1 1))
        ht))

(defun esobject? (o) (and (hash-table-p o) (oget o "is-esobject")))

(defun oset (o attr val)
    (if (eq val 'NIL)
      (setf (gethash attr o) 'ESO-NIL)
      (setf (gethash attr o) val)))

(defun oget (o attr)
    (let ((v (gethash attr o)))
        (if (eq v 'NIL)
            'NOT-EXIST
            (if (eq v 'ESO-NIL) NIL v))))

(defun oid (o) (oget o "esobject-id"))

(defun attrs (o)
    (let ((l '()))
        (maphash #'(lambda (attr val) (push attr l)) o)
        l))

(defun mapfor (l f) (mapcar f l))

(defun list-to-esobject (ps)
    (let ((o (make-esobject)))
        (mapfor ps #'(lambda (p)
            (if (listp p) 
                  (cond
                     ((null (cdr p)) (oset o (car p) NIL))
                     ((listp (cdr p)) (oset o (car p) (list-to-esobject (cdr p))))
                     (t (oset o (car p) (cdr p)))))))
        o))
         
(defparameter *OPRINT-DONE-LIST* '())

(defun oprint (o)
    (format t "~%")
    (setf *OPRINT-DONE-LIST* '())
    (oprint-each 0 o))

(defun oprint-each (indent o)
    (if (find (oid o) *OPRINT-DONE-LIST*)
        (progn
            (dotimes (i indent) (princ " "))
            (format t "<ES-OBJECT> ~a (Already printed)~%" (oid o)))
        (progn 
            (push (oid o) *OPRINT-DONE-LIST*) 
            (mapc #'(lambda (a) (oprint-each-attr indent o a)) (attrs o)))))

(defun oprint-each-attr (indent o attr)
    (dotimes (i indent) (princ " "))
    (format t "~a : " attr)
    (let ((v (oget o attr)))
        (if (esobject? v)
            (progn (format t "~%")(oprint-each (+ 4 indent) v))
            (format t "~a~%" (oget o attr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter t1 (make-esobject))

(oset t1 "name" "test")
(oset t1 "color" 255)
(oset t1 "forward" NIL)
(oset t1 "f" #'(lambda (x) (+ x x)))

(oset t1 "sub" (make-esobject))
(oset t1 "hai" "hoo")
(oset (oget t1 "sub") "sub-name" "this is sub.")
(oset (oget t1 "sub") "subsub" (make-esobject))
(oset (oget t1 "sub") "cycled" t1)
(oset (oget (oget t1 "sub") "subsub") "nest" "nest-nest")

(oprint t1)

(exit)



;;
;; collection: parameter space
;;
(defun new-param-space ()
     (make-hash-table :test #'equal))

(defun get-param (params name create-func)
    ; return parameter with the name
    ;   params: parameter space
    ;   name: parameter name
    ;   create-func: how create new parameter when the parameter does not exist
    (let ((p (gethash name params)))
        (if p 
            p
            (progn 
                (let ((np (funcall create-func )))
                    (setf (gethash name params)np)
                    np)))))

;;
;; Arrow
;;
(defstruct arrow name from to forward forward-buffer backward)

(defun new-arrow (name params)
    (make-arrow
        :name name
        :from 0 
        :to 0
        :forward #'id
        :backward #'id
        :forward-buffer 0)
    )

(defun forward-id (arrow params) )




(defun param-name (arrow param)
    (concatenate 'string (arrow-name arrow) "-" param))

(setf a (make-arrow :name "testArrow"))
(print a)

(setf params (new-param-space))

(print params)

(print (get-param params (param-name a "w") #'(lambda () 1)))
(print (get-param params (param-name a "w") #'(lambda () 0)))
(print (get-param params (param-name a "beta") #'(lambda () '(2 3))))
(print (get-param params (param-name a "beta") #'(lambda () '())))
(print (get-param params "b" #'(lambda () 1)))
(print params)
