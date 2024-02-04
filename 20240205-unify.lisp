;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISP exercise (without defmacro)
;;
;; Quasi-Destructive Unification Algorithm that introduced:
;;   https://maxpec.net/amazoncampaign_present/Dr_Tomabechi_treatise.pdf 
;;   (Tomabechi, H.: Efficient Unification for Natural Language, 1993)
;;
;; Memo
;;  - add 'timestamp' to each node for print-nodes. (to print 'generation' values)
;;  - add 'timestamp' to each arc for unifying graphs with minimal cycles.such as a->a.
;;  - data-sharing version not implemented correctly.
;;  - 'inconsistency' is represended as node. So, modify for unify-dg(inconsistency, dg)
;;  - add generalization operation.
;;       dg1
;;         -a-> 0 (atm 0)
;;         -b-> 1 (atm 1)
;;         -c-> 0    ... reentrant
;;       dg2
;;         -a-> 0 (atm 0)
;;         -b-> 1 (atm 1)
;;         -c-> 1    ... reentrant
;;       dg that generalizes dg1 and dg2 be:
;;         -a-> 0 (atm 0)
;;         -b-> 1 (atm 1)
;;         -c-> X (var)
;;
;;    + todo : consider generalize cyclic dg.
;;  - todo : serialization.
;;
;; % clisp --version 
;; GNU CLISP 2.49.92 (2018-02-18) (built on ventura-arm64.local [127.0.0.1])
;; Software: GNU C Apple LLVM 14.0.0 (clang-1400.0.29.202)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *time-counter* 0)
(defun get-current-time () *time-counter*)
(defun get-next-time () (incf *time-counter*) *time-counter*)

;; (metadata  .  contents)
;; metadata : id, type-of-contents 
(defstruct container (what-is         "CONTAINER")
                     (id              (gensym))
                     (allowed-type-p  (lambda (x) (eq 1 1)))
                     (contents        '()))

(defun container-size (cs) (length (container-contents cs)))

(defun container-add (cs c)
    (if (funcall (container-allowed-type-p cs) c)
      (setf (container-contents cs) (append (container-contents cs) (list c)))
      (progn (print (container-what-is cs)) (print "EXIT: TYPE-ERROR")(exit))))

(defun container-foreach (cs f)
    (mapfor (container-contents cs) #'(lambda (c) (funcall f c))))

(defun container-is-empty (cs)
    (eq (container-contents cs) ()))

(defun container-discard (cs)
    "Discard all elements."
    (setf (container-contents cs) '()))

(defun arc (label value)
    ;(when (not (stringp label))
    (unless (stringp label)
        (print "ERROR : ARC LABEL MUST BE STRING")
        (exit))
    (let ((time (get-next-time)))
        (cons 'LIVE (cons time (cons label value)))))
(defun arc-label (a) (caddr a))
(defun arc-same-label (a1 a2) (string= (arc-label a1) (arc-label a2)))
(defun arc-value (a) (cdddr a))
(defun arc-timestamp (a) (cadr a))
(defun arc-set-timestamp (a time) (setf (cadr a) time))
(defun arc-flg (a) (car a))
(defun arc-set-flg (a flg) (setf (car a) flg))
(defun arc-p (a) (and (consp a) (stringp (caddr a))))

(defun arcs ()
    (let ((as (make-container)))
        (setf (container-what-is as) "ARCS")
        (setf (container-allowed-type-p as) #'arc-p)
        as))
(defun arcs-contents (as) (container-contents as))
(defun arcs-size (as) (container-size as))
(defun arcs-add (as a) (container-add as a))
(defun arcs-foreach (as f) (container-foreach as f))
(defun arcs-is-empty (as) (container-is-empty as))
(defun arcs-discard (as)  (container-discard as)) ; 空にする。
(defun arcs-append (base as)
    (arcs-foreach as #'(lambda (a) (arcs-add base a))))
(defun arcs-from-list (ls)
    (let ((as (arcs)))
        (mapfor ls #'(lambda (l) (arcs-add as l)))
        as))

(defun arcs-intersection (as1 as2)
    "return as arcs (container, not list)."
    (let ((l1 (container-contents as1))
          (l2 (container-contents as2)))
        (arcs-from-list (intersection l1 l2 :test #'arc-same-label))))

(defun arcs-complement (as1 as2)
    "return arcs (container, not list) of as1 that as2 does not have."
    (let ((result (arcs)))
        (arcs-foreach as1 #'(lambda (a)
            ;(when (not (member a (container-contents as2) :test #'arc-same-label))
            (unless (member a (container-contents as2) :test #'arc-same-label)
                (arcs-add result a))))
         result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NODE that represets feature structure.
;; There are four types of feature structure: atom, variable, inconsistency, complex feature structure (cfs).
(defstruct node
    type
    (id           (gensym))
    (name         "NO-NAME")
    arc-list
    comp-arc-list
    (forward      'EMPTY)
    (copy         'EMPTY)
    (generation   0)
    (timestamp    0)) ; for print node which has cycles.

(defparameter *nodes* '()) ;; stores all nodes

;; atom ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun atm (str)
    (let ((a (make-node :type 'ATOM :name str)))
        (push a *nodes*)
        a))
(defun atm-p (n) (eq (node-type n) 'ATOM))
(defun atm-equal (a1 a2) (equal (node-name a1) (node-name a2)))
(defun atm-str (a) (format nil "@~A:~A" (node-name a) (node-id a)))

;; variable ;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun var (name)
    (let ((v (make-node :type 'VARIABLE :name name)))
        (push v *nodes*)
        v))
(defun var-p (n) (eq (node-type n) 'VARIABLE))
(defun var-str (a) (format nil "Var-~A:~A" (node-name a) (node-id a) ))

;; inconsistency ;;;;;;;;;;;;;;;;;;;;; 
(defun inconsistency ()
    (let ((i (make-node :type 'INCONSISTENCY)))
        (push i *nodes*)
        i))
(defun inconsistency-p (n) (eq (node-type n) 'INCONSISTENCY))
(defun inconsistency-str (i) (concatenate 'string "INCONCISTENCY:" (string (node-id i))))

;; complex feature structure ;;;;;;;;; 
(defun cfs () 
    "complex feature structure"
    (let ((c (make-node :type 'COMPLEX  :arc-list (arcs) :comp-arc-list (arcs))))
        ;(setf (node-timestamp c) (get-next-time))
        (push c *nodes*)
        c))
(defun cfs-p (n) (eq (node-type n) 'COMPLEX))
(defun cfs-arc (n label)
    "return a value of the arc with the label."
    (arcs-foreach (node-arc-list n) #'(lambda (a)
        (when (string= (arc-label a) label)
            (return-from cfs-arc a))))
    (format t "~%ERROR: IN CFS-ARC-VALUE, THERE IS NO ARC WITH LABEL ~A~%" label)
    (exit))
(defun cfs-arc-value (n label) (arc-value (cfs-arc n label)))
(defun cfs-clear-arc-flg-and-timestamp (c)
    (when (cfs-p c)
        (cfs-clear-arc-flg-and-timestamp0 c (get-next-time))))
(defun cfs-clear-arc-flg-and-timestamp0 (c past-time)
   (arcs-foreach (node-arc-list c) #'(lambda (a)
      (when (< (arc-timestamp a) past-time)
         (arc-set-timestamp a (get-next-time))
         (arc-set-flg a 'LIVE)
         (when (cfs-p (arc-value a))
            (cfs-clear-arc-flg-and-timestamp0 (arc-value a) past-time))))))

;;
;; for abbreviation
;;
(defun -> (l v) (arc l v))
(defun [] (&rest ls)
    (let ((c (cfs))
          (as (arcs)))
        (mapfor ls #'(lambda (l) (arcs-add as l)))
        (setf (node-arc-list c) as)
        c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; print-node
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun print-arcs-as-node (msg as)
    (print msg)
    (print "")
    (let ((cs (cfs)))
        (setf (node-arc-list cs) as)
        (print-node cs)))

(defun print-node (n)
    (get-next-time)
    (print-node-i n 0))

(defun print-node-i (n i)
    (cond ((atm-p n) 
              (format t "~A" (atm-str n))
              (when (>= (node-timestamp n) (get-current-time))
                (format t "  ;;;(already printed)"))
              ;(when (not (eq (node-forward n) 'EMPTY))
              (unless (eq (node-forward n) 'EMPTY)
                (format t " ---> ~A(~A)" (node-id (node-forward n))(node-generation n)))
              (format t "~%")
              (setf (node-timestamp n) (get-current-time)))
          ((var-p n)
              (format t "~A(~A)" (var-str n)(node-generation n))
              (when (>= (node-timestamp n) (get-current-time))
                (format t "  ;;;(already printed)"))
              ;(when (not (eq (node-forward n) 'EMPTY))
              (unless (eq (node-forward n) 'EMPTY)
                (format t " ---> ~A(~A)" (node-id (node-forward n))(node-generation n)))
              (format t "~%")
              (setf (node-timestamp n) (get-current-time)))
          ((inconsistency-p n)
              (format t "~A~%" (inconsistency-str n))
              ;(when (not (eq (node-forward n) 'EMPTY))
              (unless (eq (node-forward n) 'EMPTY)
                (format t " ---> ~A(~A)" (node-id (node-forward n))(node-generation n))))
          ((cfs-p n)
              (format t "[c:~A](~A)" (node-id n)(node-generation n))
              ;(when (not (eq (node-forward n) 'EMPTY))
              (unless (eq (node-forward n) 'EMPTY)
                (format t " ---> ~A(~A)" (node-id (node-forward n))(node-generation n)))
              (if (>= (node-timestamp n) (get-current-time))
                 (format t "  ;;;(already printed)~%")
                 (progn  
                    (let ((indent (n-times-str i " ")))
                        (setf (node-timestamp n) (get-current-time))
                        (format t "~%")
                        (arcs-foreach (node-arc-list n) #'(lambda (a)
                            ;(format t "~A~A *~A ==> " indent (arc-flg a) (arc-label a))
                            (format t "~A *~A ==> " indent (arc-label a))
                            (print-node-i (arc-value a) (+ i 4))))
                        (arcs-foreach (node-comp-arc-list n) #'(lambda (a)
                            (format t "~A +~A ==> " indent (arc-label a))
                            (print-node-i (arc-value a) (+ i 4))))))))
           (t (print "NODE-PRINT-I NOT IMPLEMENTED"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *unify-global-counter* 10)

(defun unify-dg (dg1 dg2)
    (incf *unify-global-counter*)
    (let ((result (unify0 dg1 dg2)))
        (incf *unify-global-counter*)
        result))

(defun unify0 (dg1 dg2)
    (handler-case (let ((result (unify1 dg1 dg2)))
                      (copy-dg-with-comp-arcs dg1))
                      ;(multiple-value-bind (v s) (copy-dg-with-comp-arcs-share dg1) v)) ; data-sharing ver.
                    (UNIFY-FAIL (e)
                        (cfs-clear-arc-flg-and-timestamp dg1)
                        (cfs-clear-arc-flg-and-timestamp dg2)
                        (inconsistency))))

(define-condition UNIFY-FAIL (condition) ())
(defun escape-by-fail () (signal (make-condition 'UNIFY-FAIL)))

(defun unify1 (dg1-underef dg2-underef)
    (let ((dg1 (dereference dg1-underef))
          (dg2 (dereference dg2-underef)))

        (setf (node-copy dg1) 'EMPTY)
        (setf (node-copy dg2) 'EMPTY)

        (when (or (inconsistency-p dg1) (inconsistency-p dg2))
            (escape-by-fail))

        (cond ((eq  dg1 dg2) ; when both are same node
                  (return-from unify1 'UNIFY-SUCCESS))
              ((var-p dg1)
                  (setf (node-forward dg1) dg2)
                  (setf (node-generation dg1) *unify-global-counter*)
                  (return-from unify1 'UNIFY-SUCCESS))
              ((var-p dg2)
                  (setf (node-forward dg2) dg1)
                  (setf (node-generation dg2) *unify-global-counter*)
                  (return-from unify1 'UNIFY-SUCCESS))
              ((and (atm-p dg1) (atm-p dg2))
                  (if (atm-equal dg1 dg2)
                     (progn
                        (setf (node-forward dg2) dg1)
                        (setf (node-generation dg2) *unify-global-counter*)
                        (return-from unify1 'UNIFY-SUCCESS))
                     (escape-by-fail)))
              ((or (atm-p dg1) (atm-p dg2))
                  (escape-by-fail))
              (t  ;; both of dg1 and dg2 are complex feature structures
                  (let ((shared (arcs-intersection (node-arc-list dg1) (node-arc-list dg2))))
                      (arcs-foreach shared #'(lambda (s)
                        (let ((a1 (cfs-arc dg1 (arc-label s)))
                              (a2 (cfs-arc dg2 (arc-label s))))
                            ;(when (not (equal (arc-timestamp a1) (arc-timestamp a2)))
                            (unless (equal (arc-timestamp a1) (arc-timestamp a2))
                                (arc-set-timestamp a1 (get-next-time))
                                (arc-set-timestamp a2 (arc-timestamp a1))
                                (unify1 (arc-value a1) (arc-value a2))
                                (arc-set-timestamp a1 (get-next-time)) (arc-set-timestamp a2 (get-next-time)); 次の呼び出しのために別のタイムスタンプにする
                             ))))
                      (setf (node-forward dg2) dg1)
                      (setf (node-generation dg2) *unify-global-counter*)
                      (let ((newarcs (arcs-complement (node-arc-list dg2) (node-arc-list dg1))))
                          (if (arcs-is-empty (node-comp-arc-list dg1))
                                (progn
                                    (setf (node-generation dg1) *unify-global-counter*)
                                    (setf (node-comp-arc-list dg1) newarcs))
                                (if (= (node-generation dg1) *unify-global-counter*)
                                    (arcs-append (node-comp-arc-list dg1) newarcs)
                                    (arcs-discard (node-comp-arc-list dg1)))))) ;; 何度も実行しないためにここで実行している。先頭で実行しても変わらない。
                       'UNIFY-SUCCESS))))
                  
(defun dereference (n)
    (let ((dest (node-forward n)))
        (when (eq dest n)
            (setf (node-forward n) 'EMPTY)
            n)
        (when (eq dest 'EMPTY)
            (return-from dereference n))
        (when (or (= (node-generation n) *unify-global-counter*)
                  (<= (node-generation n) 9)) ; permanent node
            (return-from dereference (dereference dest)))
        (setf (node-forward n) 'EMPTY)
        n)) 

(defun copy-dg-with-comp-arcs (dg-underef)
    (let ((dg (dereference dg-underef)))
        (cond ((and (not (eq (node-copy dg) 'EMPTY))
                    (= (node-generation (node-copy dg)) *unify-global-counter*))
                  (node-copy dg))
              ((atm-p dg)
                  (let ((newcopy (atm (node-name dg))))
                      (setf (node-generation newcopy) *unify-global-counter*)
                      (setf (node-copy dg) newcopy)
                      newcopy))
              ((var-p dg)
                  (let ((newcopy (var (node-name dg))))
                      (setf (node-generation newcopy) *unify-global-counter*)
                      (setf (node-copy dg) newcopy)
                      newcopy))
              (t
                  (let ((newcopy (cfs)))
                      (setf (node-generation newcopy) *unify-global-counter*)
                      (setf (node-copy dg) newcopy)
                      (arcs-foreach (node-arc-list dg) #'(lambda (a)
                          (let ((newarc (copy-arc a)))
                             (arcs-add (node-arc-list newcopy) newarc))))
                      (when (and (not (arcs-is-empty (node-comp-arc-list dg)))
                                 (= (node-generation dg) *unify-global-counter*))
                          (arcs-foreach (node-comp-arc-list dg) #'(lambda (a)
                             (let ((newarc (copy-arc a)))
                                (arcs-add (node-arc-list newcopy) newarc)))))
                      (arcs-discard (node-comp-arc-list dg))
                      newcopy)))))

(defun copy-arc (a)
    (arc (arc-label a) (copy-dg-with-comp-arcs (arc-value a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copying with structure-shareing ver.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun copy-dg-with-comp-arcs-share (dg-underef)
    (let ((dg (dereference dg-underef)))
        (when (and (not (eq (node-copy dg) 'EMPTY))
                   (= (node-generation (node-copy dg)) *unify-global-counter*))
            (return-from copy-dg-with-comp-arcs-share (values (node-copy dg) 'CHANGED)))
        (when (eq dg dg-underef)
            (return-from copy-dg-with-comp-arcs-share (copy-node-comp-not-forwarded dg)))
        (copy-node-comp-forwarded dg)))

(defun copy-node-comp-not-forwarded (dg)
    (when (or (atm-p dg) (var-p dg))
        (return-from copy-node-comp-not-forwarded (values dg 'NOT-CHANGED)))

    (when (and (not (arcs-is-empty (node-comp-arc-list dg)))
               (= (node-generation dg) *unify-global-counter*))
        (let ((newcopy (cfs)))
            (setf (node-generation newcopy) *unify-global-counter*)
            (setf (node-copy dg) newcopy)
            (arcs-foreach (node-arc-list dg) #'(lambda (a)
                (multiple-value-bind (new-a s) (copy-arc-and-comp-arc-share a)
                    (arcs-add (node-arc-list newcopy) new-a))))
            (arcs-foreach (node-comp-arc-list dg) #'(lambda (a)
                (multiple-value-bind (new-a s) (copy-arc-and-comp-arc-share a)
                    (arcs-add (node-arc-list newcopy) new-a))))
            (arcs-discard (node-comp-arc-list dg))
            (return-from copy-node-comp-not-forwarded (values newcopy 'CHANGED))))

    (let ((state  'NOT-CHANGED)
          (as     (arcs)))
        (setf (node-copy dg) dg)
        (setf (node-generation dg) *unify-global-counter*)
        (arcs-foreach (node-arc-list dg) #'(lambda (a)
            (multiple-value-bind (new-a new-s) (copy-arc-and-comp-arc-share a)
                (arcs-add as new-a)
                (when (eq new-s 'CHANGED) (setf state 'CHANGED)))))
                ;(setf state 'CHANGED))))
        (when (eq state 'CHANGED)
            (let ((newcopy (cfs)))
                (setf (node-generation newcopy) *unify-global-counter*)
                (arcs-append (node-arc-list newcopy) as)
                (setf (node-copy dg) newcopy)
                (return-from copy-node-comp-not-forwarded (values newcopy 'CHANGED))))
        (setf (node-copy dg) 'EMPTY)
        (values dg 'NOT-CHANGED)))

(defun copy-node-comp-forwarded (dg)
    (when (or (atm-p dg) (var-p dg))
        (return-from copy-node-comp-forwarded (values dg 'CHANGED)))

    (when (and (not (arcs-is-empty (node-comp-arc-list dg)))
               (= (node-generation dg) *unify-global-counter*))
        (let ((newcopy (cfs)))
            (setf (node-generation newcopy) *unify-global-counter*)
            (setf (node-copy dg) newcopy)
            (arcs-foreach (node-arcs-list dg) #'(lambda (a)
                (multiple-value-bind (newa s) (copy-arc-and-comp-arc-share a)
                    (arcs-add (node-arc-list newcopy) newa))))
            (arcs-foreach (node-comp-arc-list dg) #'(lambda (a)
                (multiple-value-bind (newa s) (copy-arc-and-comp-arc-share a)
                    (arcs-add (node-arc-list newcopy) newa))))
            (arcs-discard (node-comp-arc-list dg))
            (return-from copy-node-comp-forwarded (values newcopy 'CHANGED))))

    (let ((state 'NOT-CHANGED)
          (as    (arcs)))
        (setf (node-copy dg) dg)
        (setf (node-generation dg) *unify-global-counter*)
        (arcs-foreach (node-arc-list dg) #'(lambda (a)
            (multiple-value-bind (new-a new-s) (copy-arc-and-comp-arc-share a)
                (arcs-add as new-a)
                (when (eq new-s 'CHANGED) (setf state 'CHANGED)))))
                ;(setf state 'CHANGED))))
        (when (eq state 'CHANGED)
            (let ((newcopy (cfs)))
                (setf (node-generation newcopy) *unify-global-counter*)
                (arcs-append (node-arc-list newcopy) as)
                (setf (node-copy dg) newcopy)
                (return-from copy-node-comp-forwarded (values newcopy 'CHANGED))))
        (setf (node-copy dg) 'EMPTY)
        (values dg 'CHANGED)))

(defun copy-arc-and-comp-arc-share (input-arc)
    (multiple-value-bind (dest state) (copy-dg-with-comp-arcs-share (arc-value input-arc))
        (when (eq state 'NOT-CHANGED)
            (return-from copy-arc-and-comp-arc-share (values input-arc 'NOT-CHANGED)))
        (values (arc (arc-label input-arc) dest) 'CHANGED)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Generalization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun generalize-dg (dg1 dg2)
    (incf *unify-global-counter*)
    (cfs-clear-arc-flg-and-timestamp dg1)
    (let ((result (generalize00 dg1 dg2)))
        (incf *unify-global-counter*)
            result))

; これが呼ばれた時点で (not (eq dg1 dg2))
(defun general-node (dg1 dg2 f)
  (let ((newnode (funcall f)))
    (setf (node-generation newnode) *unify-global-counter*)
    ;(when (not (= (node-generation dg1) *unify-global-counter*))
    (unless (= (node-generation dg1) *unify-global-counter*)
         (setf (node-generation dg1) *unify-global-counter*)
         (setf (node-forward dg1) newnode))
    (unless (= (node-generation dg2) *unify-global-counter*)
         (setf (node-generation dg2) *unify-global-counter*)
         (setf (node-forward dg2) newnode))
    ; 上の2つの when に引っかからない例
    ;   (dg1 からも dg2 からも forward されない node ができる。)
    ; dg1
    ;  -a-> 0
    ;  -b-> 1
    ;  -c-> 0
    ; dg2
    ;  -a-> 0
    ;  -b-> 1
    ;  -c-> 1
    newnode))

(defun generalize00 (dg1-underef dg2-underef)
    (let ((dg1 (dereference dg1-underef))
          (dg2 (dereference dg2-underef)))

        (cond
          ((eq dg1 dg2)
            dg1)
          ((var-p dg1) (general-node dg1 dg2 #'(lambda () (var "GDG01"))))
          ((var-p dg2) (general-node dg1 dg2 #'(lambda () (var "GDG02")))) 
          ((and (atm-p dg1) (atm-p dg2))
            (if (atm-equal dg1 dg2)
               (general-node dg1 dg2 #'(lambda () (atm (node-name dg1))))
               (general-node dg1 dg2 #'(lambda () (var "GDG1")))))
          ((or (atm-p dg1) (atm-p dg2))
            (general-node dg1 dg2 #'(lambda () (var "GDG2"))))
          ((and (inconsistency-p dg1) (inconsistency-p dg2)) (inconsistency))
          ((or (inconsistency-p dg1) (inconsistency-p dg2))
            (general-node dg1 dg2 #'(lambda () (var "GDG3"))))
          (t
            (let ((shared (arcs-intersection (node-arc-list dg1) (node-arc-list dg2))))
                (when (arcs-is-empty shared)
                    (let ((newnode (general-node dg1 dg2 #'(lambda () (var "GDG40")))))
                        (return-from generalize00 (var "GDG41"))))

                (let ((newnode (general-node dg1 dg2 #'(lambda () (cfs))))
                      (as (arcs)))
                   (arcs-foreach shared #'(lambda (s)
                        (let ((a1 (cfs-arc dg1 (arc-label s)))
                              (a2 (cfs-arc dg2 (arc-label s))))
                            ;(when (not (equal (arc-timestamp a1) (arc-timestamp a2)))
                            (unless (equal (arc-timestamp a1) (arc-timestamp a2))
                                (arc-set-timestamp a1 (get-next-time))
                                (arc-set-timestamp a2 (arc-timestamp a1))
                                (arcs-add as (arc (arc-label s) (generalize00 (arc-value a1) (arc-value a2))))
                             ))))
                    (arcs-append (node-arc-list newnode) as)
                    newnode))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dg-equal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dg-equal (dg1 dg2)
    (if (eq dg1 dg2)
        T
        (dg-equal00 dg1 dg2 (get-next-time))))

(defun dg-equal00 (dg1-underef dg2-underef past)
  (let ((dg1 (dereference dg1-underef))
        (dg2 (dereference dg2-underef))) 
    (when (and (or (>= (node-timestamp dg1) past)
                   (>= (node-timestamp dg2) past))
               (not (= (node-timestamp dg1) (node-timestamp dg2))))
         (return-from dg-equal00 NIL))

    ;; when both nodes' timestamp < past or both have same timestamp
    (setf (node-timestamp dg1) (get-next-time))
    (setf (node-timestamp dg2) (node-timestamp dg1))

    (cond  ((and (atm-p dg1) (atm-p dg2))
                (atm-equal dg1 dg2))
           ((and (var-p dg1) (var-p dg2))
                T)
           ((and (cfs-p dg1) (cfs-p dg2))
                (let ((shared (arcs-intersection (node-arc-list dg1) (node-arc-list dg2)))
                      (flg T))
                    (unless (= (arcs-size shared) (arcs-size (node-arc-list dg1)))
                        (return-from dg-equal00 NIL))
                    (unless (= (arcs-size shared) (arcs-size (node-arc-list dg2)))
                        (return-from dg-equal00 NIL))
                    (arcs-foreach shared #'(lambda (s)
                        (let ((v1 (arc-value (cfs-arc dg1 (arc-label s))))
                              (v2 (arc-value (cfs-arc dg2 (arc-label s))))
                              (a1 (cfs-arc dg1 (arc-label s))))
                            ; node は何度か通過するかもしれないが、
                            ; どの arc も一度しか通過しない
                            (when (< (arc-timestamp a1) past)
                                (arc-set-timestamp a1 (get-current-time))
                                (setf flg (and flg (dg-equal00 v1 v2 past)))))))
                    flg))
           (t NIL)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which is informative
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dg-equal-or-more-informative-than (dg1 dg2)
    (let ((g-dg (generalize-dg dg1 dg2)))
        (dg-equal g-dg dg2)))

;; util ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapfor (ls f) (mapcar f ls))

(defun n-times-str (n str)
    (let ((nstr ""))
        (loop for i from 1 to n do (setf nstr (concatenate 'string str nstr)))
        nstr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; S-expression to node
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *s2n-tagged-nodes* '())

(defun s2n (s)
    (setf *s2n-tagged-nodes* '())
    (let ((result (s2n00 s)))
        (copy-dg-with-comp-arcs result)))

(defun s2n00 (s)
    (unless (listp s)
        (return-from s2n00 (var "S2N00-1")))

    (cond ((equal (nth 0 s) 'ATOM)
                ;(atm (string (nth 1 s))))
                (atm (nth 1 s)))

          ((equal (nth 0 s) 'VAR)
                (var (string (nth 1 s))))

          ((equal (nth 0 s) 'TAG)
             (let ((p (assoc (nth 1 s) *s2n-tagged-nodes*))
                   (v (var "S2N00-TAG")))
                (if (not p)
                   (push (cons (nth 1 s) v) *s2n-tagged-nodes*)
                   (setf v (dereference (cdr p))))
                (let ((n (s2n00 (nth 2 s))))
                   (setf (node-forward v) n)
                   n)))

           ((equal (nth 0 s) 'REF)
              (let ((p (assoc (nth 1 s) *s2n-tagged-nodes*))
                    (v (var "S2N00-REF")))
                 (when (not p)
                    (push (cons (nth 1 s) v) *s2n-tagged-nodes*)
                    (return-from s2n00 v))
              (dereference (cdr p))))

          ((string= (string (nth 0 s)) "[]")
                (let ((c (cfs)))
                    (mapfor (cdr s) #'(lambda (sub-s)
                        (arcs-add (node-arc-list c) (arc (string (nth 0 sub-s)) (s2n00 (nth 1 sub-s) )))))
                    c))
                    
          (t
                (var "S2N00-COND-T")))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node to S-expression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; atom や var の引数を数値か文字列か決める必要がある?
;;
(defparameter *n2s-reent-nodes* '())

(defun n2s (n)
    (setf *n2s-reent-nodes* '())
    (n2s00 n (get-next-time))
    (n2s01 n (get-next-time)))

(defun n2s00 (underef-dg past)
    (let ((dg (dereference underef-dg)))
        (when (>= (node-timestamp dg) past)
            (setf *n2s-reent-nodes* (append *n2s-reent-nodes* (list dg))))
        (setf (node-timestamp dg) (get-next-time))
        (when (cfs-p dg)
           (arcs-foreach (node-arc-list dg) #'(lambda (a)
                (when (< (arc-timestamp a) past)
                    (arc-set-timestamp a (get-next-time))
                    (n2s00 (arc-value a) past)))))))

(defun n2s01 (underef-dg past)
    (let ((dg (dereference underef-dg)))
        (if (member dg *n2s-reent-nodes*)
            (if (>= (node-timestamp dg) past)
                (list 'REF  (intern (string (node-id dg))))
                (progn
                    (setf (node-timestamp dg) (get-next-time))
                    (list 'TAG (intern (string (node-id dg))) (n2s02 dg past))))

            (n2s02 dg past))))

(defun n2s02 (dg past)
    ;(print "hai")
    ;(print (princ-to-string (node-name dg)))
    (cond ((atm-p dg) (list 'ATOM (node-name dg)))
          ((var-p dg) (list 'VAR  (princ-to-string (node-name dg))))
          ((cfs-p dg)
            (let ((as '()))
                (arcs-foreach (node-arc-list dg) #'(lambda (a)
                    (when (< (arc-timestamp a) past)
                        (arc-set-timestamp a (get-next-time))
                        (setf as (cons (list  (intern (arc-label a)) (n2s01 (arc-value a) past)) as)))))
                (setf as (cons '[] as))))
          (t '(UNDEF))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun assert-equal-s (s1 s2)
    (assert-equal-dg (s2n s1) (s2n s2)))

(defun assert-equal-dg (g1 g2)
    (format t "both nodes are same?")(terpri)
    (print-node g1)
    (print-node g2)
    (let ((are-same (dg-equal g1 g2)))
        (unless are-same
                (format t "ASSERT FAIL")(terpri)
                (exit))
            (format t "OK")(terpri)
            are-same))

(defun assert-unify-s (comment s1 s2 ans)
    (assert-unify-dg comment (s2n s1) (s2n s2) (s2n ans)))

(defun assert-unify-dg (comment g1 g2 ans)
    (format t "~A ###### assert unify #############################" comment)(terpri)
    (print-node g1)
    (print-node g2)
    (let ((result (unify-dg g1 g2)))
        (assert-equal-dg result ans)))

(defun assert-generalize-s (comment s1 s2 ans)
    (assert-generalize-dg comment (s2n s1) (s2n s2) (s2n ans)))

(defun assert-generalize-dg (comment g1 g2 ans)
    (format t "~A ###### assert generalize ########################" comment)(terpri)
    (print-node g1)
    (print-node g2)
    (let ((result (generalize-dg g1 g2)))
        (assert-equal-dg result ans)))

