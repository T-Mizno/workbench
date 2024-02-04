(load "./20240102-u.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(format t "~%test for primitive ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%")
(setf i00 (inconsistency))

(format t "~%dg-equal-or-more-informative-than ;;;;;;;;;;;;;;~%")
(setf i-g1 (s2n '([] (a (ATOM "hai"))
                     (b (ATOM "hoo"))
                     (c (ATOM "foo")))))

(setf i-g2 (s2n '([] (a (ATOM "hai"))
                     ;(b (ATOM "hoo"))
                     (c (VAR "foo")))))

(print-node i-g1)
(print-node i-g2)

(print (dg-equal-or-more-informative-than i-g1 i-g2))

(format t "~%example reentrancy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%")
; Generalizing the graphs, the original graph aproach cannot careit.
;
; mc1
; 0
;  -a-> 1
;  -b-> 2
;  -c-> 1
;
; mc2
; 0
;  -a-> 1
;  -b-> 2
;  -c-> 2
;
(setf rc1 (let ((a1 (atm 1)))
            ([] (-> "a" a1)
                (-> "b" (atm 2))
                (-> "c" a1))))

(setf rc11 (let ((a1 (atm 11)))
            ([] (-> "a" ([] (-> "aa"   a1)
                            (-> "ab"   a1))); (atm 12))))
                (-> "b" ([] (-> "bb"   (atm 22))
                            (-> "beta" (atm 777))))
                (-> "c" a1))))

(setf rc12 (let ((a1 (atm 12)))
            ([] (-> "a" ([] (-> "ab"   a1)
                            (-> "aa"   a1)))
                (-> "b" ([] (-> "beta" (atm 777))
                            (-> "bb"   (atm 777))))
                (-> "e" a1))))

(setf rc14 (let ((a1 (atm 12)))
            ([] (-> "a" a1)
                (-> "b" (atm 14))
                (-> "c" (atm 15)))))

(setf rc2 (let ((b2 (atm 2)))
            ([] (-> "a" (atm 1))
                (-> "b" b2)
                (-> "c" b2))))

(setf rc22 (let ((b2 (atm 22)))
            ([] (-> "a" (atm 12))
                (-> "b" b2)
                (-> "c" b2))))

(print-node rc11)
(print-node rc12)

(format t "unify :::::;;;;;;;;;;;;;;;~%")
(print-node (unify-dg rc11 rc12))

(format t "generalize ;;;;;;;;;;;;;;;~%")
(print-node (generalize-dg rc11 rc12))

(format t "~%example minimal cyclic ;;;;;;;;;;;;;;;;;;;;;;;;;~%")
; The original algorith cannot treat these graphs:
;
; mc1
; 0
;  -a-> 0
;  -b-> 1
;       -c-> 0
;  -d-> @x
;
; mc2
; 0
;  -a-> 0
;  -b-> 1
;       -c-> 1
;       -d-> @y
;
(setf mc1 (cfs))
(setf mc2 (cfs))

(arcs-add (node-arc-list mc1) (-> "a" mc1))
(arcs-add (node-arc-list mc1) (-> "b" ([] (-> "c" mc1))))
;(arcs-add (node-arc-list mc1) (-> "d" (atm "x"))) ; add to be inconsistency

(arcs-add (node-arc-list mc2) (-> "a" mc2))
(let ((mcb (cfs)))
    (arcs-add (node-arc-list mc2) (-> "b" mcb))
    (arcs-add (node-arc-list mcb) (-> "c" mcb))
    (arcs-add (node-arc-list mcb) (-> "d" (atm "y")))
)

(print-node mc1)
(print-node mc2)

(format t "unify :::::;;;;;;;;;;;;;;;~%")
(print-node (unify-dg mc1 mc2))

(format t "generalize ;;;;;;;;;;;;;;;~%")
;
; Todo: treat overflow
;
(print-node (generalize-dg mc1 mc2))
;(exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sample
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf primA00 '(ATOM "123"))
(setf primA01 '(ATOM "123"))
(setf primA02 '(ATOM "789"))
(setf primV00 '(VAR "345"))
(setf primC00 '([] (hai (ATOM "foo"))))
(setf primC01 '([] (delta (VAR "go"))))
(setf primC02 '([] (hai (VAR "went"))))
(setf primC03 '([] (hai (ATOM "foo"))))
(setf primI00 '(ATOM "INCONSISTENCY"))
(setf primR01 '([] (a (TAG A (ATOM "hai")))(b (REF A))))
(setf primR02 '([] (a (ATOM "hai"))(b (ATOM "hai"))))
(setf eqG1 '([] (a (ATOM "hai"))(b (ATOM "foo"))))
(setf eqG1Copy '([] (a (ATOM "hai"))(b (ATOM "foo"))))
(setf eqG2 '([] (a (TAG H (VAR "hei")))(b (REF H))))
(setf eqG2Copy '([] (a (REF I))(b (TAG I (VAR "hai")))))
(setf eqG3 '([] (a ([] (aa (ATOM "hoo"))))(b ([] (aa (ATOM "hoo"))))))
(setf eqG3Copy '([] (a ([] (aa (ATOM "hoo"))))(b ([] (aa (ATOM "hoo"))))))
(setf eqG4 '([] (a (TAG AA ([] (aa (ATOM "hoo"))(bb (VAR "fff")))))(b (REF AA))))
(setf eqG4Copy '([] (a (REF BB))(b (TAG BB ([] (aa (ATOM "hoo"))(bb (VAR "hei")))))))
(setf eqG5 '(TAG X ([] (z (REF X))(a (TAG AA ([] (aa (ATOM "hoo"))(bb (VAR "fff"))(cc (REF X)))))(b (REF AA))(c (ATOM "hoo")))))
(setf eqG5Copy '(TAG Y ([] (a (REF BB))(b (TAG BB ([] (aa (ATOM "hoo"))(bb (VAR "hei"))(cc (REF Y)))))(z (REF Y))(c (ATOM "hoo")))))
(setf failG1 '([] (a ([] (b (ATOM "test"))))))
(setf failG2 '([] (a (ATOM "ex"))))
(setf rc10 '([] (a (TAG a1 (ATOM "1")))(b (ATOM "2"))(c (REF a1))))
(setf rc11 '([] (a ([] (aa (TAG a1 (ATOM "11")))(ab (REF a1))))(b ([] (bb (ATOM "22"))(beta (ATOM "777"))))(c (REF a1))))
(setf rc12 '([] (a ([] (ab (TAG a1 (ATOM "12")))(aa (REF a1))))(b ([] (beta (ATOM "777"))(bb (ATOM "777"))))(e (REF a1))))
(setf rc14 '([] (a (ATOM "12"))(b (ATOM "14"))(c (ATOM "15"))))
(setf rc20 '([] (a (ATOM "1"))(b (TAG b2 (ATOM "2")))(c (REF b2))))
(setf rc22 '([] (a (ATOM "12"))(b (TAG b2 (ATOM "22")))(c (REF b2))))
(setf mc1 '(TAG mc1 ([] (a (REF mc1))(b ([] (c (REF mc1)))))))
(setf mc2 '(TAG mc2 ([] (a (REF mc2))(b (TAG mcb ([] (c (REF mcb))(d (ATOM "y"))))))))
(setf mcu '(TAG t ([] (a (REF t))(b (TAG m ([] (c (REF t))(d (TAG Y (ATOM "y"))))))(c (REF m))(d (REF Y)))))
(setf mcg '(TAG t ([] (a (REF t))(b ([] (c (VAR "New")))))))
(setf p24dg '([] (subject ([] (agreement (TAG X01 ([] (number (ATOM "singular"))(person (ATOM "third"))(gender (ATOM "feminine")))))))(predicate ([] (agreement (REF X01))))))
(setf p26dg1 '([] (born (TAG X01 (ATOM "Tokyo")))(home (REF X01))))
(setf p26dg2 '([] (born (ATOM "Tokyo"))(home (ATOM "Tokyo"))))
(setf p27dg1 '(VAR "[]"))
(setf p27dg2 '([] (category (VAR "[]"))))
(setf p28dg3 '([] (category (ATOM "N"))(agreement ([] (number (ATOM "singular"))))))
(setf p28dg4 '([] (category (ATOM "N"))(agreement ([] (number (ATOM "singular"))(person (ATOM "third"))))))
(setf p28dg5 '([] (category (ATOM "N"))(agreement ([] (number (ATOM "singular"))(person (ATOM "third"))))(subject ([] (number (ATOM "singular"))(person (ATOM "third"))))))
(setf p27dg6 '([] (category (ATOM "N"))(agreement (TAG X01 ([] (number (ATOM "singular"))(person (ATOM "third")))))(subject (REF X01))))
(setf p37dg1 '([] (a ([] (a (TAG X01 (VAR "X01")))))(b (REF X01))))
(setf p37dg2 '([] (a (TAG X02 (VAR "X02")))(b ([] (a (REF X02))))))
(setf p38dg3 '([] (a (TAG X01 ([] (a (REF X02)))))(b (TAG X02 ([] (a (REF X01)))))))
(setf p39dg1 '([] (a ([] (a (TAG X01 (VAR "[]")))))(b (REF X01))))
(setf p39dg2 '([] (a (TAG X02 (VAR "[]")))(b ([] (a (REF X02))))))
(setf p39dg3 '([] (a (TAG X01 ([] (a (REF X02)))))(b (TAG X02 ([] (a (REF X01)))))))
(setf p74dg1 '([] (a (ATOM "S"))(b (VAR "[]"))))
(setf p74dg2 '([] (a (TAG X (VAR "[]")))(b (REF X))(c (ATOM "t"))))
(setf p75dg '([] (a (TAG Y (ATOM "S")))(b (REF Y))(c (ATOM "t"))))
(setf p78dg1 '([] (a (ATOM "X"))(b (VAR "[]"))))
(setf p78dg2 '([] (a (VAR "[]"))(b ([] (d (ATOM "Y"))))(c (ATOM "Z"))))
(setf p79dg3 '([] (a (ATOM "X"))(b ([] (d (ATOM "Y"))))(c (ATOM "Z"))))
(setf p79dg '([] (a (VAR "V1"))(b (VAR "V2"))))
(setf p79dg1 '([] (a ([] (d (ATOM "X"))))(b (VAR "[]"))(c (ATOM "Y"))))
(setf p79dg2 '([] (a ([] (d (TAG X (ATOM "X")))(e (REF X))))(b ([] (f (ATOM "Z"))))(c (VAR "[]"))))
(setf p80dg3 '([] (a ([] (d (REF X))(e (TAG X (ATOM "X")))))(b ([] (f (ATOM "Z"))))(c (ATOM "Y"))))
(setf p80dg '([] (a ([] (d (ATOM "X"))))(b (VAR "[]"))(c (VAR "New"))))
(setf p81dg1 '([] (a (TAG dV ([] (d (TAG V (VAR "[]"))))))(b (REF dV))(c (REF V))))
(setf p81dg2 '(TAG X ([] (a (VAR "[]"))(b ([] (d (VAR "[]"))))(c ([] (e (REF X))))(f (ATOM "X")))))
(setf p82dg3 '(TAG START ([] (a (TAG M ([] (d (TAG BOTTOM ([] (e (REF START))))))))(b (REF M))(c (REF BOTTOM))(f (ATOM "X")))))
(setf p83dg1 '([] (a ([] (a (TAG V (VAR "[]")))))(b (REF V))))
(setf p83dg2 '([] (a (TAG V (VAR "[]")))(b ([] (a (REF V))))))
(setf p84dg3 '([] (a (TAG M ([] (a (TAG B ([] (a (REF M))))))))(b (REF B))))
(setf p112rule1 '([] (DTRS ([] (DTR1 ([] (SYN ([] (HEAD (TAG X05 (VAR "X05")))))))))(SYN ([] (HEAD (REF X05))))))
(setf p113rule2 '([] (DTRS ([] (DTR1 ([] (SYN ([] (HEAD ([] (COH (TAG X06 (VAR "X06")))))))))(DTR2 ([] (SYN ([] (SUBCAT ([] (FIRST (REF X06))(REST (TAG X10 (VAR "X10")))))))))))(SYN ([] (SUBCAT (REF X10))))))
(setf p113rule3 '([] (DTRS ([] (DTR1 ([] (SYN ([] (HEAD ([] (COH (TAG X06 (VAR "X06")))))))))(DTR2 (REF X06))))))
(setf p114dg1 '([] (DTRS ([] (DTR1 ([] (SYN ([] (HEAD (TAG X05 ([] (COH (TAG X06 ([] (SYN ([] (SUBCAT ([] (FIRST (REF X06))(REST (TAG X09 (VAR "[]")))))))))))))))))(DTR2 (REF X06))))(SYN ([] (HEAD (REF X05))(SUBCAT (REF X09))))))
(setf p114lex1 '([] (SYN ([] (HEAD ([] (AGR ([] (GEN (ATOM "FEM"))(NUM (ATOM "SING"))(PERS (ATOM "THIRD"))))(CASE (ATOM "-MINIATIVE"))(MAJ (ATOM "N"))(NFORM (ATOM "NORMAL"))(PRED (ATOM "MINUS"))))))))
(setf p114lex2 '([] (SYN ([] (HEAD ([] (AGR ([] (GEN (ATOM "FEM"))(NUM (ATOM "SING"))(PERS (ATOM "THIRD"))))(CASE (ATOM "OBJECTIVE"))(MAJ (ATOM "N"))(NFORM (ATOM "NORMAL"))(PRED (ATOM "MINUS"))))))))
(setf p115dg2 '([] (DTRS ([] (DTR2 (TAG X03 ([] (SYN ([] (SUBCAT ([] (REST (TAG X06 (VAR "[]")))(FIRST (REF X03)))))))))(DTR1 ([] (SYN ([] (HEAD (TAG X09 ([] (COH (REF X03))(PRED (ATOM "MINUS"))(NFORM (ATOM "NORMAL"))(MAJ (ATOM "N"))(CASE (ATOM "-MINIATIVE"))(AGR ([] (PERS (ATOM "THIRD"))(NUM (ATOM "SING"))(GEN (ATOM "FEM")))))))))))))(SYN ([] (SUBCAT (REF X06))(HEAD (REF X09))))))
(setf p115dg3 '([] (DTRS ([] (DTR2 (TAG X03 ([] (SYN ([] (SUBCAT ([] (REST (TAG X06 (VAR "[]")))(FIRST (REF X03)))))))))(DTR1 ([] (SYN ([] (HEAD (TAG X09 ([] (COH (REF X03))(PRED (ATOM "MINUS"))(NFORM (ATOM "NORMAL"))(MAJ (ATOM "N"))(CASE (ATOM "OBJECTIVE"))(AGR ([] (PERS (ATOM "THIRD"))(NUM (ATOM "SING"))(GEN (ATOM "FEM")))))))))))))(SYN ([] (SUBCAT (REF X06))(HEAD (REF X09))))))
(setf last '(ATOM "INCONSISTENCY"))

(assert-equal-s primA00 primA01)
;(not (assert-equal-s primA00 primA02))
(assert-equal-s primC00 primC03)
(assert-equal-s eqG1 eqG1Copy)
(assert-equal-s eqG2 eqG2Copy)
(assert-equal-s eqG3 eqG3Copy)
(assert-equal-s eqG4 eqG4Copy)
(assert-equal-s eqG5 eqG5Copy)
(assert-unify-s "minimal cycle" mc1 mc2 mcu)
(assert-generalize-s "minimal cycle" mc1    mc2    mcg)
(assert-generalize-s "p.26"          p26dg1 p26dg2 p26dg2)

(assert-unify-s "pp.37-38" p37dg1 p37dg2 p38dg3)
(assert-unify-s "p.39"     p39dg1 p39dg2 p39dg3)
(assert-unify-s "pp.74-75" p74dg1 p74dg2 p75dg)
(assert-unify-s "pp.78-79" p78dg1 p78dg2 p79dg3)
(assert-generalize-s "pp.78-79" p78dg1 p78dg2 p79dg)
(assert-unify-s "pp.79-80" p79dg1 p79dg2 p80dg3)
(assert-generalize-s "pp.79-80" p79dg1 p79dg2 p80dg)
(assert-unify-s "pp.81-82" p81dg1 p81dg2 p82dg3)
(assert-unify-s "pp.83-84" p83dg1 p83dg2 p84dg3)
(assert-unify-dg "pp.112-114" (unify-dg (s2n p112rule1) (s2n p113rule2))  (s2n p113rule3) (s2n p114dg1))
(assert-unify-s "pp.114-115-1" p114dg1  p114lex1 p115dg2)
(assert-unify-s "pp.114-115-2" p114lex2 p114dg1  p115dg3)

(assert-equal-dg (s2n (n2s (s2n p114lex2))) (s2n p114lex2))

(print "timestampe") (print (get-current-time))
(print "unify-global-counter") (print *unify-global-counter*)
(print "node size") (print (length *nodes*))

