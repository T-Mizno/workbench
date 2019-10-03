(require "./myzLinear.scm")
(import myzLinear)


(use util.combinations)

(define (zero? x) (< (abs x) 0.0001))
(define (close? x y) (zero? (abs (- x y))))
(define (existZero? ls)
    (cond ((null? ls) #f)
          ((zero? (car ls)) #t)
          (#t (existZero? (cdr ls)))))
(define (existZeroInStanding? mat)
    (existZero? (map (lambda (id) (if (equal? (id-i id) (id-j id)) 1  (at@ mat id))) (ids mat)))
)

;;
;;  ns preferences   represents prorile
:;


;; m is # of alternatives
(define (allPreferences m)
  (permutations (upto 0 (- m 1)))
)

(define (printPreference p)
    (format #t " ")
    (for (listIds p)
        (lambda (j)
            (if (> j 0) (format #t " > "))
            (format #t "X~D" (list-ref p j))
            ))
    (print))

(define (printProfile ns preferences)
    (for (listIds preferences)
      (lambda (i)
        (if (> (list-ref ns i) 0)
            (begin
                (format #t "~2D voters think :" (list-ref ns i))
                (printPreference (list-ref preferences i))
                ))))
    (print "There are " (length preferences) " possible preferences."))

(define (majorityCounts m ns preferences)
    (let ((result (make-list m 0)))
        (for (listIds preferences)
            (lambda (pid) (list-set! result (car (list-ref preferences pid)) (+ (list-ref ns pid) (list-ref result (car (list-ref preferences pid)))))))
        result))

(define (bordaCounts m ns preferences)
  (let ((result (make-list m 0)))
       (for (listIds preferences)
          (lambda (i)
              (for (upto 0 (- m 1))
                 (lambda (ith)
                   (let ((ithAlternative (list-ref (list-ref preferences i) ith)))
                     (list-set! result ithAlternative (+ (list-ref result ithAlternative) (* (list-ref ns i) (- m (+ ith 1))))))))))
        result))

(define (bordaCounts2 standing)
   (map (lambda (i) (apply + (map (lambda (j) (at standing i j)) (colIds standing))))
        (rowIds standing)
   ))

(define (eigenVector mat)
    (let ((result (powerMethod mat 100)))
       (map (lambda (i) (at (car (cdr result)) i 0)) (upto 0 (- (M mat) 1))))
)

(define (geomMean mat)
    (map (lambda (i) (expt (apply * (map (lambda (j) (at mat i j)) (colIds mat))) (/ 1 (M mat))))
         (rowIds mat)
    )
)

(define (normalize ls)
    (let ((deno (apply + ls)))
        (map (lambda (l) (+ 0.0 (/ l deno))) ls)))

(define (printBorda bc)
   (for (upto 0 (- (length bc) 1))
      (lambda (i) (format #t "  [X~D]/~5D" i (list-ref bc i))))
    (print))

(define (prefer i j preference)
  (cond ((null? preference) #f)
        ((equal? (car preference) i) #t)
        ((equal? (car preference) j) #f)
        (#t (prefer i j (cdr preference)))))

(define (numOfPrefer i j ns preferences)
    (apply +  (map (lambda (pid)
               (if (and (> (list-ref ns pid) 0) (prefer i j (list-ref preferences pid)) (not (equal? i j)))
                 (list-ref ns pid)
                 0))
              (listIds preferences))))

(define (makeStanding m ns preferences)
  (let ((mat (newMatrix 0 (- m 1) 0 (- m 1))))
     (matFor mat
        (lambda (aMat i j) (set aMat i j (numOfPrefer i j ns preferences))))
    mat))


(define (makeDefaultPCM standing)
   (matMap@ (lambda (id val)
                (if (equal? (id-i id) (id-j id))
                    1
                    (/ val (at standing (id-j id) (id-i id)))))
                standing))

(define (makeBPCM standing)
    (matMap@ (lambda (id val)
                (if (equal? (id-i id) (id-j id))
                    1/2
                    (/ val (+ val (at standing (id-j id) (id-i id))))))
                standing))

(define (makeNPCM standing)
    (matMap@ (lambda (id val) (/ (+ 1 val) (+  1 (at standing (id-j id) (id-i id))))) standing)
)

(define (makeEPCM n standing)
    (matMap@ (lambda (id val) (exp (/ (- val (at standing (id-j id) (id-i id))) n))) standing)
)

(define (makeXt preference)
    (let ((Xt (newMatrix 0 (- (length preference) 1) 0 (- (length preference) 1))))
        (matMap@ (lambda (id val)
                    (if (prefer (id-i id) (id-j id) preference)
                        1
                        -1
                        ))
                Xt)))

(define (kemenyDot standing preference)
    (let* ((Xt (makeXt preference))
           (mat (matMap@ (lambda (id val) (* (at@ standing id) (at@ Xt id))) Xt)))
         ;(stdout Xt)
        (apply + (map (lambda (i) (apply + (map (lambda (j) (at mat i j)) (colIds mat)))) (rowIds mat)))))

(define (kemenyFunction standing ns preferences)
    (let* ( (result '())
            (kemenyDs (map
                        (lambda (i)
                                (kemenyDot standing (list-ref preferences i))
                                )
                        (listIds preferences)))
            (maxDs (apply max kemenyDs)))
        (for (listIds kemenyDs)
            (lambda (i)
                (if (equal? maxDs (list-ref kemenyDs i))
                    (begin
                        (set! result (cons (cons maxDs (list-ref preferences i)) result))
                        ;(format #t "~3D : " (list-ref kemenyDs i))
                        ;(printPreference (preferenceOfVotedPreference (list-ref profile i)))
                        ))
            ))
    result
    )
)

(define (compFunc x y)
    (cond ((close? x y) 0)
          ((> x y) 1)
          (#t -1)))

(define (rankDiff as bs)
    (apply +
        (map (lambda (i)
                (apply +
                    (map (lambda (j)
                            ;(print (compFunc (list-ref as i) (list-ref as j))  (compFunc (list-ref bs i) (list-ref bs j)))
                            (abs (- (compFunc (list-ref as i) (list-ref as j))  (compFunc (list-ref bs i) (list-ref bs j)))))
                        (upto 0 (- (length as) 1)))))
            (upto 0 (- (length as) 1)))))

(define (printKemenyResult vps)
    (for vps
      (lambda (vp)
        (format #t "maxD=~3D : " (car vp))
         (printPreference (cdr vp)))))

(define (preference2Rate pre)
    (let ((rate (make-list (length pre) 0)))
        (for (upto 0 (- (length pre) 1))
            (lambda (i) (list-set! rate (list-ref pre i) (- (length pre) i))))
        rate))

(define (existSameInKemeny? borda resultKemeny)
    (if (null? resultKemeny)
        #f
        (let ((rate (preference2Rate (cdr (car resultKemeny)))))
            (if (equal? (rankDiff borda rate) 0)
                    #t
                    (existSameInKemeny? borda (cdr resultKemeny))))))

(define (kemenyTops resultKemeny)
    (delete-duplicates (map (lambda (r) (car (cdr r))) resultKemeny))
)

(define (maxId r)
    (let ((maxId 0)
          (tmpMax (list-ref r 0)))
          (for (upto 0 (- (length r) 1))
            (lambda (i) (if (> (list-ref r i) (list-ref r maxId))
                            (set! maxId i))))
            maxId))
(define (existSameTop borda resultKemeny)
    (let ((i (maxId borda)))
        (existTopI i resultKemeny)))

(define (existTopI i resultKemeny)
        (cond ((null? resultKemeny) #f)
              ((equal? i (car (cdr (car resultKemeny)))) #t)
              (#t (existTopI i (cdr resultKemeny)))))

(define (tops bs)
    (let ((tmpMax (apply max bs)))
        (filter (lambda (i) (close? tmpMax (list-ref bs i))) (listIds bs))))
(define (existTopsInKemeny bs resultKemeny)
    (> (length (listIntersection (tops bs) (kemenyTops resultKemeny) )) 0))
(define (existSameTops as bs)
    (> (length (listIntersection (tops as) (tops bs))) 0)
    )
(define (listIntersection as bs)
   (lstInts as bs '())
)
(define (lstInts as bs result)
    (cond ((equal? bs '()) result)
          ((member (car bs) as) (lstInts as (cdr bs) (cons (car bs) result)) )
          (#t (lstInts as (cdr bs) result))))

(define (listIds ls) (upto 0 (- (length ls) 1)))

(define (isCondorcetWinner i npcm)
  (let ((js (colIds npcm)))
       (every (lambda (j) (if (eq? i j) #t (> (at npcm i j) 1))) js))
)
(define (existsCondorcetWinner npcm)
   (any (lambda (i) (isCondorcetWinner i npcm)) (rowIds npcm)))

(define (selectCondorcetWinner bs npcm)
   (any (lambda (i) (isCondorcetWinner i npcm)) (tops bs))
)

;; return (C n k tmp)
(define (initCombination n k) (list (upto 0 (+ n 1)) n  k (make-list (+ n 2) 0)))

(define (nextCombination C)
    (let ((r (list-ref C 0))
            (n (list-ref C 1))
            (k (list-ref C 2))
            (tmp (list-ref C 3))
            (J 0)
            )
        (for (upto 0 n) (lambda (i) (list-set! tmp (+ i 1) (+ (list-ref r i) 1))))
        (set! J (apply max (cons 0 (filter (lambda (i) (not (equal? (list-ref tmp i) (+ (- n k) i)))) (upto 0 k)))))
        (if (<= J 0)
            #f
            (begin
                (list-set! tmp J (+ (list-ref tmp J) 1))
                (for (upto (+ J 1) k) (lambda (j) (list-set! tmp j (+ (list-ref tmp (- j 1)) 1))))
                (for (upto 0 n) (lambda (i) (list-set! r i (- (list-ref tmp (+ i 1)) 1))))
                #t
            )
            )))
(define (combination C) (take (list-ref C 0) (list-ref C 2)))

(define (comb2Resource CR)
    (let* ((comb (combination (car CR)))
            (R (resource CR))
            (n (+ (length comb) 1))
            (N (list-ref (car CR) 1))
            )
        ;(print "comb " comb ", n " n ", N " N " " CR)
        (list-set! R 0 (list-ref comb 0))
        (for (upto 1 (- n 2)) (lambda (i) (list-set! R i (- (- (list-ref comb i) (list-ref comb (- i 1))) 1))))
        (list-set! R  (- n 1) (- (- N 1) (last comb)))
        )
)
(define (initResource N n)
    (let* ((C (initCombination (- (+ N n) 1) (- n 1)))
           (CR (cons C (make-list n 0))))
        (comb2Resource CR)
        CR
        ))

(define (resource CR) (cdr CR))

(define (nextResource CR)
    (if (nextCombination (car CR))
        (comb2Resource CR)
        #f)
)


(define (incCounts counts i)
    (list-set! counts i (+ (list-ref counts i) 1))
)

(define (testb m n ns preferences counts)
  (let*
      ((standing (makeStanding m ns preferences))
       (majority (majorityCounts m ns preferences))
       (borda (bordaCounts m ns preferences))
       (borda2 (bordaCounts2 standing))
       ;(pcm (makeDefaultPCM standing))
       ;(resultPCM (normalize (eigenVector pcm)))
       (bpcm (makeBPCM standing))
       (resultBPCM (normalize (eigenVector bpcm)))
       (npcm (makeNPCM standing))
       (resultNPCM (normalize (eigenVector npcm)))
       (epcm (makeEPCM n standing))
       (resultEPCM (normalize (eigenVector epcm)))
       (resultKemeny (kemenyFunction standing ns preferences))
       (flgs (make-list 25 #f))
       )
    ;; vs majority
    (if (not (existSameTops majority borda2)) (list-set! flgs 0 #t))
    (if (not (existSameTops majority resultNPCM)) (list-set! flgs 1 #t))
    (if (not (existSameTops majority resultBPCM)) (list-set! flgs 2 #t))
    (if (not (existTopsInKemeny majority resultKemeny)) (list-set! flgs 3 #t))
    ;; vs Borda counts
    (if (not (existSameTops borda2 resultNPCM)) (list-set! flgs 4 #t))
    (if (not (existSameTops borda2 resultBPCM)) (list-set! flgs 5 #t))
    (if (not (existTopsInKemeny borda2 resultKemeny)) (list-set! flgs 6 #t))
    ;; vs Kemeny function
    (if (not (existSameTops resultNPCM resultBPCM)) (list-set! flgs 7 #t))
    (if (not (existTopsInKemeny resultNPCM resultKemeny)) (list-set! flgs 8 #t))
    (if (not (existTopsInKemeny resultBPCM resultKemeny)) (list-set! flgs 9 #t))
    ;; Majority Borda Kemeny are different
    (if (and (list-ref flgs 0) (list-ref flgs 6) (list-ref flgs 3)) (list-set! flgs 10 #t))
    ;; other info
    (if (>= (length (tops majority)) 2) (list-set! flgs 11 #t))
    (if (>= (length (tops borda2)) 2) (list-set! flgs 12 #t))
    (if (>= (length (tops resultNPCM)) 2) (list-set! flgs 13 #t))
    (if (>= (length (tops resultBPCM)) 2) (list-set! flgs 14 #t))
    (if (>= (length (kemenyTops resultKemeny)) 2) (list-set! flgs 15 #t))
    (if (>= (length (kemenyTops resultKemeny)) 3) (list-set! flgs 16 #t))
    (if (>= (length (kemenyTops resultKemeny)) 4) (list-set! flgs 17 #t))
    ;(if (existsCondorcetWinner npcm) (list-set! flgs 18 #t))
    ;(if (and (list-ref flgs 18) (not (selectCondorcetWinner borda2 npcm))) (list-set! flgs 19 #t))
    ;(if (and (list-ref flgs 18) (not (selectCondorcetWinner resultNPCM npcm))) (list-set! flgs 20 #t))
    ;(if (and (list-ref flgs 18) (not (list-ref flgs 19)) (list-ref flgs 20)) (list-set! flgs 21 #t))
    ;;

    (for (listIds flgs) (lambda (i) (if (list-ref flgs i) (incCounts counts i))))

    (if (and
            #t
            ;(list-ref flgs 10)
            ;(list-ref flgs 15)
            (and  (not(selectCondorcetWinner resultNPCM npcm))  (selectCondorcetWinner borda2 npcm))
        )
        (begin
            (print "##################################################")
            ;(print "Different: Top of NPCM's result and Top of Kemeny function.")
            ;(print "Different: Alt. with highest Borda count and Top of Kemeny function.")

            (print "Profile: # of alternative is " m ",  # of voters is " n)
            (printProfile ns preferences)
            (print "Majority counts:")
            (printBorda majority)
            (print "Standing Table")
            (stdout standing)
            (print "Borda counts: ")
            ;(printBorda borda)
            (printBorda borda2)
            (printBorda (normalize borda2))
            ;(print "Default Pairwise comparison matrix:")
            ;(stdout pcm)
            ;(printBorda resultPCM)
            (print "N PCM a_ij = (1+|ij|)/(1+|ji|)")
            (stdout npcm)
            (printBorda resultNPCM)
            (print "B Robust PCM by Lipovetsky")
            (stdout bpcm)
            (printBorda resultBPCM)
        ;    (printBorda resultNPCM)
        ;    (print "E PCM exp")
        ;    (stdout epcm)
        ;    (printBorda resultEPCM)

            (print "Kemeny Function: ")
            (printKemenyResult resultKemeny)
            (print (tops majority))
            (print (kemenyTops resultKemeny))
            (print (existTopsInKemeny majority resultKemeny))
            ;(printBorda (preference2Rate (cdr (car resultKemeny))))
            (print)(print)(print)
            (exit)
            #t
            )
            #f
            )
    )
)


(define (testc m n ns preferences)
    (print ns)
    (printProfile ns preferences)
  (let* ((standing (makeStanding m ns preferences)))
    (stdout standing)
    )
    )

;; m is # of alternatives
;; n is # of voters
(define (test m n)
    (let* ((preferences (allPreferences m))
           (CR (initResource n (length preferences)))
           )
           (inTest m n CR preferences 1 (make-list 25 0))))

(define (inTest m n CR preferences itrCount strangeCounts)
    (let ((ns (resource CR)))
        ;(print CR)
        (print "m=" m ", n=" n ", checking " itrCount  "th profile.")
        ;(printProfile ns preferences)
        ;(print ns (listIds ns))
        (testb m n ns preferences strangeCounts)
        (if (nextResource CR)
            (inTest m n CR preferences (+ itrCount 1) strangeCounts)
            (begin   ;; all profile checked
                (list-set! strangeCounts 22 m)
                (list-set! strangeCounts 23 n)
                (list-set! strangeCounts 24 itrCount)
                (print "End: checked " itrCount " profiles. Extra counts: " strangeCounts)
                (let ((fp (open-output-file "Bordaout.txt" :if-exists :append)))
                    (write strangeCounts fp)
                    (close-output-port fp))
            )
        )))


;(define (printEachLs ls)    (for-each (lambda (l) (format #t ", ~D" l) ) ls)(print))
;(for-each (lambda (ls) (printEachLs ls)) results)

(for (upto 5 10)    (lambda (n) (test 4 n)))
