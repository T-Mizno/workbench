(define (is-variable? x)
  (and (string? x) (char-upper-case? (string-ref (x->string x) 0))))

(define (toString t)
    (cond ((pair? t) (string-append "mkPair(e, " (toString (car t)) ", " (toString (cdr t)) ")"))
          ((list? t) "nil(e)")
          ((is-variable? t) (string-append "mkVar(e, \"" (x->string t) "\")"))
          (t (string-append "mkStr(e, \"" (x->string t) "\")")))
     )

 (define (toC t) (print (toString t)))

 (print "#include <stdio.h>")

 (print "#include \"prolog_dest.h\"")

(print  "#include <time.h>")
(print  "#include <sys/time.h>")
;(print  "#include <sys/resource.h>")
(print  "int main(void) { Env *e = newEnv();")

(print "    Term *db = ")
(toC '(
(("member" "Item" ("Item" . "Rest")))
(("member" "Item" ("X" . "Rest")) ("member" "Item" "Rest"))
((nextto "X" "Y" "List") (iright "X" "Y" "List"))
((nextto "X" "Y" "List") (iright "Y" "X" "List"))
((iright "L" "R" ("L" . ("R" . "Rest"))))
((iright "L" "R" ("X" . "Rest")) (iright "L" "R" "Rest"))
(("==" "X" "X"))
;;
((zebra "H" "W" "Z")
    ("==" "H" ((house norwegian "Xa" "Xb" "Xc" "Xd")
                "Xe"
                (house "Xf" "Xg" "Xh" milk "Xi") "Xj" "Xk"))
    (member (house englishman "Ya" "Yb" "Yc" red) "H")
    (member (house spaniard dog "Za" "Zb" "Zc") "H")
    (member (house "Aa" "Ab" "Ac" coffee green) "H")
    (member (house ukrainian "Ba" "Bb" tea "Bc") "H")
    (iright (house "Ca" "Cb" "Cc" "Cd" ivory)
            (house "Da" "Db" "Dc" "Dd" green) "H")
    (member (house "Ea" snails winston "Eb" "Ec") "H")
    (member (house "Fa" "Fb" kools "Fc" yellow) "H")
    (nextto (house "Ga" "Gb" chesterfield "Gc" "Gd")
            (house "Ha" fox "Hb" "Hc" "Hd") "H")
    (nextto (house "Ia" "Ib" kools "Ic" "Id")
            (house "Ja" horse "Jb" "Jc" "Jd") "H")
    (member (house "Ka" "Kb" luckystrike orangejuice "Kc") "H")
    (member (house japanese "La" parliaments "Lb" "Lc") "H")
    (nextto (house norwegian "Ma" "Mb" "Mc" "Md")
            (house "Na" "Nb" "Nc" "Nd" blue) "H")
    (member (house "W" "Oa" "Ob" water "Oc") "H")
    (member (house "Z" zebra "Qa" "Qb" "Qc") "H")
    )
))
(print ";")


(print "Term *q =")
(toC '(
(zebra "Houses" "WaterDrinker" "ZebraOwner")
;(== "X"  (1 w d  b a ))
))
(print ";")

(print "int solveResult = 1 != 1;")
(print "time_t timeStart, timeEnd;")
(print "double timeWhile;")


;(print "db = copyWithReduceVarRename(e, db);")
(print "q = copyWithReduceVarRename(e, q);")

(print "printf(\"DB\\n\");")
(print "stdoutDB(db);")
(print "printf(\"q : \"); stdoutTerm(q); printf(\"\\n\");")
(print "e->nProoves = 0; e->nGCs = 0;")
(print "timeStart = clock();")

;(print "e->mode = SEARCH_AND_DIALOG;")
(print "e->mode = SEARCH_ONE_SOLUTION;")
;(print "e->mode = COLLECT_ALL_SOLUTIONS;")

(print "initSolve(e, q, db); while(nextSolve(e, db, q,mkPair(e, db, mkPair(e,q, nil(e))))) {break; printf(\"query : \"); stdoutTerm(substBindings(e,q)); printf(\"\\n\");}")

;(print "solveResult = solve(e, q, db, 0, e->nil, q, mkPair(e, db, mkPair(e,q, nil(e))));")

(print "timeEnd = clock();")
(print "timeWhile = (double)(timeEnd - timeStart)/CLOCKS_PER_SEC;")
(print "printf(\"solve %d\\n\", solveResult);")
;(print "printf(\"after solve : \"); stdoutTerm(e->trail);printf(\"\\n\");")
(print "printf(\"%d prooves with %f sec = %f LIPS\\n\", e->nProoves, timeWhile, (double)(e->nProoves)/timeWhile);")

(print "printf(\"try to free env ...  \"); destroyNotReached(e, e->nil); printf(\"\\n\");")

(print "    freeEnv(e);");
(print "return 0;")

(print "}")
