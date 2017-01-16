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
(("member2" "X" "Ys")("append" "As" ("X" . "Xs") "Ys")) ; TAP p.61
(("append" () "Ys" "Ys")) ; TAP p.60 3.15
(("append" ("X" . "Xs") "Ys" ("X" . "Zs")) ("append" "Xs" "Ys" "Zs"))
(("prefix" "Xs" "Ys")("append" "Xs" "As" "Ys")) ; TAP p.61
(("suffix" "Xs" "Ys")("append" "As" "Xs" "Ys")) ; TAP p.61
(("reverse" () ())) ; TAP p.61 3.16
(("reverse" ("X" . "Xs") "Zs") ("reverse" "Xs" "Ys") ("append" "Ys" ("X" . ()) "Zs"))
(("sublist" "Xs" "Ys")("prefix" "Ps" "Ys")("suffix" "Xs" "Ps")) ; TAP p.60 3.14 a
(("length" () "Zero") (str2Int "0" "Zero"))
(("length" ("X" . "Xs") "L1") ("length" "Xs" "L") ("inc" "L" "L1"))
(("sumlist" () "Zero") (str2Int "0" "Zero")) ;; TAP p.157 8.6a
(("sumlist" ("Istr" . "Is") "Sum") ("sumlist" "Is" "IsSum") (str2Int "Istr" "I") ("add" "I" "IsSum" "Sum"))
(("prodlist" () "One") (str2Int "1" "One"))
(("prodlist" ("Istr" . "Is") "Prod") ("prodlist" "Is" "IsProd") (str2Int "Istr" "I") ("multi" "I" "IsProd" "Prod"))
(("factorial" "N" "F") ("factorial" "N" "1" "F"))
(("factorial" "0" "F" "F")) ; TAP p.156 8.4
(("factorial" "N" "T" "F")(">" "N" "0") ("multi" "T" "N" "T1") ("sub" "N" "1" "N1") ("factorial" "N1" "T1" "F"))
(("factorial2" "N" "F") (">=" "N" "1") ("rangelist" "1" "N" "L") ("prodlist" "L" "F"))
(("between" "Istr" "Jstr" "K") (str2Int "Istr" "I") (str2Int "Jstr" "J") ("_between" "I" "J" "K"))
(("_between" "I" "I" ("I")))  ; TAP p.157 8.5
(("_between" "I" "J" ("I" ."Ks")) ("<" "I" "J") ("inc" "I" "I1") ("_between" "I1" "J" "Ks"))
(("gcd" "Istr" "0" "I") (str2Int "Istr" "I")) ; TAB p.152 8.2
(("gcd" "Istr" "Jstr" "Gcd") (str2Int "Istr" "I") (str2Int "Jstr" "J") (str2Int "0" "Zero") ("<" "Zero" "J") ("mod" "I" "J" "R")("gcd" "J" "R" "Gcd"))
(("reverse2" () ())) ;p.388
(("reverse2" ("X" . "A") "B") ("reverse2" "A" "C") (concat "C" ("X") "B"))
((concat () "L" "L"))
((concat ("X" . "A") "B" ("X" . "C")) (concat "A" "B" "C"))
(("ireverse" "L" "R") ("irev3" "L" () "R"))
(("irev3" ("X" . "L") "SoFar" "R") ("irev3" "L" ("X" . "SoFar") "R"))
(("irev3" () "R" "R"))
))
(print ";")


(print "Term *q =")
(toC '(("reverse2" (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100) "X")))
;(toC '(("reverse2" (1 2 3  ) "X")))
;(toC '((member "X" (1a 2b 3c 444 555 666))))
;(toC '((length (1a 2b 3c 4dd 5ee 6ff gg7 e8 ff9) "L")))
;(toC '((sumlist (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100) "S")))
;(toC '((between 1 20 "L")  (reverse2 "L" "R")))
;(toC '((prodlist () "P")))
;(toC '((prodlist (1 2 3 4 5 6 7 8 9 10) "P")))
;(toC '((between 1 10 "L")  (prodlist "L" "Sum")))
;(toC '((gcd 24 6 "L")))
(print ";")

(print "int solveResult = 1 != 1;")
(print "time_t timeStart, timeEnd;")
(print "double timeWhile;")

(print "printf(\"DB\\n\");")
(print "stdoutDB(db);")
(print "printf(\"q : \"); stdoutTerm(q); printf(\"\\n\");")
(print "e->nProoves = 0; e->nGCs = 0;")
(print "timeStart = clock();")

;(print "e->mode = SEARCH_AND_DIALOG;")
;(print "e->mode = SEARCH_ONE_SOLUTION;")
(print "e->mode = COLLECT_ALL_SOLUTIONS;")

(print "q = copyWithReduceVarRename(e, q);")
(print "initSolve(e, q, db); while(nextSolve(e, db, q,mkPair(e, db, mkPair(e,q, nil(e))))) {   printf(\"query : \"); stdoutTerm(substBindings(e,q)); printf(\"\\n\"); }")
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
