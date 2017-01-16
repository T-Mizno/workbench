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
;;
((csvFile "File" "Node") (file2cs "File" "Cs") (csv "Cs" "Node"))
((csv "C" ("R")) (record "C" "R"))
((csv "CCs" ("R" . "Rs")) (append "C" ("\\n" . "Cs") "CCs") (record "C" "R") (csv "Cs" "Rs"))
;((csv "CsLn" "F") (apppend "Cs" ("\\n" . ("\\r")) "CsLn") (csv "Cs" "F"))
((record ()()))
((record "C" ((new_string "F"))) (field "C" "F"))
((record "CcCs" ((new_string "F") . "Fs")) (append "C" ("," . "Cs") "CcCs") (field "C" "F") (record "Cs" "Fs"))
((field "C" "F") (escaped "C" "F"))
((field "C" "F") (nonEscaped "C" "F"))
((escaped ("\\\"" . "Csd") "E") (append "Cs" ("\\\"") "Csd") (qStr "Cs" "E"))
((qStr ()()))
((qStr ("\\\"" . ("\\\"" . "Cs")) ("\\\"" . "Qs")) (qStr "Cs" "Qs"))
((qStr ("C" . "Cs") ("Q" . "Qs")) (qTextData "C" "Q") (qStr "Cs" "Qs"))
((qTextData "C" "Q") (textData "C" "Q"))
((qTextData " " " "))
((qTextData "," ","))
((nonEscaped () ()))
((nonEscaped ("C" .  "Cs") ("E" . "Es")) (textData "C" "E") (nonEscaped "Cs" "Es"))
((textData "C" "C") (lowerAlphabet "C"))
((textData "C" "C") (upperAlphabet "C"))
((textData "C" "C") (digit "C"))
((textData " " " "))
((textData "C" "C") (member "C" ("!" "#" "$" "%" "&" "'" "(" ")" "-" "=" "~" "^" "|" "`" "{" "}" "[" "]" "+" ";" "*" ":" "<" ">" "." "/" "?" "_" "@")))
((dquote "\\\""))
;;
;;
(("member" "X" "Ys")("append" "As" ("X" . "Xs") "Ys")) ; TAP p.61
(("append" () "Ys" "Ys")) ; TAP p.60 3.15
(("append" ("X" . "Xs") "Ys" ("X" . "Zs")) ("append" "Xs" "Ys" "Zs"))
))
(print ";")

;(print "Term *q = mkPair(e, mkPair(e, mkStr(e, \"csvFile\"), mkPair(e, chars2List(e, \"  a, 1, 3, aab\\n \\\"h \\\"\\\"\\\"  \"), mkPair(e, mkVar(e, \"X\"), nil(e)))), nil(e));")
;(print "Term *q = mkPair(e, mkPair(e, mkStr(e, \"csv\"), mkPair(e, chars2List(e, \"a, b, c\\n 1\"), mkPair(e, mkVar(e, \"X\"), nil(e)))), nil(e));")
;(print "Term *q = mkPair(e, mkPair(e, mkStr(e, \"csvFile\"), mkPair(e, mkStr(e, \"tmp\"), mkPair(e, mkVar(e, \"X\"), nil(e)))), nil(e));")

(print "Term *q =")
(toC '( (csvFile tmp.csv "X")))
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
(print "e->mode = SEARCH_ONE_SOLUTION;")

(print "solveResult = solve(e, q, db, 0, e->nil, q, mkPair(e, db, mkPair(e,q, nil(e))));")

(print "timeEnd = clock();")
(print "timeWhile = (double)(timeEnd - timeStart)/CLOCKS_PER_SEC;")
(print "printf(\"solve %d\\n\", solveResult);")
;(print "printf(\"after solve : \"); stdoutTerm(e->trail);printf(\"\\n\");")
(print "printf(\"%d prooves with %f sec = %f LIPS\\n\", e->nProoves, timeWhile, (double)(e->nProoves)/timeWhile);")

(print "printf(\"try to free env ...  \"); destroyNotReached(e, e->nil); printf(\"\\n\");")

(print "    freeEnv(e);");
(print "return 0;")

(print "}")
