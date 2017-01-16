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
(("likes" "kim" "robin"))
(("likes" "sandy" "lee"))
(("likes" "sandy" "kim"))
(("likes" "robin" "cats"))
(("likes" "sandy" "X") ("likes" "X" "cats"))
(("likes" "kim" "X") ("likes" "X" "lee") ("likes" "X" "kim"))
(("likes" "X" "X"))
))
(print ";")


(print "Term *q =")
(toC '(("likes"  "sandy" "Who")))
;(toC '(("likes" "Who" "sandy")))
(print ";")

(print "int solveResult = 1 != 1;")
(print "time_t timeStart, timeEnd;")
(print "double timeWhile;")

(print "printf(\"DB\\n\");")
(print "stdoutDB(db);")
(print "printf(\"q : \"); stdoutTerm(q); printf(\"\\n\");")
(print "e->nProoves = 0; e->nGCs = 0;")
(print "timeStart = clock();")

(print "e->mode = SEARCH_AND_DIALOG;")
;(print "e->mode = SEARCH_ONE_SOLUTION;")

(print "initSolve(e, q, db); while(nextSolve(e, db, q,mkPair(e, db, mkPair(e,q, nil(e))))) {  printf(\"OUT query : \"); stdoutTerm(q); printf(\"\\n\");}")
;(print "solveResult = solve(e, q, db, 0, e->nil, q, mkPair(e, db, mkPair(e,q, nil(e))));")

(print "timeEnd = clock();")
(print "timeWhile = (double)(timeEnd - timeStart)/CLOCKS_PER_SEC;")
(print "printf(\"solve %d\\n\", solveResult);")
(print "printf(\"after solve : \"); stdoutTerm(e->trail);printf(\"\\n\");")
(print "printf(\"%d prooves with %f sec = %f LIPS\\n\", e->nProoves, timeWhile, (double)(e->nProoves)/timeWhile);")

(print "printf(\"try to free env ...  \"); destroyNotReached(e, e->nil); printf(\"\\n\");")

(print "    freeEnv(e);");
(print "return 0;")

(print "}")
