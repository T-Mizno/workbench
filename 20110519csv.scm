(use text.csv)
(use gauche.charconv)

(define (main args)
  (if (null? (cdr args))
       (print "required csv file")
       (begin
         (stdoutCSV (csvfile2list (car (cdr args))))
        )
  )
)

(define read-csv (make-csv-reader #\,))
(define (csvfile2list filename)
  (call-with-input-file filename (cut port->list read-csv <>) :encoding 'shift_jis)
)

(define (stdoutCSV ls)
  (for-each (lambda (l) (stdoutOneRow l)) ls)
)

(define (stdoutOneRow ls)
  (if (null? ls) 
       (format #t "\n") 
       (begin 
         (format #t "  ~10a" (ces-convert (car ls) 'utf-8 'shift_jis))
         (stdoutOneRow (cdr ls))
        )
   )
)
