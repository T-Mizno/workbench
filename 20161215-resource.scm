(require "./myzLinear.scm")
(import myzLinear)

(define (listIds ls) (upto 0 (- (length ls) 1)))

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

(define *C* (initResource 15 4))

(define (test itr)
    ;(if (> itr 10) (exit))
    ;(print *C*)
    (print itr " " (resource *C*) "sum " (apply + (resource *C*)))
    (if (nextResource *C*) (test (+ itr 1))))
(test 0)
