(use-modules (ice-9 textual-ports)(srfi srfi-1))
(include "core-statistics.scm")
(include "knuth-shuffle.scm")
(include "knuth-test-library.scm")

;;
(define deck (list #\A #\B #\C #\D #\E))
;;(define deck (list #\Q #\R))
(define perms (permutations deck))
(define perms-length (length perms))
(define iterations (* 100 perms-length))

(set! *random-state* (random-state-from-platform))

(define hash-table ; init hash-table
  ((lambda() 
     (let ((htbl (make-hash-table)))
       (let loop ((count 0))
	 (if (eqv? count perms-length)
	     htbl
	     (begin
	       (hash-set! htbl (car (list-tail perms count)) count)
	       (loop (+ count 1)))))))))

((lambda () (let loop ((count 0) (c (make-list perms-length 0)))
  (if (< count iterations)
      (begin       
	(let ((val (knuth-shuffle (list-copy deck))))
	  (loop (+ count 1) (find-inc-indx (hash-ref hash-table val) c))))
      (print-results c)))))




