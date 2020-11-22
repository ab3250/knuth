(use-modules (ice-9 textual-ports))
(include "core-statistics.scm")
(include "knuth-shuffle.scm")
(include "knuth-test-library.scm")
;;
(define deck (list #\A #\B #\C #\D #\E))
;;(define deck (list #\Q #\R))
(define perms (permutations deck))
(define perms-length (length perms))
(define iterations (* 100 perms-length))
(define hash-table (make-hash-table))

;messy
(define random-pool ((lambda()
   (let ((in-port (open-file "rand.data" "r")))
     (let loop ((data-list '()))
       (if (and(< (length data-list) 10000) (not(eof-object?(peek-char in-port))))
	   (let* ((data-char (get-char in-port))(data-number (char->integer data-char)))
	   (loop (if (char-numeric? data-char) (append data-list (list data-char)) data-list)))
	   (begin
	     (close-port in-port)
	     (delete 4 (map numerical-char->integer data-list) <))))))))

;(display random-pool)

(set! *random-state* (random-state-from-platform))

((lambda() ; init hash-table
  (let loop ((count 0))
    (if (not(eqv? count perms-length))
	(begin	  
	  (hash-set! hash-table  (car (list-tail perms count)) count)
	  (loop (+ count 1)))))))


((lambda () (let loop ((count 0) (c (make-list perms-length 0)))
  (if (< count iterations)
      (begin       
	(let ((val (knuth-shuffle (list-copy deck))))
	  (loop (+ count 1) (find-inc-indx (hash-ref hash-table val) c))))
      (begin
	(newline)
	(displayln "knuth shuffle")
	(newline)
	(displayln "permutation  occurrences")
	(map displayln (zip perms c))
	(newline)
	(displayln (list "deck" deck))
	(displayln (list "permutations" (length perms)))
	(displayln (list "iterations" iterations))
	(displayln (list "max" (apply max c)))
	(displayln (list "min" (apply min c)))
	(displayln (list "standard dev" (standard-deviation c)))
	(displayln (list "mean" (mean c)))
	(displayln (list "standard dev / mean" (/ (standard-deviation c)  (mean c))))
	(newline))))))







