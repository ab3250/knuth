(use-modules (ice-9 textual-ports))

;((lambda()
;(let ((in-port (open-file "rand.data" "r")))
;   (let loop ((count 0))
;     (if (< count 15);
;	(begin;
;	  (display(get-char in-port))
;	  (newline)
;	  (loop (+ count 1)))))
;	(close-port in-port))))
(define (numerical-char->integer char)
  (let ([num (- (char->integer char) 48)]) ; 48 = (char->integer #\0)
    (if
     (or (< num 0) (> num 9))
     (raise 'non-numerical-char #t)
     num)))

(define delete
  (case-lambda
    ((x lis)
     (delete x lis equal?))
    ((x lis elt=)
     (let recur ((lis lis))
       (if (eqv? lis '()) lis
           (let ((head (car lis))
                 (tail (cdr lis)))
             (if (not (elt= x head))
                 (let ((new-tail (recur tail)))
                   (if (eq? tail new-tail) lis
                       (cons head new-tail)))
                 (recur tail))))))))

;;; Extended from R4RS to take an optional comparison argument.
(define member
  (case-lambda
    ((x lis)
     (r6rs:member x lis))
    ((x lis elt=)
     (let lp ((lis lis))
       (and (not (eqv? lis '()))
            (if (elt= x (car lis)) lis
                (lp (cdr lis))))))))


#|
;messy
(define random-pool ((lambda()
   (let ((in-port (open-file "rand.data" "r")))
     (let loop ((data-list '()))
       (if (and(< (length data-list) 100) (not(eof-object?(peek-char in-port))))
	   (let* ((data-char (get-char in-port))(data-number (char->integer data-char)))
	   (loop (if (char-numeric? data-char) (append data-list (list data-char)) data-list)))
	   (begin
	     (close-port in-port)
	     (filter-map
	      (lambda(x)
		(let ((val (string->number (string x)  16)))
		  (if(< val 5) val #f)))
	      data-list))))))))

;;;;	     (delete 4 (map (lambda(x)(string->number (string x)  16))  data-list) <))))))))

;(display random-pool)
|#


;
