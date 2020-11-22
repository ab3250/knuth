;;knuth-test-library.scm
(use-modules (ice-9 textual-ports))
(define (permutations items)
;;
    (define (remove x lst)
    (cond
     ((null? lst) '())
     ((eqv? x (car lst))(remove x (cdr lst)))
     (else (cons (car lst) (remove x (cdr lst))))))
;;    
    (if (null? items) '(())
      (apply append
             (map (lambda (element)
            (map (lambda (permutation)
               (cons element permutation))
             (permutations (remove element items))))
          items))))

(define (zip list1 . more-lists) (apply map list list1 more-lists))


;;;;;;;;;;;;
(define (find-inc-indx indx lst)
  (let loop ((lst2 '()) (count 0))
    (if (> count (-(length lst)1))
	(reverse lst2)
	(loop
      	 (append (list (+ (list-ref lst count) (if (eqv? count indx)1 0))) lst2) (+ count 1)))))

(define (displayln x)
  (display x)(newline))
		   
(define (numerical-char->integer char)
  (let ([num (- (char->integer char) 48)]) ; 48 = (char->integer #\0)
    (if
     (or (< num 0) (> num 9))
     (raise 'non-numerical-char #t)
     num)))

;(define (string->integer str)
;  (let ([char-list (string->list str)])
;    (if (null? char-list)
;        (raise 'empty-string #t)
;        (foldl
;         (lambda([x : Integer] [y : Integer])
;           (+ (* y 10) x))
;         0
;         (map numerical-char->integer char-list)))))
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
