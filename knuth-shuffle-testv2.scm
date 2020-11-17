(define (find-inc-indx indx lst)
  (let loop ((lst2 '()) (count 0))
    (if (> count (-(length lst)1))
	lst2
	(begin
	  (if (eqv? count indx)
      	      (begin(display(append (list (+ (list-ref lst count) 1)) lst2))(newline))
	      (begin(display(append (list (list-ref lst count)) lst2))(newline)))
	  (loop lst2 (+ count 1))))))

(display (find-inc-indx 3 '(0 0 0 0 0 0 0)))

(set! *random-state* (random-state-from-platform))



(newline)

;(define r 1)
;(let loop ((count 0)(c (list 0 0 0 0 0 0 0 0 0 0)))
;  (if (< count 1000000)
;      (begin       
;	      (loop (+ count 1) (find-inc-indx (random 10) c)))
;      (display c)))
;
;(newline)

;(set-car! (list-tail c r) (+ (list-ref c r) 1))
