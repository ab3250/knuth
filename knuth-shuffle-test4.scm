(set! *random-state* (random-state-from-platform))

(define (find-inc-indx indx lst)
  (let loop ((lst2 '()) (count 0))
    (if (> count (-(length lst)1))
	(reverse lst2)
	(loop
      	 (append (list (+ (list-ref lst count) (if (eqv? count indx)1 0))) lst2) (+ count 1)))))

;(display (find-inc-indx 3 (find-inc-indx 3 '(0 0 0 0 0 0 0 0 0 0 0))))




(newline)






(let loop ((count 0)(c (list 0 0 0 0 0 0 0 0 0 0)))
  (if (< count 1000000)
      (begin       
	      (loop (+ count 1) (find-inc-indx (random 10) c)))
      (display c)))

(newline)

;(set-car! (list-tail c r) (+ (list-ref c r) 1))
