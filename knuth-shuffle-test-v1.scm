

(set! *random-state* (random-state-from-platform))



(newline)

(define r 1)
(let loop ((count 0)(c (list 0 0 0 0 0 0 0 0 0 0)))
  (if (< count 10000000000000000)
      (begin  (set! r (random 10))
	      (set-car! (list-tail c r) (+ (car(list-tail c r)) 1))
	      (loop (+ count 1) c))
      (display c)))

(newline)
