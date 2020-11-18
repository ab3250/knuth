(include "data.scm")

(set! *random-state* (random-state-from-platform))


;;list-set! list k val

(define (find-inc-indx indx lst)
  (let loop ((lst2 '()) (count 0))
    (if (> count (-(length lst)1))
	(reverse lst2)
	(loop
      	 (append (list (+ (list-ref lst count) (if (eqv? count indx)1 0))) lst2) (+ count 1)))))

;(display (find-inc-indx 3 (find-inc-indx 3 '(0 0 0 0 0 0 0 0 0 0 0))))


(newline)

(define (knuth-shuffle lst)
  (let loop ((count 0))    
    (if (> count (-(length lst)1))
	lst
	(let ((j (random (+ count 1)))
	      (tmp (car (list-tail lst count))))
	(list-set! lst count (car (list-tail lst j)))
	(list-set! lst j tmp)
	(loop (+ count 1))))))



;#|
(let loop ((count 0) (c (make-list 120 0)))
  (if (< count 10000000)
      (begin
	(let ((val (knuth-shuffle (list 'A 'B 'C 'D 'E))))
	  (loop (+ count 1) (if (equal? val (list 'A 'B 'D 'E 'C)) (find-inc-indx 119 c)c))))
      (begin
	(newline)
	(display c)
	(newline))))



;|#
;(define lst (list 'A 'B 'C 'D 'E))
;(display(knuth-shuffle lst))
(newline)


;//knuth-shuffle recursive
;function shuffle2 (inputArray) {
;  //const loopCount = 0;  
;  (function loop(loopCount){
;      if ( loopCount === inputArray.length ) return 
;      const j = Math.floor(Math.random() * loopCount)
;      const temp = inputArray[loopCount]
;      inputArray[loopCount] = inputArray[j]
;      inputArray[j] = temp
;      loop(loopCount + 1)
;  })( 0 )
;  return inputArray
;}


