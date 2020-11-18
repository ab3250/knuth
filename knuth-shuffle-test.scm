
(include "data.scm")

(set! *random-state* (random-state-from-platform))

(define hash-table (make-hash-table))

(display (hash-map->list cons hash-table))

((lambda()
  (let loop ((count 0))
    (if (not(eqv? count 120))
	(begin	  
	  (hash-set! hash-table  (car (list-tail data count)) count)
	  (loop (+ count 1)))))))

(define (find-inc-indx indx lst)
  (let loop ((lst2 '()) (count 0))
    (if (> count (-(length lst)1))
	(reverse lst2)
	(loop
      	 (append (list (+ (list-ref lst count) (if (eqv? count indx)1 0))) lst2) (+ count 1)))))

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
  (if (< count 120000000)
      (begin
	(let ((val (knuth-shuffle (list #\A #\B #\C #\D #\E))))
	  (loop (+ count 1) (find-inc-indx (hash-ref hash-table val) c))))
      (begin
	(newline)
	(display c)
	(newline))))



;|#


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


