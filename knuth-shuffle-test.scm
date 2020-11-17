(set! *random-state* (random-state-from-platform))

(define-syntax rec
  (syntax-rules ()
    ((rec name value)
     (let ()
       (define name value)
       name))))

;;list-set! list k val
;; Macros are hygienic, you cannot clobber existing variables!
(define-syntax-rule (swap! x y) ; -! is idomatic for mutation
  (let ((tmp x))
    (list-set! x y)
    (list-set! y tmp)))

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
(let loop ((count 0))
  (if (< count 1000000)
      (begin
	(display (knuth-shuffle (list 'A 'B 'C 'D 'E)))
	(newline)
	(loop (+ count 1)))))
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


