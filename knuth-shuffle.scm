(define (knuth-shuffle lst)
  (let loop ((count 0))    
    (if (> count (-(length lst)1))
	lst
	(let ((j (random (+ count 1)))
	      (tmp (car (list-tail lst count))))
	(list-set! lst count (car (list-tail lst j)))
	(list-set! lst j tmp)
	(loop (+ count 1))))))

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
