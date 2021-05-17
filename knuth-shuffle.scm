

;;function shuffle(array) {
;;  var m = array.length, t, i;
;;
;;  // While there remain elements to shuffle…
;;  while (m) {
;;
;;    // Pick a remaining element…
;;    i = Math.floor(Math.random() * m--);
;;
;;    // And swap it with the current element.
;;    t = array[m];
;;    array[m] = array[i];
;;    array[i] = t;
;;  }
;;  return array;
;;}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(define (knuth-shuffle lst-org)  
;;  (let loop ((count (length lst-org)) (lst lst-org))      
;;    (if (zero? count)
;;    	lst
;;	(let*   ((j (random count))
;;		 (new-count (- count 1))
;;	         (tmp (list-ref lst new-count))
;;	         (lst2 (list-set lst new-count (list-ref lst j)))
;;	         (lst3 (list-set lst2 j tmp)))
;;	         (loop new-count lst3)))))

(define (knuth-shuffle lst-org)  
  (let loop ((count (length lst-org)) (lst lst-org))      
    (if (zero? count)
    	lst
	(let*   ((j (random count))
		 (new-count (- count 1))
	         (tmp (list-ref lst new-count))
	         (lst2 (list-set lst new-count (list-ref lst j)))
	         (lst3 (list-set lst2 j tmp)))	         
	         (loop new-count lst3)))))




(define (list-set lst idx val)
  (if (null? lst)
    lst
    (cons
      (if (zero? idx)
        val
        (car lst))
      (list-set (cdr lst) (- idx 1) val))))


;;(newline)(display count)(newline)(display new-count)(newline)(display j)(newline)(display lst)(newline)(display lst)(newline)(display lst2)(newline)(display lst3)(newline)

;;
;;
;;(define (knuth-shuffle lst-org)
;;  (define lst (list-copy lst-org))
;;  (let loop ((count 0) (lst (list-copy lst-org)))    
;;    (if (> count (-(length lst)1))
;;	lst
;;	(let ((j (random (+ count 1)))
;;	      (tmp (car (list-tail lst count))))
;;	(loop (+ count 1) (list-set (list-set lst count (car (list-tail lst j))) j tmp)))))

;;(define (knuth-shuffle lst-org)
;;  (define lst (list-copy lst-org))
;;  (let loop ((count 0) (lst lst-org))    
;;    (if (> count (-(length lst)1))
;;	lst
;;	(let ((j (random (+ count 1))))
;;	(loop (+ count 1) (list-set (list-set lst count (car (list-tail lst j))) j (car (list-tail lst count))))))))

;;(define (knuth-shuffle lst-org)
;;  (let loop ((count 0) (lst lst-org))    
;;    (if (> count (-(length lst)1))
;;	lst
;;	(let ((j (random (+ count 1))))
;;	(loop (+ count 1) (list-set (list-set lst count (car (list-tail lst j))) j (car (list-tail lst count))))))))
