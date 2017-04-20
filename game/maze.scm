(load "../struct/stackfp.scm")
(load "../struct/for.scm")

;;; an input func with a tip
(define (prompt tip)
  (display tip)
  (newline)
  (read))

;;; make a sequence like '(1 2 3 4 5 ... n)
(define (make-sequence n)
  (define tmp (make-vector n))
  (do
    ((i 0 (+ i 1)))
    ((= i n))
    (vector-set! tmp i i))
  (vector->list tmp))

;;; the length of the maze square
(define len 0)

;;; make a maze . a maze should let 0s as path and 1s as wall, the top left corner and the opposite shoule be 0.
;;; it seems like below : 
;;; 0 0 0 1
;;; 1 1 0 1
;;; 1 1 0 0
;;; 1 1 1 0
(define (make-maze)
  (set! len (prompt "Input length : "))
  (display "Input the maze : ")
  (newline)
  (define maze (make-vector len))
  (for (i in (make-sequence len))
       (begin
	 (vector-set! maze i (make-vector len))
	 (for (j in (make-sequence len))
	      (vector-set! (vector-ref maze i) j (read))
	      )
	 ))
  maze)

(define maze (make-maze))

;;; show the maze
(for (i in (make-sequence len))
     (begin
       (display (vector-ref maze i))
       (newline)))

;;; packing the operation of get maze's position
(define (getxy mz x y)
  (vector-ref (vector-ref maze x) y))

;;; find the path through the maze
(define (find-path mz)
  (define path '())
  (do
    ((i 0 (+ i 0)) (j 0 (+ j 0)))
    ((and (= i (- len 1)) (= j (- len 1))) 
     (begin
       (push path (cons i j))
       (for-each
	 (lambda (x)
	   (display x)
	   (newline))
	 (reverse path))))
    (cond
      ;;; walk down
      ((and (> len (+ i 1)) (= 0 (getxy mz (+ i 1) j)))
       (begin
	 (push path (cons i j))
	 ;;; path and mark
	 (vector-set! (vector-ref maze i) j -1)
	 (set! i (+ i 1))
	 ))
      ;;; walk right
      ((and (> len (+ j 1)) (= 0 (getxy mz i (+ j 1))))
       (begin
	 (push path (cons i j))
	 (vector-set! (vector-ref maze i) j -1)
	 (set! j (+ j 1))
	 ))
      ;;; walk up
      ((and (<= 0 (- i 1)) (= 0 (getxy mz (- i 1) j)))
       (begin
	 (push path (cons i j))
	 (vector-set! (vector-ref maze i) j -1)
	 (set! i (- i 1))
	 ))
      ;;; walk left
      ((and (<= 0 (- j 1)) (= 0 (getxy mz i (- j 1))))
       (begin
	 (push path (cons i j))
	 (vector-set! (vector-ref maze i) j -1)
	 (set! j (- j 1))
	 ))
      (else
	;;; back to the last position
	(begin
	  (display (reverse path))
	  (newline)
	  (if (empty? path)
	    (begin
	    (display "There is no path to the end!")
	    (set! i (- len 1))
	    (set! j (- len 1)))
	    (begin
	      (define pos (pop path))
	      (vector-set! (vector-ref maze i) j -1)
	      (set! i (car pos))
	      (set! j (cdr pos))
	      ))))
	)))

(find-path maze)
