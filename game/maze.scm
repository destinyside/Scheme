;;;#lang r5rs

(define-syntax for
  (syntax-rules
    (in)
    ((_ (mn mx) body)
     (cond
       ((< mx mn) #f)
       (else
	 (do 
	   ((i mn (+ i 1)))
	   ((= i mx))
	   body))))
    ((_ (n) body)
     (cond
       ((> n 0)
	(for (0 n) body))
       (else
	 (for (n 0) body))))
    ((_ (i in lst) body)
     (for-each
       (lambda (x)
	 (let ((i x))
	   body)
	 )
       lst))
    ))
(define (make-stack) '())

;;; It works as below
(define-syntax push
  (syntax-rules ()
		((_) #f)
		((_ stack) #f)
		((_ stack value ...)
		 (set! stack (append (list value ...) stack)))
		))

(define-syntax pop
  (syntax-rules
    ()
    ((_) #f)
    ((_ stack)
     (cond
       ((null? stack) #f)
       (else
	 (begin
	   (let ((i (car stack)))
	     (set! stack (cdr stack))
	     i
	     )
	   )
	 )
       ))))

(define (empty? stack)
  (null? stack))




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

;;; make a maze . a maze should let 0s as path and 1s as wall, the top left corner and the opposite shoule be 0.
;;; it seems like below : 
;;; 0 0 0 1
;;; 1 1 0 1
;;; 1 1 0 0
;;; 1 1 1 0
(define len 0)

(define (make-maze)
  (define mz 0)
  (set! len (prompt "Input length : "))
  (display "Input the maze : ")
  (newline)
  (set! mz (make-vector len))
  (for (i in (make-sequence len))
       (begin
	 (vector-set! mz i (make-vector len))
	 (for (j in (make-sequence len))
	      (vector-set! (vector-ref mz i) j (read))
	      )
	 ))
  mz)


;;; show the maze
(define (show mz)
  (for (i in (make-sequence len))
       (begin
	 (display (vector-ref mz i))
	 (newline)))
  )

;;; packing the operation of get maze's position
(define (getxy mz x y)
  (vector-ref (vector-ref mz x) y))

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
    ;(show mz)
    (cond
      ;;; walk down
      ((and (> len (+ i 1)) (= 0 (getxy mz (+ i 1) j)))
       (begin
	 (push path (cons i j))
	 ;;; path and mark
	 (vector-set! (vector-ref mz i) j -1)
	 (set! i (+ i 1))
	 ))
      ;;; walk right
      ((and (> len (+ j 1)) (= 0 (getxy mz i (+ j 1))))
       (begin
	 (push path (cons i j))
	 (vector-set! (vector-ref mz i) j -1)
	 (set! j (+ j 1))
	 ))
      ;;; walk up
      ((and (<= 0 (- i 1)) (= 0 (getxy mz (- i 1) j)))
       (begin
	 (push path (cons i j))
	 (vector-set! (vector-ref mz i) j -1)
	 (set! i (- i 1))
	 ))
      ;;; walk left
      ((and (<= 0 (- j 1)) (= 0 (getxy mz i (- j 1))))
       (begin
	 (push path (cons i j))
	 (vector-set! (vector-ref mz i) j -1)
	 (set! j (- j 1))
	 ))
      (else
	;;; back to the last position
	(begin
	  ;(display (reverse path))
	  ;(newline)
	  (if (empty? path)
	    (begin
	      (display "There is no path to the end!")
	      (set! i (- len 1))
	      (set! j (- len 1)))
	    (let
	      ((pos (pop path)))
	      (vector-set! (vector-ref mz i) j -1)
	      (set! i (car pos))
	      (set! j (cdr pos))
	      )
	    )))
      )))

(define maze (make-maze))
(show maze)
(find-path maze)
