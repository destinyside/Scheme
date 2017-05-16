
;;; accept a list and sort it 
(define (shell-sort lst)
  (do ((i (- (length lst) 1) (- i 1)))
    ((<= i 0) lst)
    (set! lst (vector->list (sorts i 0 (list->vector lst)))))
  )
(define (shell-sort-2 lst)
  (do ((i (/ (length lst) 2) (/ i 2)))
    ((< i 1) lst)
    (set! lst (vector->list (sorts (round i) 0 (list->vector lst)))))
  )


(define (state step i vec)
  (display step)
  (display " : ")
  (display i)
  (display " : ")
  (display vec)
  (newline)
 ) 

;;; switch the value of  i and i+step position's  element in the vector
;;; the step is decrementing
(define (sorts step i vec)
  ;;; (state step i vec)
  (if (< (+ i step) (vector-length vec))
    (begin
      (if (> 
	    (vector-ref vec i) 
	    (vector-ref vec (+ i step)))
	(let ((tmp (vector-ref vec (+ i step))))
	  (vector-set! vec (+ i step) (vector-ref vec i))
	  (vector-set! vec i tmp)
	  )
	)
      (sorts step (+ i 1) vec)
      )
    vec
    )
  )

(shell-sort '(2 6 4 8 4 6 3 2))
