(library (sort)
	 (export insertion-sort quicksort shell-sort simple-selection-sort)
	 (import (rnrs))
	 (define ptr 0)
	 (define (insertion-sort lst)
	   (let ((newlst (list (car lst))))
	     (do ((i 1 (+ i 1)))
	       ((= i (length lst)))
	       (set! newlst 
		 (insert (car newlst) (list-ref lst i) (cdr newlst)))
	       (display newlst)) 
	     newlst))

	 ;;; insert the num to the list 
	 (define (insert pre num nxt)
	   (cond
	     ((null? nxt) 
	      (if (> pre num)
		(list num pre)
		(list pre num)
		))
	     ((< num pre) 
	      (append (list num pre) nxt))
	     (else
	       (append 
		 (list pre) 
		 (insert (car nxt) num (cdr nxt))))
	     ))

	 (define (old-list-set! lst idx value)
	   (let ((v (list->vector lst)))
	     (vector-set! v idx value)
	     (vector->list v)))

	 (define (quicksort lst)
	   (call/cc (lambda (cc) (quicksort-item cc lst 0 (- (length lst) 1)))))

	 (define (quicksort-item cc lst low high)
	   (define first 0)
	   (define last 0)
	   (define key 0)
	   (cond
	     ((>= low high)
		(cc lst)
		)
	     (else
	       (set! first low)
	       (set! last high)
	       (set! key (list-ref lst first))
	       (do ()
		 ((>= first last)
		  (begin
		    (set! lst (old-list-set! lst first key))
		    (quicksort-item cc lst low (- first 1))
		    (quicksort-item cc lst (+ first 1) high)
		    )
		  )
		 (display lst)
		 (newline)
		 (do ()
		   ((not (and (< first last) (>= (list-ref lst last) key)))
		    (set! lst (old-list-set! lst first (list-ref lst last))))
		   (set! last (- last 1)))
		 (do ()
		   ((not (and (< first last) (<= (list-ref lst first) key)))
		    (set! lst (old-list-set! lst last (list-ref lst first))))
		   (set! first (+ first 1)))

		 ))))

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
	   (state step i vec)
	   (if (< (+ i step) (vector-length vec))
	     (begin
	       (if (> 
		     (vector-ref vec i) 
		     (vector-ref vec (+ i step)))
		 (let ((tmp (vector-ref vec (+ i step))))
		   (vector-set! vec (+ i step) (vector-ref vec i))
		   (vector-set! vec i tmp)))
	       (sorts step (+ i 1) vec))
	     vec))

	 (define (simple-selection-sort lst)
	   (display lst)
	   )

	 )
