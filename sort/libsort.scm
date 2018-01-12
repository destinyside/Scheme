(library (sort)
	 (export insertion-sort quicksort shell-sort simple-selection-sort)
	 (import (rnrs))
         
	 ;doesn't work in r6rs
	 ;(define (random-array n)
	 ;  (map (lambda (x) (random n)) (make-list n)))

	 (define (insertion-sort lst)
	   (let ((ptr 0)
		 (newlst (list (car lst))))
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
	     (do ((i 1 (+ i 1)))
	       ((= i (length lst)))
	       (set! newlst 
		 (insert (car newlst) (list-ref lst i) (cdr newlst)))
	       (display newlst)) 
	     newlst))


	 ;;; accept a list and sort it 
	 (define (shell-sort lst)
	   (let ()
	     ;;; switch the value of  i and i+step position's  element in the vector
	     ;;; the step is decrementing
	     (define (sorts step i vec)
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

	     (do ((i (/ (length lst) 2) (/ i 2)))
	       ((< i 1) lst)
	       (set! lst (vector->list (sorts (round i) 0 (list->vector lst)))))
	     ))


	 (define (simple-selection-sort lst)
	   (display lst)
	   )


	 (define (quicksort lst)
	   (define-syntax map-filter
	     (lambda (x)
	       (syntax-case 
		 x 
		 ()
		 ((_ fn lst)
		  #'(let ((tmp (map fn lst)))
		      (filter number? tmp))))))

	   (if (<= (length lst) 1) 
	     lst
	     (let ((sign (car lst)))
	       (append
		 (quicksort (map-filter (lambda (x) (if (< x sign) x)) lst))
		 (filter number? (map (lambda (x) (if (= x sign) x)) lst))
		 (quicksort (map-filter (lambda (x) (if (> x sign) x)) lst))
		 ))))

	 )
