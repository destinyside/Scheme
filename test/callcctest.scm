(define-syntax for
  (syntax-rules 
    ()
    ((_ (a b) body)
     (do ((i a (+ i 1)))
       ((= i b))
       body))
    ((_ (i) body)
     (for (0 i) body))))

(define db '())
(define max-length 5)
(define i 0)
(define (get)
  (set! i (+ i 1))
  i)

(define (full? lst)
  (if (>= (length lst) max-length)
    #t
    #f
    ))
(define (producer cmr)
  (define n (random 5))
  (call/cc
    (lambda (cc)
      (for (n)
	   (begin
	     (if (not (full? db))
	       (begin
		 (set! db (append (list (get)) db))
		 (display "produce ")
		 (display db)
		 (newline))
	       (begin
		 (display "its full, waiting for consume.")
		 (newline)))
	     ))
	     ;;;(sleep 1) working in guile but not in chicken
	     (newline)
	     (cc (cmr producer)))))

    (define (consumer pdr)
      (define n (random 5))
      (call/cc
	(lambda (cc)
	  (cond
	    ((null? db)
	     (cc (pdr consumer)))
	    (else
	      (begin
		(for (n)
		     (begin
		       (if (not (null? db))
			 (begin
			   (display "consume ")
			   (display (car db))
			   (newline)
			   (set! db (cdr db))
			   )
			 (begin
			   (display "its empty, waiting for produce.")
			   (newline)
			   )
			 ))
		     )
		;;;(sleep 1)  working in guile but not in chicken
		(newline)
		(cc (pdr consumer))))))))



