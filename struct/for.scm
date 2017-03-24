(define-syntax for
  (syntax-rules 
    (in)
    ((_ (a b) c)
     (begin
       (define (iter i)
	    (if (< i b)
	      (begin
		c
		(iter (+ i 1)))))
       (iter a))
     )
    ((_ (a) c) (for (0 a) c))
    ((_ (i in b) c) 
     (for-each 
       (lambda (x)
	 (let ((i x))
	   c))
	       b)
     ) 
    )
  )
