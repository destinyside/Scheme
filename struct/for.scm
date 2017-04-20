;;; a for macro

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
