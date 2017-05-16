(define construct-name
  (lambda (template-identifier . args)
    (datum->syntax
      template-identifier
      (string->symbol
	(apply string-append
	       (map (lambda (x)
		      (if (string? x)
			x
			(symbol->string (syntax->datum x))))
		    args))))))
(construct-name #'name "make-" #'name)
(construct-name #'name #'name "?")

(define-syntax kn
  (syntax-rules ()
		((_ s value)
		 (with-syntax
		   ((constructor (construct-name #'s "make-" #'s)))
		   #'(begin
		       (define constructor value)
		       
		       )
		   )
		 )
		)
  )
