(define construct-name
  (lambda (id . args)
    (datum->syntax
      id
      (string->symbol
	(apply string-append
	       (map (lambda (x)
		      (if (string? x)
			x
			(symbol->string (syntax->datum x))))
		    args))))))
(construct-name #'name "make-" #'name)
(construct-name #'name #'name "?")

(define-syntax (kn x)
  (syntax-case 
    x 
    ()
    ((_ s value)
     (with-syntax
       (
	(id (datum->syntax #'s (syntax->datum #'s)))
	(getter (construct-name #'s "get" #'s))
	(setter (construct-name #'s "set" #'s)))
       #'(begin
	   (define id value)
	   (define (getter) id)
	   (define (setter x) (set! id x))
	   )
       )
     )
    )
  )
