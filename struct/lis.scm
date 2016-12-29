(define (make-point-2D x y)
	(define (get-x) x)
	(define (get-y) y)
	(define (set-x! new-x) (set! x new-x))
        (define (set-y! new-y) (set! y new-y))
	(lambda (selector . args)     ;; a dispatcher
	 (case selector
	  ((get-x) (apply get-x args))
	  ((get-y) (apply get-y args))
	  ((set-x! (apply set-x! args))
	  ((set-y! (apply set-x! args))
	    (else (error "don't understand " selector)))))))
