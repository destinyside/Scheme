(define (make-stack) '())

;;; It doesn't work when defined as a function
(define old_push
  (lambda (stack . value)
    (set! stack (append value stack))
    (display stack))
  )
;;; It works as below
(define-syntax push
  (syntax-rules 
    ()
    ((_) #f)
    ((_ stack) #f)
    ((_ stack value ...)
     (set! stack (append (list value ...) stack)))
    ))
;;; same as the push
(define (old_pop stack)
  (cond
    ((null? stack) #f)
    (else
      (begin
	(car stack)
	(set! stack (cdr stack)))
      )))

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
