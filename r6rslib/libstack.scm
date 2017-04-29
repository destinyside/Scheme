(library 
  (stack)
  (export pop push empty?)
  (import (rnrs))

;;; a stack is performed as a list ()

  (define-syntax push
    (syntax-rules
      ()
      ((_) #f)
      ((_ stack) #f)
      ((_ stack value ...)
       (set! stack (append (list value ...) stack)))
      ))

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
  )


