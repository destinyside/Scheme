;; a interpreter of c0
(load "stack.scm")
(define (make-list num)
  (if (number? num)
    (cond
      ((<= num 1) 0)
      (else (cons 0 (make-list (- num 1)))))
    (error "Please input a number")))
(define (list-set! list1 i value)
  (set! v list1)
  (if (list? list1) 
    (begin
      (set! v (list->vector list1))
      (vector-set! v i value)
      (set! v (vector->list v))
  ))
  v
  )
(define (inter)
  (define p (stack))
  (lambda (opr . args)
    (define lev (car args))
    (display lev)
    (newline)
    (define act (cadr args))
    (display act)
    (case opr
      ((int) 
       (begin 
	 (define (push-iter arg)
	   (if (>= arg 1)
	     (p 'push 0))
	   (if (>= arg 1)
	     (push-iter (- arg 1))))
	 (push-iter act)))
      ((wrt) (p 'show))
      ((sto) (set! p (list-set! p act (p 'pop))))
      ((lit) (p 'push act))
      ((red) (p 'push (read)))
      ((add) (p 'push (+ (p 'pop) (p 'pop))))
      )))


(define k (inter))
