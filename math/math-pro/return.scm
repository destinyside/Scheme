(define (gt0? x)
  (call/cc (lambda (return)                              (set! k (read))
	   (if (> x k)                                         (return #t)
	     (return #f)))))
