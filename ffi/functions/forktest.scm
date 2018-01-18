;;;
;;;
;;;

(load "./functions.so")


(define (test)
  (let ([pid -1]
	[count 0]
	[port (current-output-port)])
    (set! pid fork)
    (case pid
      ((< pid 0) (error 'fork "error in fork!"))
      ((= pid 0) 
       (begin
	 (write "this is child!" port)
	 (newline port)
	 (set! count (+ count 1))))
      (else
       (begin
	 (write "this is parent!" port)
	 (newline port)
	 (set! count (+ count 1)))))
    (write "the count is : " port)
    (write count port)
    (newline port)))
	
