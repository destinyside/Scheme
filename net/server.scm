 ;server.scm
(declare (uses tcp))
(define l (tcp-listen 4242))
(define-values (i o) (tcp-accept l))
(write-line "1.hello 2.world 3.exit"  o)
(define (lsen-iter char)
  (write-line char o)
  (case char
    (('1) (write-line "req hello" o))
    (('2) (write-line "req world" o))
    (('3) (begin
	     (write-line "bye" o)
	     (close-input-port i)
	     (close-output-port o))))
  (lsen-iter (read-line i)))

(lsen-iter (read-line i))
