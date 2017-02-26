;It can convert decimal numbers to other formats,such as hex,octal and binary.

(define (to n num)
  (define p '())
  (define (iter i)
    (if (< n 2)
      (error "Can't Convert!")
      (if (>= i n)
	(begin 
	  (set! p (append (list (modulo i n)) p))
	  (iter (/ (- i (modulo i n)) n))
	  )
	(set! p (append (list i) p)))))
  (iter num)
  (display p)
  )

