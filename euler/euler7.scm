;By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
;What is the 10 001st prime number?

(define (prime? x)
  (define (factors i n)
    (if (= 0 (modulo x i))
      (if (> i 1)
	#f)
      (if (< i n)
	(factors (+ i 1) n)
	#t)))
  (cond
    ((= x 2) #t)
    ((= x 1) #f)
    (else
      (factors 2 (sqrt x)))))
(define (nprime n)
  (define (np-iter i)
    (if (prime? i)
	(set! n (- n 1)))
    (if (> n 0)
      (np-iter (+ i 1))
      (display i)))
  (np-iter 2))
(nprime 10001)
