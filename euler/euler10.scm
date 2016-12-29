
;The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
;Find the sum of all the primes below two million.

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
(define (countprime n)
  (define (cp-iter i sum)
    (if (< i n)
      (if (prime? i)
	(cp-iter (+ i 1) (+ sum i))
	(cp-iter (+ i 1) sum))
      sum))
  (cp-iter 2 0))
(countprime 2000000)
