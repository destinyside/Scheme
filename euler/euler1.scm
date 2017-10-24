;If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
;Find the sum of all the multiples of 3 or 5 below 1000.
(define (sum35 k)
  (define (sum-iter a b i n)
    (if 
      (or 
	(= 0 (modulo i a)) 
	(= 0 (modulo i b)))
      (set! n (+ n i)))
      (if (< i (- k 1)) 
	(sum-iter a b (+ i 1) n)
        (display n)	
	))
  (sum-iter 3 5 1 0))
(sum35 1000)
