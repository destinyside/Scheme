;The prime factors of 13195 are 5, 7, 13 and 29.
;What is the largest prime factor of the number 600851475143 ?

(define (prime? x)
  (define (factors i n)
    (if (= 0 (modulo x i))
      (if (> i 1) 
	#f)
      (if (< i n)
        (factors (+ i 1) n)
        #t)))
  (factors 2 (sqrt x)))
(define (primeplay num)
  (if (prime? num)
    (begin
      (display num)
      (display "  ok")
      (newline))
    (newline)))
(define (lgest x)
  (define (factors i n)
    (if (= 0 (modulo x i))
      (begin
	(primeplay i)
	(primeplay (/ x i))))
      (if (< i n)
	(factors (+ i 1) n)
	(begin
	  (display "done")
	  (newline))))
  (factors 2 (sqrt x)))
(lgest 600851475143)
