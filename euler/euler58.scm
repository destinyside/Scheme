;Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.
;37 36 35 34 33 32 31
;38 17 16 15 14 13 30
;39 18  5  4  3 12 29
;40 19  6  1  2 11 28
;41 20  7  8  9 10 27
;42 21 22 23 24 25 26
;43 44 45 46 47 48 49
;It is interesting to note that the odd squares lie along the bottom right diagonal, but what is more interesting is that 8 out of the 13 numbers lying along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.
;If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed. If this process is continued, what is the side length of the square spiral for which the ratio of primes along both diagonals first falls below 10%?


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
(define (pr-iter i j k l pn n)
  (display i)
  (newline)
  (if  (or (= pn 0) (> (/ pn n) 0.1))
    (begin
      (display "the percentage is ")
      (display (/ pn n))
      (newline)
      (if (= k j)
	(if (prime? i)
	  (set! pn (+ pn 1)))
	(set! n (+ n 1)))
      (if (= l 3)
	(pr-iter (+ i 1) (+ j 2) 0 0 pn n)
	(pr-iter (+ i 1) j (+ k 1) (+ l 1) pn n)))
    (begin
      (display "prime ")
      (display pn)
      (newline)
      (display "n ")
      (display n)
      (newline))))
(pr-iter 1 2 0 0 0 1)



