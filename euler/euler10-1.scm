;The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
;Find the sum of all the primes below two million.


(define (countprime n)
  (set! p (make-vector n))
  (define (vecset-iter i)
    (if (< i n)
      (begin
	(vector-set! p i i)
	(vecset-iter (+ i 1)))))
  (vecset-iter 0)
  (vector-set! p 1 0)
  (define (mod i)
    (if (< i (sqrt n))
      (begin
      (define (loop k)
	(if (> n (* i k))
	  (begin
	(vector-set! p (* i k) 0)
	  (loop (+ k 1)))))
      (loop 2)
      (mod (+ i 1)))))
  (mod 2)
  (set! m (vector->list p))
  (define (countgt i sum)
    (if (< 0 (vector-ref p i))
      (set! sum (+ sum i)))
    (if (< (+ i 1)  (vector-length p))
      (countgt (+ i 1) sum)
      (display sum)))
  (countgt 0 0)
(countprime 2000000))
