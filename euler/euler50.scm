;The prime 41, can be written as the sum of six consecutive primes: ;41 = 2 + 3 + 5 + 7 + 11 + 13 ;This is the longest sum of consecutive primes that adds to a prime below one-hundred.
;The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.
;Which prime, below one-million, can be written as the sum of the most consecutive primes?


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

(define len 1000000)
(set! p (make-vector len))
(define (vecset-iter i)
  (if (< i len)
    (begin
      (vector-set! p i i)
      (vecset-iter (+ i 1)))))
(vecset-iter 0)
(vector-set! p 1 0)
(define (mod i)
  (if (< i (sqrt len))
    (begin
      (define (loop k)
	(if (> len (* i k))
	  (begin
	    (vector-set! p (* i k) 0)
	    (loop (+ k 1)))))
      (loop 2)
      (mod (+ i 1)))))
(mod 2)

(define (primesum a n)
  (define (countgt i sum)
    (if (< 0 (vector-ref p i))
      (set! sum (+ sum i)))
    (if (< (+ i 1) n)
      (countgt (+ i 1) sum)
      sum))
  (countgt a 0))

(define (primeseq n)
  (define (ps-iter i j sum num)
    (if (< (primesum i j) n)
      (begin
	(if (and (prime? (primesum i j)) (> (- j i) num))
	  (begin
	    (set! num (- j i))
	    (set! sum (primesum i j))))
	(ps-iter i (+ j 1) sum num)
	(ps-iter (+ i 1) (+ i 4) sum num))
      (begin
	(display "the sum is ")
	(display sum)
	(newline)
	(display "the num is ")
	(display num)
	)))
  (ps-iter 1 3 0 1))

(primeseq 1000000)
