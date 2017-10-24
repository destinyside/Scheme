;Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
;If d(a) = b and d(b) = a, where a ≠b, then a and b are an amicable pair and each of a and b are called amicable numbers.
;For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
;Evaluate the sum of all the amicable numbers under 10000.

(define (d n)
  (define fact '(1))
  (define m (sqrt n))
  (define (div i)
    (if (= 0 (modulo n i)) (set! fact (append fact (list i (/ n i)))))
    (if (>= i m) 
      0
      (div (+ i 1))))
  (div 2)
  (apply + fact))

(define (ami-sum n)
  (define sum '())
  (define (iter i)
    (define j (d i))
    (if (<= i n)
      (begin
	(if (and (not (= i j)) (= i (d j)))
	  (set! sum (append sum (list i (d i)))))
	(iter (+ i 1)))
      (format "~A \n the sum : ~A" sum (/ (apply + sum) 2))))
  (iter 1))
