;The following iterative sequence is defined for the set of positive integers:
;n →n/2 (n is even)
;n →3n + 1 (n is odd)
;Using the rule above and starting with 13, we generate the following sequence:
;13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
;It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
;Which starting number, under one million, produces the longest chain?
;NOTE: Once the chain starts the terms are allowed to go above one million.

  (define gtc 0)
  (define gtn 0)
(define (to1 i n  sum)
  (if (= i 1)
    (begin
      (display "  The chain is : ")
      (display sum)
      (if (> sum gtc)
	(begin
	(set! gtc sum)
	(set! gtn n)))
      (newline))
    (begin
      (cond
	((even? i)
	 (to1 (/ i 2) n  (+ sum 1)))
	((odd? i)
	 (to1 (+ (* i 3) 1) n  (+ sum 1)))))))
(define (findto1 n)
  (set! gtc 0)
  (set! gtn 0)
  (define (ft1-iter i)
    (if (<= i n)
      (begin
	(display i)

	(to1 i i  0)
	(ft1-iter (+ i 1)))
      (begin
	(display gtn)
	(display " ")
	(display gtc)
	(newline)
	(display "done")
	(newline))))
  (ft1-iter 1))
