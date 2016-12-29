;2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
;What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

(define mul (* 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
(define (divi? x)
  (if (and (= 0 (modulo x 2)) (= 0 (modulo x 3))
           (= 0 (modulo x 4)) (= 0 (modulo x 5))
	   (= 0 (modulo x 6)) (= 0 (modulo x 7))
	   (= 0 (modulo x 8)) (= 0 (modulo x 9))
	   (= 0 (modulo x 10)) (= 0 (modulo x 11)) 
	   (= 0 (modulo x 12)) (= 0 (modulo x 13))
	   (= 0 (modulo x 14)) (= 0 (modulo x 15)) 
	   (= 0 (modulo x 16))
	   (= 0 (modulo x 17)) (= 0 (modulo x 18))
	   (= 0 (modulo x 19)) (= 0 (modulo x 20)))
    #t
    #f))
(define (jg-iter n)
  (display mul)
  (newline)
  (cond
    ((divi? (round (/ mul 2)))
     (begin
       (set! mul (/ mul 2))
       (jg-iter mul)
       ))
    ((divi? (round (/ mul 3)))
     (begin
       (set! mul (/ mul 3))
       (jg-iter mul)
       ))
    ((divi? (round (/ mul 5)))
     (begin
       (set! mul (/ mul 5))
       (jg-iter mul)
       ))
    ((divi? (round (/ mul 7)))
     (begin
       (set! mul (/ mul 7))
       (jg-iter mul)
       ))
    (else
      (display mul))))
    
(jg-iter mul)
