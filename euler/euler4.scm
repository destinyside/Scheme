;A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
;Find the largest palindrome made from the product of two 3-digit numbers.

(define (palindrome? x)
  (string=? (number->string x) (list->string (reverse (string->list (number->string x))))))
(define (lgpalin n)
  (define (lgp-iter a b)
    (define ok 0)
    (if (palindrome? (* a b))
      (begin 
	(display a)
	(display " * ")
	(display b)
	(display " = ")
	(display (* a b))
	(display " ok")
	(newline)
	(set! ok 1)))
      (if (and (and (> (* a b) 900000)) (= ok 0))
      (begin
	(lgp-iter (- a 1) b)
	(lgp-iter a (- b 1))
	(lgp-iter (- a 1) (- b 1)))))
  (lgp-iter n n))
(lgpalin 999)
