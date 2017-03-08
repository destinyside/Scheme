;A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
;a2 + b2 = c2
;For example, 32 + 42 = 9 + 16 = 25 = 52.
;There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;Find the product abc.


(define (square x)
  (* x x))
(define (pytri? a b c)
  (eq? (square c) (+ (square b) (square a))))
(define (findtri n)
  (define (iter a b)
    (define c (- n (+ a b)))    
    (if (pytri? a b c)
      (format "~A ~A ~A" a b c)
      (cond 
	((< a n) (iter (+ a 1) b))
	((< b n) (iter 1 (+ b 1))))))
  (iter 1 1))
