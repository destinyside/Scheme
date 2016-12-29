;A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
;a2 + b2 = c2
;For example, 32 + 42 = 9 + 16 = 25 = 52.
;There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;Find the product abc.

(define (square x)
  (* x x))
(define (pytri? a b c)
     (= (square c) (+ (square b) (square a))))
(define (findtri n)
  (define (ftri-iter a b)
    (set! c (- n a b))
    (if (and (> c 0) (pytri? a b c))
      (begin
	(display a)
	(display "  ")
	(display b)
	(display "  ")
	(display c)
	(newline)))
      (if (and (< a n) (< b n))
	(begin
	(ftri-iter (+ a 1) b)
	(ftri-iter a (+ b 1)))))
  (ftri-iter 1 1))
