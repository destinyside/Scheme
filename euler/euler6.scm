;The sum of the squares of the first ten natural numbers is,
;12 + 22 + ... + 102 = 385
;The square of the sum of the first ten natural numbers is,
;(1 + 2 + ... + 10)2 = 552 = 3025
;Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.
;Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

(define (square x) (* x x))
(define (mknlist n)
  (if (> n 0)
    (cons n (mknlist (- n 1)))
    '()))
(display (- (apply + (map square (mknlist 100)))
   (square (apply + (mknlist 100)))))
