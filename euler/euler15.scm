;Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.

;How many such routes are there through a 20×20 grid?


(define (fact n)
  (define (fact-iter n ans)
    (if (> n 0) (fact-iter (- n 1) (* ans n)) ans))
  (fact-iter n 1))
(define (C n x)
  (/ (fact n) (* (fact x) (fact (- n x)))))
(define (countroutes n)
  (display (C (+ n n) n)))
(countroutes 20)
