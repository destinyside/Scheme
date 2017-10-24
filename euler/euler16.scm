;215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
;What is the sum of the digits of the number 21000?

(define (power x n)
  (define (power-iter x n ans)
    (cond
      ((= n 0) ans)
      ((< n 0) (/ 1 (power-iter x (- 0 n) ans)))
      ((> n 0) (power-iter x (- n 1) (* x ans)))))
  (power-iter x n 1))
(display (number->string(/ (power 2 1000) (power 10 300))))
