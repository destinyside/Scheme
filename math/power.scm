(define (power x n)
  (define (power-iter x n ans)
    (cond
      ((= n 0) ans)
      ((< n 0) (/ 1 (power-iter x (- 0 n) ans)))
      ((> n 0) (power-iter x (- n 1) (* x ans)))))
  (power-iter x n 1))
