
(define (power x n)
  (define (power-iter x n ans)
    (cond
      ((= n 0) ans)
      ((< n 0) (/ 1 (power-iter x (- 0 n) ans)))
      ((> n 0) (power-iter x (- n 1) (* x ans)))))
  (power-iter x n 1))
(define (cbrt x)
  (define (cbrt-iter x mini maxi)
    (set! ans (/ (+ mini maxi) 2))
    (set! tmp (- (power ans 3) x))
    (cond
      ((< tmp -0.05) (cbrt-iter x ans maxi))
      ((> tmp 0.05) (cbrt-iter x mini ans))
      (else ans)))
  (cbrt-iter x 0 x))
