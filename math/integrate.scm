(define (inte f mini maxi)
  (define (inte-iter f mini maxi ans)
    (if (< mini maxi) (inte-iter f (+ mini 0.01) maxi (+ ans (f mini))) (/ ans 100)))
  (inte-iter f mini maxi 0))
