
(include "power.scm")

(define (rootn x n)
  (define (rootn-iter x n mini maxi)
    (set! ans (/ (+ mini maxi) 2))
    (set! tmp (- (power ans n) x))
    (cond
      ((< tmp -0.005) (rootn-iter x n ans maxi))
      ((> tmp 0.005) (rootn-iter x n mini ans))
      (else ans)))
  (rootn-iter x n 0 x))
