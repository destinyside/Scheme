(include "power")

(define (cbrt x)
  (define (cbrt-iter x mini maxi)
    (set! ans (/ (+ mini maxi) 2))
    (set! tmp (- (power ans 3) x))
    (cond
      ((< tmp -0.05) (cbrt-iter x ans maxi))
      ((> tmp 0.05) (cbrt-iter x mini ans))
      (else ans)))
  (cbrt-iter x 0 x))
