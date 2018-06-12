(library (math)
	 (export A C cbrt fact inte power rootn
		 line-ps line-pp line-si
		 )
	 (import (rnrs (6)))

	 ;;line, the format of point-slope
	 (define line-ps
	   (lambda (x0 y0 k)
	     (lambda (x)
	       (+ (* k (- x x0)) y0))))

	 ;;line, the format of point-point
	 (define line-pp
	   (lambda (x0 y0 x1 y1)
	     (lambda (x)
	       (+ (* ( / (- x x0) (- x1 x0)) (- y1 y0)) y0))))

	 ;;line, the format of slope-intercept
	 (define line-si
	   (lambda (k b)
	     (lambda (x)
	       (+ (* k x) b))))

	 (define (A n x)
	   (* (C n x) (fact x)))

	 (define (C n x)
	   (/ (fact n) (* (fact x) (fact (- n x)))))

	 (define (cbrt x)
	   (define (cbrt-iter x mini maxi)
	     (define ans (/ (+ mini maxi) 2))
	     (define tmp (- (power ans 3) x))
	     (cond
	       ((< tmp -0.05) (cbrt-iter x ans maxi))
	       ((> tmp 0.05) (cbrt-iter x mini ans))
	       (else ans)))
	   (cbrt-iter x 0 x))

	 (define (fact n)
	   (define (fact-iter n ans)
	     (if (> n 0) (fact-iter (- n 1) (* ans n)) ans))
	   (fact-iter n 1))

	 (define (inte f mini maxi)
	   (define (inte-iter f mini maxi ans)
	     (if (< mini maxi) (inte-iter f (+ mini 0.01) maxi (+ ans (f mini))) (/ ans 100)))
	   (inte-iter f mini maxi 0))

	 (define (power x n)
	   (define (power-iter x n ans)
	     (cond
	       ((= n 0) ans)
	       ((< n 0) (/ 1 (power-iter x (- 0 n) ans)))
	       ((> n 0) (power-iter x (- n 1) (* x ans)))))
	   (power-iter x n 1))

	 (define (rootn x n)
	   (define (rootn-iter x n mini maxi)
	     (define ans (/ (+ mini maxi) 2))
	     (define tmp (- (power ans n) x))
	     (cond
	       ((< tmp -0.005) (rootn-iter x n ans maxi))
	       ((> tmp 0.005) (rootn-iter x n mini ans))
	       (else ans)))
	   (rootn-iter x n 0 x))

	 )
