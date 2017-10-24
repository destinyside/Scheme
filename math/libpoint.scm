(library (point)
	 (export make-point point-x point-y distance)
	 (import (rnrs (6)))
	 (define-record-type point (fields x y))

	 (define (pow2 x)
	   (* x x))

	 (define (distance pa pb)
	   (sqrt 
	     (+ 
	       (pow2 (- (point-x pb) (point-x pa)))
	       (pow2 (- (point-y pb) (point-y pa))))))

	 )
