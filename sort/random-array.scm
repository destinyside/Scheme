(define (random-array n)
 (map (lambda (x) (random n)) (make-list n)))
