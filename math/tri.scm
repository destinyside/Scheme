(define (facto x)
  (if (< x 2) 1 (* x (facto (- x 1)))))
(define (bio a  n)
  (if (< a (+ n 1)) (display (/ (facto n) (* (facto (- n a)) (facto a)))))
  (display " ")
  (if (< a (+ n 1)) (bio (+ a 1) n)))

(define (tri a n)
  (if (< a (+ n 1)) (bio 0 a))
  (newline)
  (if (< a (+ n 1)) (tri (+ a 1) n)))

(define (act x)
  (if (= x 0) 0 (tri 1 x))
  (if (> x 0) (act (read))))

;(act (read))

