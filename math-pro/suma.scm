(define (suma n)
  (if (> n 0) (+ n (suma (- n 1))) 0))
(display (suma 1000))
