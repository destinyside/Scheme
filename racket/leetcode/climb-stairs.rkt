
#lang racket
(require racket/format)

(define/contract (climb-stairs n)
  (-> exact-integer? exact-integer?)
  (define (iter n i a b)
    (cond
      ((= i n) b)
      (#t (iter n (+ i 1) b (+ a b)))
      )
    )
  (iter n 0 0 1)
  )

(let ([l '(1 2 3 10 45)])
  (display
   (map 
    (lambda (n)
      (~a (climb-stairs n))
      )
    l
    ))
  )
