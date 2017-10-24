(load "factorial.scm")
(define (C n x)
  (/ (fact n) (* (fact x) (fact (- n x)))))
