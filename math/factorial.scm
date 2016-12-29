(define (fact n)
  (define (fact-iter n ans)
    (if (> n 0) (fact-iter (- n 1) (* ans n)) ans))
  (fact-iter n 1))
