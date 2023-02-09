#lang racket
(define/contract (is-palindrome x)
  (-> exact-integer? boolean?)
    (and (>= x 0) (let* ([str (~a x)] [lst (string->list str)] [rlst (reverse lst)]) (display (~a str " " lst " " rlst " \n")) (equal? lst rlst)))
  )


(let ([l '(1 2 3 10 303 -121 121)])
  (display
   (cons
    "\n"
    (map
     (lambda (n)
       (~a n "\t" (is-palindrome n) " \n")
       )
     l
     )))
  )