#lang racket

(define/contract (int-to-roman num)
  (-> exact-integer? string?)
  (cond
    ((>= num 1000) (~a "M" (int-to-roman (- num 1000))))
    ((>= num 900) (~a "CM" (int-to-roman (- num 900))))
    ((>= num 500) (~a "D" (int-to-roman (- num 500))))
    ((>= num 400) (~a "CD" (int-to-roman (- num 400))))
    ((>= num 100) (~a "C" (int-to-roman (- num 100))))
    ((>= num 90) (~a "XC" (int-to-roman (- num 90))))
    ((>= num 50) (~a "L" (int-to-roman (- num 50))))
    ((>= num 40) (~a "XL" (int-to-roman (- num 40))))
    ((>= num 10) (~a "X" (int-to-roman (- num 10))))
    ((>= num 9) (~a "IX" (int-to-roman (- num 9))))
    ((>= num 5) (~a "V" (int-to-roman (- num 5))))
    ((>= num 4) (~a "IV" (int-to-roman (- num 4))))
    ((>= num 1) (~a "I" (int-to-roman (- num 1))))
    (#t "")
    )
  )

(let ([l '(1 2 345 500 900 1333 3999)])
  (display
   (cons
    "\n"
    (map
     (lambda (n)
       (~a n "\t" (int-to-roman n) " \n")
       )
     l
     )))
  )