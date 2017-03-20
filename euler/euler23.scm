;A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

;A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.

;As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.

;Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

(define (factor-sum n)
  (define (iter i sum)
    (if (< i n)
      (if (= 0 (modulo n i))
        (iter (+ i 1) (+ sum i))
        (iter (+ i 1) sum)
      )
      sum
    )
  )
  (iter 1 0)
)

(define (abundant? n)
  (if (> (factor-sum n) n)
    #t
    #f
  )
)

(define (non-abundant-sum n)
  (define (iter-num i sum)
    (define (iter-fact j)
      (if (< j i)
        (if (and (abundant? j) (abundant? (- i j)))
          #f
          (iter-fact (+ j 1)))
        #t))
    (if (< i n)
      (if (iter-fact 1)
        (iter-num (+ i 1) (+ sum i))
        (iter-num (+ i 1) sum))
      sum))
  (iter-num 1 0))
  
