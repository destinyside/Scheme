;Working from left-to-right if no digit is exceeded by the digit to its left it is called an increasing number; for example, 134468.

;Similarly if no digit is exceeded by the digit to its right it is called a decreasing number; for example, 66420.

;We shall call a positive integer that is neither increasing nor decreasing a "bouncy" number; for example, 155349.

;Clearly there cannot be any bouncy numbers below one-hundred, but just over half of the numbers below one-thousand (525) are bouncy. In fact, the least number for which the proportion of bouncy numbers first reaches 50% is 538.

;Surprisingly, bouncy numbers become more and more common and by the time we reach 21780 the proportion of bouncy numbers is equal to 90%.

;Find the least number for which the proportion of bouncy numbers is exactly 99%.
(include "eulerch2num.scm")
(define (increase? l)
  (cond
    ((eq? l '()) #t)
    ((eq? (cdr l) '()) #t)
    ((> (car l) (cadr l)) #f)
    (else (increase? (cdr l)))
    )
  )
(define (decrease? l)
  (cond
    ((eq? l '()) #t)
    ((eq? (cdr l) '()) #t)
    ((< (car l) (cadr l)) #f)
    (else (decrease? (cdr l)))
    )
  )
(define (bouncy? n)
  (define l (map char->number (string->list (number->string n))))
  (if (not (or (increase? l) (decrease? l)))
      #t
      #f
      )
  )
(define (bouncy-loop n per)  ;just like (bouncy-loop 1000 0.5)
  (define (bl-iter i sum num p)
    (if (< i n)
        (begin
          (if (bouncy? i)
              (begin
                (set! sum (+ sum 1))
                ;(display i)
                ;(display " bounny ")
                (set! p (* (/ sum i) 1.0))
                ;(display p)
                (if (and (= num 0) (>= p per))
                      (set! num i)
                    )
                ;(newline)
                )
              )
          (bl-iter (+ i 1) sum num p)
          )
        (begin
          (display num)
          (display "  ")
          ;(display p)
          (display " Done")
          ) 
        )
    )
  (bl-iter 1 0 0 0)
  )
  
