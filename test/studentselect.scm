

(define (less-than left right step)
  (if (<= (- right left) step)
    #t
    #f))

(define ability-power
  (lambda x
    (apply * x)))

(define (select-student lst amount)
  (display lst)
  )

(define (get-students n)
  (let ((lst '()))
    (do ()
      ((<= n 0)
       lst)
      (display "Input the ability value : ")
      (set! lst (append lst (list (read))))
      (set! n (- n 1))
      )))

(define (init-select)
  (let ((students '())
	(amount '())
	(step '()))
    (display "Input the amount of students : ")
    (set! students (get-students (read)))
    (newline)
    (display "Input the amount of selected : ")
    (set! amount (read))
    (newline)
    (display "Input the step : ")
    (set! step (read))
    (display students)
    (newline)
    (display amount)
    (newline)
    (display step)
    ))

(init-select)
