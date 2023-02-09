#lang racket
(require racket/format)

(define/contract (get-row rowIndex)
  (-> exact-integer? (listof exact-integer?))
  (define (iter b i)
    ;(display (~a " " b " " i " \n"))
    (cond
      ((= i rowIndex) b)
      (#t (iter (map (lambda (n m) (+ n m)) (append b '(0)) (append '(0) b)) (+ i 1)))
      )
    )
  (iter '(1) 0)
  )

(define/contract (generate numRows)
  (-> exact-integer? (listof (listof exact-integer?)))
  (define (iter l b i)
    (display (~a l " " b " " i " \n"))
    (cond
      ((= i numRows) l)
      (#t (iter (append l (list b)) (map (lambda (n m) (+ n m)) (append b '(0)) (append '(0) b)) (+ i 1)))
      )
    )
  (iter '() '(1) 0)
  )

(let ([l '(1 2 3 10 30)])
  (display
   (cons
    "\n"
    (map
     (lambda (n)
       (~a n "\t" (get-row n) " \n")
       )
     l
     )))
  )
 
;(define l (list 1 3 3 1))

;(map (lambda (n m) (+ n m)) (append l '(0)) (append '(0) l))