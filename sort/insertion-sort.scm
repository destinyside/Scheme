
(define ptr 0)

(define (insertion-sort lst)
  (let ((newlst (list (car lst))))
    (do ((i 1 (+ i 1)))
      ((= i (length lst)))
      (set! newlst 
	(insert (car newlst) (list-ref lst i) (cdr newlst)))
  (display newlst)
      )
    newlst
    )
  )

;;; insert the num to the list 
(define (insert pre num nxt)
  (cond
    ((null? nxt) 
     (if (> pre num)
       (list num pre)
       (list pre num)
       ))
    ((< num pre) 
     (append (list num pre) nxt))
    (else
      (append 
	(list pre) 
	(insert (car nxt) num (cdr nxt))))
    ))

(insertion-sort '(3 6 2 8 4 1 7 5 9))
