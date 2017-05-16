

(define (my-sort lst)
  (cond 
    ((null? lst) 0)
    ((null? (cdr lst)) (car lst))
    (else
      (append
	(list (min (car lst) (apply min (cdr lst)))
	      (my-sort (cdr lst)))))))
