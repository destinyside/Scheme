(load "euler22_names.scm")

;remember to sort the list

;(display names)
(define nm (list->vector names))
(define (sort v)
  (map
    (lambda (j k)
      (if (string>=? j k)
	(begin
	  (define tmp j)
	  (set! j k)
	  (set! k tmp))))
    v	(cdr v)))
(define (chg str)
  (apply + (map (lambda (x) (- (char->integer x) 64)) (string->list str))))
(define list2 (map chg names))
;(display list2)
(define i 1)
(define list3 '())
(for-each
  (lambda (x)
    (display x)
    (display " * ")
    (display i)
    (newline)
    (set! list3 (append list3 (list (* x i))))
    (set! i (+ i 1))
    (display i)
    (newline)
    )
  list2)
(display list3)
(newline)
(display (apply + list3))

(define l (list "ABA" "ABD" "ABB" "ABC"))
