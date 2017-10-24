(define-syntax map-filter
 (lambda (x)
  (syntax-case x ()
   ((_ fn lst)
#'(let ((tmp (map fn lst)))
	(filter number? tmp))))))

(define (quicksort lst)
 (if (<= (length lst) 1) 
  lst
  (let ((sign (car lst)))
   (append
	(quicksort (map-filter (lambda (x) (if (< x sign) x)) lst))
	(filter number? (map (lambda (x) (if (= x sign) x)) lst))
	(quicksort (map-filter (lambda (x) (if (> x sign) x)) lst))
   ))))

