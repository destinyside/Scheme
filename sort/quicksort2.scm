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


(display (quicksort '(1 234 6 1 2 3 5 3 4 234 5 46 67 3 45 234 234 6 5 6 78 78 234)))

