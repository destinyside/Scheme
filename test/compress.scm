(define (compress x)
  (cond
    ((null? x) x)
    (else
      (compr (car x) 1 (cdr x)))))

(define (compr a n b)
  (cond
    ((null? b)
     (if (= n 1)
       (list a)
       (list n a)))
    ((not (list? b))
     (if (= n 1)
       (list a b)
       (list (list (list n a)) b)))
    ((= a (car b)) (compr a (+ n 1) (cdr b)))
    (else
      (if (= n 1)
	(append (list a) (compr (car b) 1 (cdr b)))
	(append (list (list n a)) (compr (car b) 1 (cdr b)))))))

(define (number-pair? p)
  (if (pair? p) 
    (and (number? (car p)) (number? (cadr p)))
    #f))

(define (list-of n p)
  (let ((tmp (make-vector n)))
    (vector->list (vector-fill! tmp p))))

(define (uncompress x)
  (cond
    ((null? x) x)
    ((number-pair? x) 
     (list-of (car x) (cadr x)))
    (else
      (uncompr (car x) (cdr x)))))

(define (uncompr a b)
  (cond
    ((null? b)
     (list a))
    ((number? b)
     (list (uncompress a) b))
    ((number? a)
     (append (list a) (uncompr (car b) (cdr b))))
    ((number-pair? a)
     (append 
       (uncompress a)
       (uncompr (car b) (cdr b))))
    ))
