 (define neven?	   
   (lambda (n)	     
     (if (= 0 n)	       
       #t	       
       (nodd? (- n 1)))))
(define nodd? 
  (lambda (n)	     
    (if (= 0 n)	       
      #t       
      (neven? (- n 1)))))
