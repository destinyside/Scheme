(define *pow-base* 2)


(define (pow n)
 (define (iter i sum)
  (if (<= i n)
   (iter (+ 1 i) (* sum *pow-base*))
   sum))
 (iter 1 1))

(define (npow b n)
 (if (= n 1)
  (pow b)
  (pow (npow b (- n 1)))))
