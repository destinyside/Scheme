(define (f x)   
   ((if (even? x)
     (begin (display x)(display " even")(newline))
     (begin (display x)(display " odd")(newline)))
  (f (read))))
(define c "please input a range")
(display c)
(newline)
(f (read))