;;;
;;;
;;;

(load-shared-object "./cfunctions.so")
(load-shared-object "libc.so.6")

(define cmax 
  (foreign-procedure "max" (int int int) int))

(define creverse
  (foreign-procedure "reverse" (string) string))

(define strlen
  (foreign-procedure "strlen" (string) int))

(define strcat
  (foreign-procedure "strcat" (string string) string))

(define fork
  (foreign-procedure "fork" () int))

(define kill
  (foreign-procedure "kill" (int int) int)) 

(define pkill
  (lambda (pid)
    (define sigterm 15)
    (kill pid sigterm)
    (void)))
