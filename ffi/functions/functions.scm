;;;
;;;
;;;

(load-shared-object "./cfunctions.so")
(load-shared-object "libc.so.6")

(define cmax 
  (foreign-procedure "max" (int int int) int))

(define creverse
  (foreign-procedure "reverse" (string) string))
