;;;
;;;
;;;

(load-shared-object "./chello.so")
(load-shared-object "libc.so")

(define hello
  (foreign-procedure "hello" () void))

(define cmax 
  (foreign-procedure "max" (int int int) int))
