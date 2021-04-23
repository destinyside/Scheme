;;;
;;;
;;;

(load-shared-object "./chello.so")
(load-shared-object "libc.so.6")

(define hello
  (foreign-procedure "hello" () void))

