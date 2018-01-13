



(load-shared-object "./csocket.so")
(load-shared-object "libc.so.6")

(define perror
  (foreign-procedure "perror" (string) void))

(define socket
  (foreign-procedure "do_socket" (void) int))

(define bind
  (foreign-procedure "do_bind" (int string) int))

(define connect
  (foreign-procedure "do_connect" (int string) int))

(define accept 
  (foreign-procedure "do_accept" (int) int))

(define listen
  (foreign-procedure "listen" (int int) int))

(define close
  (foreign-procedure "close" (int int) int))

(define send
  (foreign-procedure "send" (int string int int) int))

(define recv
  (foreign-procedure "recv" (int string int int) int))

(define fork
  (foreign-procedure "fork" () int))

(define kill
  (foreign-procedure "kill" (int int) int)) 

(define check
  (lambda (who x)
    (if (< x 0)
      (errorf who (perror (symbol->string who)))
      x)))

(define setup-server-socket
  (lambda (addr)
    (let ([sock (check 'socket (socket))])
      (check 'bind (bind sock addr))
      (check 'listen (listen sock 5))
      sock))) 

(define setup-client-socket
  (lambda (addr)
    (let ([sock (check 'socket (socket))])
      (check 'connect (connect sock addr))
      sock))) 

(define accept-socket
  (lambda (sock)
    (check 'accept (accept sock)))) 

(define pkill
  (lambda (pid)
    (define sigterm 15)
    (kill pid sigterm)
    (void)))
