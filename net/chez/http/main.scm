


;;; socket from ffi/socket
(load "./socket.so")
(load "header-parser.scm")
(load "html-parser.scm")

(define operation 
  (lambda (client)
    (display "A client connected ...")
    (newline)
    (do ([data (do-recv client) (do-recv client)])
      ((and (string? data) (string=? data "quit")) (begin (close client) (exit)))
      (display data)
      (newline) 
      (let* ([html-data (string-append "<!DOCTYPE html><p>the data is " data "</p>")]
	    [html-data-len (string-length html-data)])
	(do-send client 
		 (string-append "HTTP/1.1 200 OK\r\n"
				"Cache-Control: public, max-age=60\r\n"
				"Date: Sat, 10 Apr 2018 16:50:00 CST\r\n"
				"Expires: Sat, 10 Apr 2018 17:00:00 CST\r\n"
				"Vary: Accept-Encoding\r\n"
				"Content-Type: text/html; charset=utf-8\r\n"
				"Content-Length: " (number->string html-data-len) "\r\n"
				"\r\n"
				html-data
				)))
      )  	
    (newline)))


(init-buf 2000)
(let ([pid (setup-server-socket "0.0.0.0" 8080)])
  (display "Server started ...")
  (newline)
  (do ([client 0])
    ((= -1 client) (break))
    (set! client (accept-socket pid))
    (dofork   ; child
      (operation client)
      (lambda (pid) ; parent
	; the parent waits for a connection from the client
	(set! client-pid pid)))))





#!eof


(load-shared-object "./csocket.so")
(load-shared-object "libc.so.6")

(define server-addr "127.0.0.1")

(define init-addr
  (lambda (addr)
    (set! server-addr addr)))

(define init-port
  (foreign-procedure "init_port" (int) void))

(define init-buf
  (foreign-procedure "init_buf" (int) void))

(define strlen
  (foreign-procedure "strlen" (string) int))

(define perror
  (foreign-procedure "perror" (string) void))

(define inet-addr
  (foreign-procedure "inet_addr" (string) int))

(define socket
  (foreign-procedure "do_socket" () int))

(define bind
  (foreign-procedure "do_bind" (int string) int))

(define connect
  (foreign-procedure "do_connect" (int string) int))

(define accept 
  (foreign-procedure "do_accept" (int string) int))

(define listen
  (foreign-procedure "listen" (int int) int))

(define close
  (foreign-procedure "close" (int) int))

(define send
  (foreign-procedure "send" (int string int int) int))

(define do-send
  (lambda (sock msg)
    (display (string-append "send a message : " msg))
    (newline)
    (send sock msg (strlen msg) 0)))

(define do-recv
  (foreign-procedure "do_recv" (int) string))

(define fork
  (foreign-procedure "fork" () int))

(define kill
  (foreign-procedure "kill" (int int) int)) 

(define check
  (lambda (who x)
    (if (< x 0)
      (perror (string-append "err : " (symbol->string who)))
      x)))

(define accept-socket
  (lambda (sock)
    (do ([temp (accept sock "0.0.0.0")])
      ((> temp 0) temp))))

(define pkill
  (lambda (pid)
    (define sigterm 15)
    (kill pid sigterm)
    (void)))

(define setup-server-socket
  (lambda (addr port)
    (init-addr addr)
    (init-port port)
    (let ([sock (check 'socket (socket))])
      (check 'bind (bind sock server-addr))
      (check 'listen (listen sock 5))
      sock))) 

(define setup-client-socket
  (lambda (addr port)
    (init-port port)
    (let ([sock (check 'socket (socket))])
      (check 'connect (connect sock addr))
      sock))) 

#!eof

;; these two part of codes should be run in different terminal and the server runs first.

;; the server
(let ([pid (setup-server-socket "0.0.0.0" 8080)])
  (display "Server started ...")
  (newline)
  (let ([client (accept-socket pid)])
    (display "A client connected ...")
    (newline)
    (do ([data (do-recv client) (do-recv client)])
      ((and (string? data) (string=? data "quit")) (begin (close client) (exit)))
      (display data)
      (newline)
      (if (string=? data "")
	(begin
	  (close client)
	  (quit))
	(do-send client (string-append "the data is " data)))
      (newline))))

;; the client
(let ([server (setup-client-socket)])
  (display "Client started ...")
  (newline)
  (display "send message : ")
  (do ([message (read) (read)])
    ((and (string? message) (string=? message "quit")) 
     (begin 
       (do-send server "exit")
       (close server) 
       (quit)))
    (do-send server message)
    (let ([data (do-recv server)])
      (display data)
      (newline)
      (display "send message : "))))
