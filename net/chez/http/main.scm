


;;; socket from ffi/socket
(load "./socket.so")
(load "http-parser.scm")
(load "html-parser.scm")
(load "thread.scm")

(import (http-parser))
(import (html-parser))
(import (thread))

(define ip "0.0.0.0")
(define port 8090)
(define buf 2000)

(define operation 
  (lambda (client)
    (display "A client ")
    (display client)
    (display " connected ...")
    (newline)
    (do ([data (do-recv client) (do-recv client)])
      ((not (http? data)) (close client))
      (let* ([http-table (http data)]
             [html-data (string-append 
        "<!DOCTYPE html><p>the data is " 
        "</p>" 
        "<div>"
        (ulist http-table)
        "</div>")]
      [html-data-len (string-length html-data)])
        (do-send client 
          (string-append 
              "HTTP/1.1 200 OK\r\n"
              "Cache-Control: public, max-age=60\r\n"
              "Date: Sat, 10 Apr 2018 16:50:00 CST\r\n"
              "Expires: Sat, 10 Apr 2018 17:00:00 CST\r\n"
              "Vary: Accept-Encoding\r\n"
              "Content-Type: text/html; charset=utf-8\r\n"
              "Content-Length: " (number->string html-data-len) "\r\n"
              "\r\n"
              html-data
              )))
        (yield))  	
    (newline)))

(init-buf buf)

(let ([pid (setup-server-socket ip port)])
  (display "Server started at: ")
  (display ip)
  (display ":")
  (display port)
  (display " ……")
  (newline)
  (do ([client (accept-socket pid) (accept-socket pid)])
    ((= -1 client) (begin (display client) (break)))
    (fork (operation client))
  ))