(use-modules 
  (web server) 
  (web request) 
  (web response) 
  (sxml simple)
  (web uri))  

(define (request-path-components request) 
  (split-and-decode-uri-path (uri-path (request-uri request))))  

(define (hello-hacker-handler request body) 
  (if (equal? (request-path-components request) '("hacker")) 
    (values '((content-type . (text/plain))) "Hello hacker!") 
    (values '((content-type . (text/plain))) (car (request-path-components request))) 
    )
  )  

(run-server hello-hacker-handler)
