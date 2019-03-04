
;;; basic http format:
;;; HTTP/1.1 200 OK\r\n
;;; Cache-Control: public, max-age=60\r\n
;;; Date: Sat, 10 Apr 2018 16:50:00 CST\r\n
;;; Expires: Sat, 10 Apr 2018 17:00:00 CST\r\n
;;; Vary: Accept-Encoding\r\n
;;; Content-Type: text/html; charset=utf-8\r\n
;;; Content-Length: 20\r\n
;;; \r\n
;;; html-data
;;;

;;; string->list format:
;;; #\G #\E #\T #\space #\/ #\f #\a #\v #\i #\c #\o #\n #\. #\i #\c #\o #\space #\H #\T #\T #\P #\/ #\1 #\. #\1 #\newline
;;; #\H #\o #\s #\t #\: #\space #\l #\o #\c #\a #\l #\h #\o #\s
;;; #\t #\: #\8 #\0 #\9 #\0 #\newline 
;;; #\C #\o #\n #\n #\e #\c #\t #\i #\o #\n #\: #\space #\k #\e #\e #\p #\- #\a #\l #\i #\v #\e #\newline 

(library
  (http-parser)
  (export http)
  (import (rnrs))
  (define http
    (lambda (data)
      (convert (string->list data))))
  ;(hashtable-set! headers 'a 73)

  (define convert 
    (lambda (data)
      (letrec ([iter (lambda (list-data temp-row rows)
		       (if (null? list-data) 
			 rows
			 (case (car list-data)
			   ((#\newline) (iter (cdr list-data) '() (append rows (list (list->string temp-row)))))
			   (else (iter (cdr list-data) (append temp-row (list (car list-data))) rows)))))]
	       [http-table (make-eq-hashtable)]
	       [rows '()])
	(set! rows (iter data '() '()))

	;(parse-first-row (car rows) http-table)
	;(display (hashtable-entries http-table))
	;(parse-header-row (cdr rows) http-table)
	;(parse-data-row (cdr rows))
	)))
  (define parse-first-row
    (lambda (row http-table)
      (let ([point 1]
	    [temp '()])
	(do ([index 0 (+ index 1)])
	  ((= index (string-length row)))
	  (case (string-ref row index)
	    ((#\space) (begin 
			 (cond
			   ((= point 1) (hashtable-set! http-table 'method (list->string temp)))
			   ((= point 2) (hashtable-set! http-table 'uri (list->string temp)))
			   (else #f))
			 (set! temp '())
			 (set! point (+ point 1))))
	    ((#\newline) (begin
			   (hashtable-set! http-table 'version (list->string temp))
			   (set! temp '())))
	    (else (append temp (list (string-ref row index)))))))))

  (define parse-header-row
    (lambda (rows http-table)
      '()))

  (define parse-data-row
    (lambda (rows)
      '()))


  )


