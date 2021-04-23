
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
  (export http http?)
  (import (rnrs))
  (define http
    (lambda (data)
      (convert (string->list data))))
  ;(hashtable-set! headers 'a 73)

  ;; todo
  (define http?
	(lambda (data)
		(and (not (or (null? data) (string=? "" data))) (pair? (string->list data)) )
	)
  )

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

	(parse-first-row (car rows) http-table)
	;(display (hashtable-entries http-table))
	(parse-header-rows (cdr rows) http-table)
	;;(newline)
	;;(map (lambda (x) (begin (display "|->") (display x) (newline))) (cdr rows))
	;;(pretty-out http-table)
	http-table
	;(parse-data-row (cdr rows))
	)))

  (define parse-first-row
    (lambda (row http-table)
      (let ([point 1]
	    [temp '()])
	(do ([index 0 (+ index 1)])
	  ((= index (string-length row)) 'first-row-end)
	  (case (string-ref row index)
	    ((#\space) (begin 
			 (cond
			   ((= point 1) (hashtable-set! http-table 'method (list->string temp)))
			   ((= point 2) (hashtable-set! http-table 'uri (list->string temp)))
			   ((= point 3) (hashtable-set! http-table 'version (list->string temp)))
			   (else #f))
			 (set! temp '())
			 (set! point (+ point 1))))
	    ((#\return) (begin
			  (hashtable-set! http-table 'version (list->string temp))))
	    (else (set! temp (append temp (list (string-ref row index))))))
	  )
	;;http-table
	)))

  (define parse-header-rows
    (lambda (rows http-table)
      (if (null? rows)  '()
	(begin
	  (let* ([point 0]
		 [temp '()]
		 [row (car rows)]
		 [row-length (string-length row)])
	    (do ([index 0 (+ index 1)])
	      ((or (= row-length 1) (= index row-length)) 'header-rows-end)
	      (case (string-ref row index)
		((#\:) (begin
			 (if (= point 0)
			   (begin
			     (parse-header-row (list->string temp) (substring row (+ index 1) (string-length row)) http-table)
			     (set! point (+ point 1))
			     ) '() 
			   )))
		((#\space) 'header-rows-space)
		(else (set! temp (append temp (list (string-ref row index))))))))
	  (parse-header-rows (cdr rows) http-table)
	))))

  (define parse-header-row
    (lambda (header-name header-value http-table)
      (letrec ([iter (lambda (char-list temp value-list)
		       (if (null? char-list)
			 (append value-list (list (list->string temp)))
			 (case (car char-list)
			   ((#\,) (iter (cdr char-list) '() (append value-list (list (list->string temp)))))
			   ((#\space) (iter (cdr char-list) temp value-list))
			   ((#\return) (iter (cdr char-list) temp value-list))
			   (else (iter (cdr char-list) (append temp (list (car char-list))) value-list)))))]
	       [header-values '()])
	;; (display "|")
	;; (display header-name)
	;; (display "|")
	;; (newline)
	(case (string-upcase header-name) 
	  (("USER-AGENT")
	   (hashtable-set! http-table (string->symbol header-name) header-value))
	  (else (begin
		  (set! header-values (iter (string->list header-value) '() '()))
		  (hashtable-set! http-table (string->symbol header-name) header-values))))
		;; header-values
	)))

  (define pretty-out
    (lambda (http-table)
      (newline)
      (letrec ([iter (lambda (lst)
		       (if (null? lst)
			 (begin
			   (display 'end)
			   (newline))
			 (begin
			   (display (car lst))
			   (display " ")
			   (display (hashtable-ref http-table (car lst) #f))
			   (newline)
			   (iter (cdr lst)))))])
	(iter (vector->list (hashtable-keys http-table))))))

  )
