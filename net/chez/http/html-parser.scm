
(library
  (html-parser)
  (export html-head ulist)
  (import (rnrs))


  (define ulist
    (lambda (http-table)
      (string-append "<ul>"
		     (apply string-append
			    (map (lambda (x) 
				   (string-append 
				     "<li>" 
				     (symbol->string x) 
				     ":"
				     (let ([value (hashtable-ref http-table x #f)])
				       (if (string? value)
					 value
					 (apply string-append
						(map (lambda (y) 
						       (if (string? y) y (apply string-append y))) value))))
				     "</li>")) 
				 (vector->list (hashtable-keys http-table))))
		     "</ul>")))

  (define html-head
    (lambda (data)
      (string->list data)))


  )
