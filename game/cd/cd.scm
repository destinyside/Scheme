(define *db* '())

(define (record lst)
  (set! *db* (append *db* (list lst))))

(define (add-record title artist rating ripped)
  (record (list (cons "Title"  title) (cons "Artist"  artist) (cons "Rating"  rating) (cons "Ripped"  ripped))))

(define (y-or-n?)
  (define ans (read))
  (set! ans (string-ref (symbol->string ans) 0))
  (case ans
    ((#\y #\Y) #t)
    ((#\n #\n) #f)))

(define (add-items item op)
  (display item)
  (display " : ")
  (newline)
  (op))

(define (add-cds)
  (add-record
    (add-items "Title" read)
    (add-items "Artist" read)
    (add-items "Rating" read)
    (add-items "Ripped" y-or-n?))
  (display "Another ? [y/n]")
  (newline)
  (if (y-or-n?)
    (add-cds)))

(define (show-cds)
  ;(display *db*)
  (for-each
    (lambda (x)
      (for-each
	(lambda (y)
	  (display (car y))
	  (display " : ")
	  (display (cdr y))
	  (newline))
	x)
      (newline))
    *db*))

(define (file rw name)
  (case rw
    ((r) (open-input-file name))
    ((w) (open-output-file name))
    (else
      (error "Unknown op"))))
(define (save-cds filename)
  (define f (file 'w filename))
  (for-each
    (lambda (x)
      (write x f)
      (newline f))
    *db*)
  (close-output-port f))

(define (load-cds filename)
  (define f (file 'r filename))
  (define (iter)
    (define tmp (read f))
    (if (not (eof-object? tmp))
      (begin
	;(display tmp)
	(set! *db* (append *db* (list tmp)))
	(iter))
      (close-input-port f)))
  (iter))

(load-cds "cd.db")
