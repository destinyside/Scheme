
(load "list-set.scm")

(define (quicksort lst low high)
  (define first 0)
  (define last 0)
  (define key 0)
  (cond
    ((>= low high) lst)
    (else
      (set! first low)
      (set! last high)
      (set! key (list-ref lst first))
      (do ()
	((>= first last)
	 (begin
	   (new-list-set! lst first key)
	   (quicksort lst low (- first 1))
	   (quicksort lst (+ first 1) high)
	   )
	 )
	(do ()
	  ((not (and (< first last) (>= (list-ref lst last) key)))
	   (new-list-set! lst first (list-ref lst last)))
	  (set! last (- last 1)))
	(do ()
	  ((not (and (< first last) (<= (list-ref lst first) key)))
	   (new-list-set! lst last (list-ref lst first)))
	  (set! first (+ first 1)))

	)
      )
    )
  )
(define l '(2 3 1 5 4))
(quicksort l 0 (- (length l) 1))
(begin (display l) (newline))
