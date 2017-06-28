;; an interpreter of c0

;; for racket
;; #lang r5rs

;; for guile
(define eval primitive-eval)

(define (make-stack) '())

(define-syntax push
  (syntax-rules ()
    ((_) #f)
    ((_ stack) #f)
    ((_ stack value ...)
     (set! stack (append (list value ...) stack)))
    ))

(define-syntax pop
  (syntax-rules
    ()
    ((_) #f)
    ((_ stack)
     (cond
       ((null? stack) #f)
       (else
	 (begin
	   (let ((i (car stack)))
	     (set! stack (cdr stack))
	     i
	     )
	   )
	 )
       ))))

(define (empty? stack)
  (null? stack))

(define (file rw filename)
  (case rw
    ((r) 
     (open-input-file filename))
    ((w)
     (open-output-file filename))
    (else
      (display "Unknown operation"))))


(define v 0)

(define (make-list num)
  (set! v (make-vector num))
  (vector-fill! v 0)
  (vector->list v))

(define (list-set! list1 i value)
  (set! v list1)
  (if (list? list1) 
    (begin
      (set! v (list->vector list1))
      (vector-set! v i value)
      (set! v (vector->list v))
      ))
  v
  )
(define ins '())
(define ptr #f)
(define iter 0)
(define len 0)

(define stk '())
(define top '())
(define global '())

(define (wrt lv act)
  (display "Stack : ")
  (display stk)
  (newline)
  (display "Top : ")
  (display top)
  (newline)
  (newline)
  )

(define (int lv act)
  (case lv
    ((0)
     (push stk (make-list act)))
    ((1)
     (set! global (make-list act))))
  )

(define (red lv act)
  (push top (read))
  )
(define (lit lv act)
  (push top act))

(define (lod lv act)
  (case lv
    ((0)
       (push top (list-ref (car stk) act)))
    ((1)
     (push top (list-ref global act)))))

(define now '())
(define now-top '())

(define (sto lv act)
  (case lv
    ((0) 
     (begin
       (set! now (pop stk))
       (set! now-top (pop top))
       (push stk (list-set! now act now-top))))
    ((1)
     (begin
       (set! now-top (pop top))
       (set! global (list-set! global act now-top))))))

(define (ret lv act)
  (pop stk)
  (if (empty? stk)
    (set! iter len))
  (set! ptr (list-ref ins iter))
  (eval ptr)
  )

(define (add lv act)
  (push top (+ (pop top) (pop top))))
(define (sub lv act)
  (push top (- (pop top) (pop top))))
(define (mul lv act)
  (push top (* (pop top) (pop top))))
(define (div lv act)
  (push top (/ (pop top) (pop top))))

(define (jmp lv act)
  (set! iter act))

(define (jpc lv act)
  (if (not (= 0 (car top)))
    (set! iter act)))

(define (load-oprs filepath)
  (define opr (file 'r filepath))
  (do ((op 0 (read opr)) 
       (lv 0 (read opr)) 
       (act 0 (read opr)))
    ((eof-object? op) (close-input-port opr))
    (push ins (list op lv act))))

(define (exec)
  (load-oprs "oprs")
  (set! ins (reverse ins))
  (pop ins)
  (set! len (length ins))
  (do ((i 0 (+ i 0)))
    ((>= iter len))
    (set! ptr (list-ref ins iter))
    (set! iter (+ iter 1)) 
    (display ptr)
    (newline)
    (eval ptr)
    )
  )

(exec)


