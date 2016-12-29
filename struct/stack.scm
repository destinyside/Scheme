(define (stack)
  (define p '())
  (define (push x) (set! p (cons x p)))
  (define (pop)
    (begin 
      (display (car p))
      (set! p (cdr p))))
  (define (show) (display p))
  (lambda (selector . arg)
    (case selector
      ((push) (apply push arg))
      ((pop) (apply pop arg))
      ((show) (apply show arg))
      ((isempty) (eqv? '() p)))))
