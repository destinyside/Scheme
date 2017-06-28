

(define (old-list-set! lst idx value)
  (let ((v (list->vector lst)))
    (vector-set! v idx value)
    (vector->list v)))

(define (new-list-set! lst idx value)
  (cond
    ((or (< idx 0) (null? lst)) #f)
    ((= idx 0) (set-car! lst value))
    (else (new-list-set! (cdr lst) (- idx 1) value))))


