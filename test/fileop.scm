(define (file rw filename)
  (case rw
    ((r) 
     (open-input-file filename))
    ((w)
     (open-output-file filename))
    (else
      (error "Unknown operation"))))
