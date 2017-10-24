(define k (read))
(cond
  ((string? k) k)
  (else (display "kkk"))
  )
(case (car k)
  (('_+) (display "hhhjk"))
  (else (display "nnn"))
  )
