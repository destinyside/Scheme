
(library
  (html-parser)
  (export html-head)
  (import (rnrs))

  (define html-head
    (lambda (data)
      (string->list data)))


  )
