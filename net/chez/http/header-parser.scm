
(library
  (header-parser)
  (export header-type)
  (import (rnrs))

  (define header-type
    (lambda (data)
      (string->list data)))



  )
