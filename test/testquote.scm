(define (p x)
  'x
  )
(define-syntax quo
  (lambda (x)
    (syntax-case x ()
                 ((_ name)
                  (with-syntax
                    ((nm (syntax->datum #'name)))
                    #'(list nm 123)
                    )
                  )
                 )
    )
  )
