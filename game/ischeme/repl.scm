(library (repl)
         (export i+ i- i* i/)
         (import (rnrs))

         (define-syntax i+
           (lambda (x)
             (syntax-case 
               x ()
               ((_ a b ...) #'(+ a b ...))
               )
             )
           )

         (define-syntax i-
           (lambda (x)
             (syntax-case 
               x ()
               ((_ a b ...) #'(- a b ...))
               )
             )
           )

         (define-syntax i*
           (lambda (x)
             (syntax-case 
               x ()
               ((_ a b ...) #'(* a b ...))
               )
             )
           )

         (define-syntax i/
           (lambda (x)
             (syntax-case 
               x ()
               ((_ a b ...) #'(/ a b ...))
               )
             )
           )
         )
