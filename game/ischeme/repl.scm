(library (repl)
         (export _+ _- _* _/)
         (import (rnrs))

         (define-syntax _+
           (lambda (x)
             (syntax-case 
               x ()
               ((_ a b ...) #'(+ a b ...))
               )
             )
           )

         (define-syntax _-
           (lambda (x)
             (syntax-case 
               x ()
               ((_ a b ...) #'(- a b ...))
               )
             )
           )

         (define-syntax _*
           (lambda (x)
             (syntax-case 
               x ()
               ((_ a b ...) #'(* a b ...))
               )
             )
           )

         (define-syntax _/
           (lambda (x)
             (syntax-case 
               x ()
               ((_ a b ...) #'(/ a b ...))
               )
             )
           )
         )
