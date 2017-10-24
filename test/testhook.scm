(define hook (make-hook 2))
(add-hook! hook (lambda (x y)
                  (display "Foo: ")
                  (display (+ x y))
                  (newline)))
(add-hook! hook (lambda (x y)
                  (display "Bar: ")
                  (display (* x y))
                  (newline)))

(run-hook hook 3 4)

 
(set! hook (make-hook 3))
(add-hook! hook (lambda (x y z) (display (* x y z)))) 
(add-hook! hook (lambda (x y z) (display (+ x y z))))
(add-hook! hook (lambda (x y z) (display (max x y z))))
(run-hook hook 3 4 5)
