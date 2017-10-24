
;let and let* are  different from their perform in cl
(let ((a 1)
      (b 2)
      (a 3))
  (display (+ a b))
  (newline))

(let* ((a 1)
      (b 2)
      (a 3))
  (display (+ a b))
  (newline))
