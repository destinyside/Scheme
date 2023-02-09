#lang racket

(define/contract (int-to-roman num)
  (-> exact-integer? string?)
  (cond
    ((>= num 1000) (~a "M" (int-to-roman (- num 1000))))
    ((>= num 900) (~a "CM" (int-to-roman (- num 900))))
    ((>= num 500) (~a "D" (int-to-roman (- num 500))))
    ((>= num 400) (~a "CD" (int-to-roman (- num 400))))
    ((>= num 100) (~a "C" (int-to-roman (- num 100))))
    ((>= num 90) (~a "XC" (int-to-roman (- num 90))))
    ((>= num 50) (~a "L" (int-to-roman (- num 50))))
    ((>= num 40) (~a "XL" (int-to-roman (- num 40))))
    ((>= num 10) (~a "X" (int-to-roman (- num 10))))
    ((>= num 9) (~a "IX" (int-to-roman (- num 9))))
    ((>= num 5) (~a "V" (int-to-roman (- num 5))))
    ((>= num 4) (~a "IV" (int-to-roman (- num 4))))
    ((>= num 1) (~a "I" (int-to-roman (- num 1))))
    (#t "")
    )
  )

(define/contract (roman-letter ch pre)
  (-> char-alphabetic? char-alphabetic? exact-integer?)
  (let* ([ch1 (char-upcase ch)]
         [pre1 (char-upcase pre)]
         [lst '((#\I 1) (#\V 5) (#\X 10) (#\L 50) (#\C 100) (#\D 500) (#\M 1000))]
         [pre-lst '((#\I (#\V -1) (#\X -1)) (#\X (#\L -10) (#\C -10)) (#\C (#\D -100) (#\M -100)))]
         [ch2 (assoc ch1 lst)]
         [ch3 (assoc ch1 pre-lst)]
         [ch4 (assoc pre1 lst)]
         [pre2 (if ch3 (assoc pre1 (cdr ch3)) #f)]
         [val (if pre2 (cadr pre2) (if ch2 (cadr ch2) 0))]
         )
    val)
  )

(define/contract (roman-to-int s)
  (-> string? exact-integer?)
  (let ([lst (string->list s)])
    ;(display (~a (map roman-letter (append '(#\N) lst) (append lst '(#\N))) "\n"))
    (apply + (map roman-letter (append '(#\N) lst) (append lst '(#\N))))
    )
  )


(let* ([str "CMXIII"]
       [l (string->list str)]
       [l1 (list (int-to-roman 3999) (int-to-roman 1234))])
  (display (cons "\n" (map (lambda (n) (~a n "\t" (roman-to-int n) " \n") ) l1 )))
  )