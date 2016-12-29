;In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way:
;High Card: Highest value card.
;One Pair: Two cards of the same value.
;Two Pairs: Two different pairs.
;Three of a Kind: Three cards of the same value.
;Straight: All cards are consecutive values.
;Flush: All cards of the same suit.
;Full House: Three of a kind and a pair.
;Four of a Kind: Four cards of the same value.
;Straight Flush: All cards are consecutive values of same suit.
;Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
;The cards are valued in the order:
;2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.
;If two players have the same ranked hands then the rank made up of the highest value wins; for example, a pair of eights beats a pair of fives (see example 1 below). But if two ranks tie, for example, both players have a pair of queens, then highest cards in each hand are compared (see example 4 below); if the highest cards tie then the next highest cards are compared, and so on.
;Consider the following five hands dealt to two players:
;Hand Player 1 Player 2 Winner
;1 5H 5C 6S 7S KD
;Pair of Fives
;2C 3S 8S 8D TD
;Pair of Eights
;Player 2
;2 5D 8C 9S JS AC
;Highest card Ace
;2C 5C 7D 8S QH
;Highest card Queen
;Player 1
;3 2D 9C AS AH AC
;Three Aces
;3D 6D 7D TD QD
;Flush with Diamonds
;Player 2
;4 4D 6S 9H QH QC
;Pair of Queens
;Highest card Nine
;3D 6D 7H QD QS
;Pair of Queens
;Highest card Seven
;Player 1
;5 2H 2D 4C 4D 4S
;Full House
;With Three Fours
;3C 3D 3S 9S 9D
;Full House
;with Three Threes
;Player 1
;The file, poker.txt, contains one-thousand random hands dealt to two players. Each line of the file contains ten cards (separated by a single space): the first five are Player 1's cards and the last five are Player 2's cards. You can assume that all hands are valid (no invalid characters or repeated cards), each player's hand is in no specific order, and in each hand there is a clear winner.
;How many hands does Player 1 win?

(define (sort k)
  (define vl (list->vector k))
  (define (s-iter i j tmp)
    (if (< i 5)
      (begin
	(if (< j 5)
	  (begin
	    (if (< (vector-ref vl i) (vector-ref vl j))
	      (begin
		(set! tmp (vector-ref vl i))
		(vector-set! vl i (vector-ref vl j))
		(vector-set! vl j tmp)))
	    (s-iter i (+ j 1) tmp)))
	(s-iter (+ i 1) j tmp))
      (begin
	(define lst (vector->list vl))
	lst 
	)))
  (s-iter 1 0 0))

(define (seq? l)
  (cond 
    ((eqv? l '()) #t)
    ((eqv? (cdr l) '()) #t)
    ((= (+ (car l) 1) (cadr l))
     (seq? (cdr l)))
    (else #f)))

(define (list-eq? a b)
  (cond
    ((and (eqv? a '()) (eqv? b '())) #t)
    ((eqv? (car a) (car b)) (list-eq? (cdr a) (cdr b)))
    (else #f)))

(define (high-card? l) #t)

(define (one-pair? l)
  (define cd (list (vector-ref l 0) (vector-ref l 3) (vector-ref l 6) (vector-ref l 9) (vector-ref l 12)))
  (define cd2 (list->vector (sort cd)))
  (if (or (= (vector-ref cd2 1) (vector-ref cd2 2)) 
	  (= (vector-ref cd2 3) (vector-ref cd2 4))
	  (= (vector-ref cd2 0) (vector-ref cd2 1)) 
	  (= (vector-ref cd2 2) (vector-ref cd2 3)))
    #t
    #f
    ))

(define (two-pairs? l)
  (define cd (list (vector-ref l 0) (vector-ref l 3) (vector-ref l 6) (vector-ref l 9) (vector-ref l 12)))
  (define cd2 (list->vector (sort cd)))
  (if (or (and (= (vector-ref cd2 1) (vector-ref cd2 2)) (= (vector-ref cd2 3) (vector-ref cd2 4)))
	  (and (= (vector-ref cd2 0) (vector-ref cd2 1)) (= (vector-ref cd2 2) (vector-ref cd2 3)))
	  (and (= (vector-ref cd2 0) (vector-ref cd2 1)) (= (vector-ref cd2 3) (vector-ref cd2 4))))
    #t
    #f
    ))

(define (three-of-a-kind? l)
  (define cd (list (vector-ref l 0) (vector-ref l 3) (vector-ref l 6) (vector-ref l 9) (vector-ref l 12)))
  (define cd2 (list->vector (sort cd)))
  (if (or (= (vector-ref cd2 2) (vector-ref cd2 3) (vector-ref cd2 4))
	  (= (vector-ref cd2 0) (vector-ref cd2 1) (vector-ref cd2 2))
	  (= (vector-ref cd2 1) (vector-ref cd2 2) (vector-ref cd2 3)))
    #t
    #f
    ))

(define (straight? l)
  (define cd (list (vector-ref l 0) (vector-ref l 3) (vector-ref l 6) (vector-ref l 9) (vector-ref l 12)))
  (if (seq? (sort cd))
    #t
    #f
    ))

(define (flush? l)
  (define clr (list (vector-ref l 1) (vector-ref l 4) (vector-ref l 7) (vector-ref l 10) (vector-ref l 13)))
    (if (or (list-eq? clr '(#\D #\D #\D #\D #\D))
	    (list-eq? clr '(#\S #\S #\S #\S #\S))
	    (list-eq? clr '(#\H #\H #\H #\H #\H))
	    (list-eq? clr '(#\C #\C #\C #\C #\C)))
      #t
      #f
      )
    #f
    )

(define (full-house? l) 
  (define cd (list (vector-ref l 0) (vector-ref l 3) (vector-ref l 6) (vector-ref l 9) (vector-ref l 12)))
  (define cd2 (list->vector (sort cd)))
  (if (or
	(and (= (vector-ref cd2 0) (vector-ref cd2 1))
	     (= (vector-ref cd2 2) (vector-ref cd2 3) (vector-ref cd2 4)))
	(and (= (vector-ref cd2 3) (vector-ref cd2 4))
	     (= (vector-ref cd2 0) (vector-ref cd2 1) (vector-ref cd2 2))))
    #t
    #f
    ))

(define (four-of-a-kind? l)
  (define cd (list (vector-ref l 0) (vector-ref l 3) (vector-ref l 6) (vector-ref l 9) (vector-ref l 12)))
  (define cd2 (list->vector (sort cd)))
  (if (or
	(= (vector-ref cd2 0) (vector-ref cd2 1) (vector-ref cd2 2) (vector-ref cd2 3))
	(= (vector-ref cd2 1) (vector-ref cd2 2) (vector-ref cd2 3) (vector-ref cd2 4)))
    #t
    #f
    ))

(define (straight-flush? l)
  (define cd (list (vector-ref l 0) (vector-ref l 3) (vector-ref l 6) (vector-ref l 9) (vector-ref l 12)))
  (define clr (list (vector-ref l 1) (vector-ref l 4) (vector-ref l 7) (vector-ref l 10) (vector-ref l 13)))
  (if (seq? (sort cd))
    (if (or (list-eq? clr '(#\D #\D #\D #\D #\D))
	    (list-eq? clr '(#\S #\S #\S #\S #\S))
	    (list-eq? clr '(#\H #\H #\H #\H #\H))
	    (list-eq? clr '(#\C #\C #\C #\C #\C)))
      #t
      #f
      )
    #f
    )
  )

(define (royal-flush? l)
  (define cd (list (vector-ref l 0) (vector-ref l 3) (vector-ref l 6) (vector-ref l 9) (vector-ref l 12)))
  (define clr (list (vector-ref l 1) (vector-ref l 4) (vector-ref l 7) (vector-ref l 10) (vector-ref l 13)))
  (if (list-eq? (sort cd) '(10 11 12 13 14))
    (if (or (list-eq? clr '(#\D #\D #\D #\D #\D))
	    (list-eq? clr '(#\S #\S #\S #\S #\S))
	    (list-eq? clr '(#\H #\H #\H #\H #\H))
	    (list-eq? clr '(#\C #\C #\C #\C #\C)))
      #t
      #f
      )
    #f
    ))
(define (judge ab) 
  (define (change x)
    (case x
      ((#\2) 2)
      ((#\3) 3)
      ((#\4) 4)
      ((#\5) 5)
      ((#\6) 6)
      ((#\7) 7)
      ((#\8) 8)
      ((#\9) 9)
      ((#\T) 10)
      ((#\J) 11)
      ((#\Q) 12)
      ((#\K) 13)
      ((#\A) 14)
      (else x)))
  (set! l  (string->list ab))
  (set! l (map change l)) 
  (set! l (list->vector l))
  (cond
    ((royal-flush? l) 10)
    ((straight-flush? l) 9)                          
    ((four-of-a-kind? l) 8)                          
    ((full-house? l) 7)                              
    ((flush? l) 6)                                   
    ((straight? l) 5)                                
    ((three-of-a-kind? l) 4)                         
    ((two-pairs? l) 3)                               
    ((one-pair? l) 2)                                
    ((high-card? l) 1)                               
    (else 0)))
(define (get-cards)
  (define f (open-input-file "euler54_poker.txt"))
  (define (gc-iter awin bwin)
	(define line (read-line f))
	(if (not (eqv? #!eof line))
      (begin
	(define a (substring line 0 14))
	(define b (substring line 15 29))
	(define carda (judge a))
	(define cardb (judge b))
	(begin
	     (display "card a ")
	     (display a)
	     (display "  ")
	     (display carda)
	     (newline)
	     (display "card b ")
	     (display b)
	     (display "  ")
	     (display cardb)
	     (newline))
	(cond
	  ((> carda cardb) (gc-iter (+ awin 1) bwin))
	  ((= carda cardb) 
	     (gc-iter awin bwin))
	  ((< carda cardb) (gc-iter awin (+ bwin 1))))
	)
      (begin
	(display "player a win : ")
	(display awin)
	(newline)
	(display "player b win : ")
	(display bwin)
	(newline)
	)))
  (gc-iter 0 0)
  (close-input-port f)
  )
;(get-cards)

