;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      CS 3 Scheme programming assignment               ;;;;
;;;;                 April 2016                            ;;;;
;;;;     Michael Harrison's solutions HRRMIC014            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  
;; This depends on "cs3-black-jack.scm"
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Question 1.  Code for "best-hand"

;; A version of best-hand
;; Decides whether it's best to assign aces to 1 or 11
;; Returns the best total value of the player's hand

;; > (best-hand '((A d) (8 h)))
;; 19

;; > (best-hand '((A d) (8 h) (5 s)))
;; 14

;; > (best-hand '((A d) (8 h) (A s)))
;; 20

(define (best-hand hand)
  (if (> (min-val hand) 11)
      (min-val hand)
      (if (let contains-a? ((h hand))             ;checks if the player has an ace
	    (if (eqv? h '())
		#f
		(if (eqv? (car (car h)) 'A)
		    #t
		    (contains-a? (cdr h)))))
	  (+ (min-val hand) 10)
	  (min-val hand))))
		      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Question 2.  Code for "stop-at"

;; Returns a function
;; The returned function is a strategy that stops at the value of the input argument

;; > ((stop-at 17) '((A d) (8 h)) '(5 s))
;; #f

;; > ((stop-at 17) '((6 d) (8 h)) '(5 s))
;; #t

;; > ((stop-at 19) '((A d) (7 h)) '(5 s))
;; #t

(define (stop-at n)
  (lambda (player-hand up-card)
    (< (best-hand player-hand) n)))
		      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Question 3.  Code for "repeat-game"

;; Replays a game a given number n times
;; The games are played with the strategy argument
;; Returns games won minus games lost

;; > (repeat-game (stop-at 18) 3)
;; -3

;; > (repeat-game (stop-at 17) 4)
;; -2

(define (repeat-game strategy n)
  (let repeater ((n n)                   ; Create a recursive function that repeats for n
		 (score 0))
    (if (= n 0)
	score
	(let ((the-deck (shuffled-pack)))
	   (let ((player-hand (list (car the-deck) (cadr the-deck)))
		 (dealer-up-card (caddr the-deck))
		 (rest-of-deck (cdddr the-deck)))
      
	     (let ((outcome (play 
			     strategy 
			     player-hand 
			     dealer-up-card 
			     rest-of-deck)))	
	       (if (> (best-hand (car outcome)) 21)
		   (repeater (- n 1) (- score 1))                     ; Player bust (loses)
		   (let ((dealer-hand (play-dealer
				       (list dealer-up-card) 
				       (cdr outcome))))
		     (cond ((> (best-hand dealer-hand) 21)
			    (repeater (- n 1) (+ score 1)))           ; Dealer Bust, player wins
			   ((< (best-hand (car outcome)) 
			       (best-hand dealer-hand)) (repeater (- n 1) (- score 1))) ; Dealer wins, player loses
			   ((> (best-hand (car outcome)) 
			       (best-hand dealer-hand)) (repeater (- n 1) (+ score 1))) ; Player wins
			   (else (repeater (- n 1) score)))))))))))    ; Draw


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Question 4.  Code for "clever"

;; A strategy that takes the player's cards and the dealer's into account
;; Decides whether to hit based on different player's and dealer's cards

;; > (clever '((5 c) (4 d)) '(9 h))
;; #t

;; > (clever '((K c) (8 d)) '(9 h))
;; #f

;; > (clever '((5 c) (7 d)) '(5 h))
;; #f

;; > (clever '((5 c) (7 d)) '(7 h))
;; #t

;; > (clever '((5 c) (8 d)) '(9 h))
;; #t

;; > (clever '((5 c) (8 d)) '(A h))
;; #t

;; > (clever '((5 c) (8 d)) '(6 h))
;; #f

(define (clever player-hand up-card)
  (cond ((<= (min-val player-hand) 11)
	 #t)
	((>= (min-val player-hand) 17)
	 #f)
	((= (min-val player-hand) 12)
	 (if (or (= (min-val (list up-card)) 4)
		 (= (min-val (list up-card)) 5)
		 (= (min-val (list up-card)) 6))
	     #f
	     #t))
	((and (>= (min-val player-hand) 13) (<= (min-val player-hand) 16))
	 (if (or (>= (min-val (list up-card)) 7) (eqv? (car up-card) 'A))
	     #t
	     #f))))
	 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Question 5.  Code for "majority"

;; Takes the majority result of 3 strategies
;; Returns a function that makes the same decision as the majority for the same values

;; > ((majority (stop-at 19) (stop-at 17) (stop-at 13)) '((Q d) (5 h)) '(8 s))
;; #t

;; > ((majority (stop-at 19) clever (stop-at 13)) '((Q d) (5 h)) '(6 s))
;; #f

(define (majority strategy-1 strategy-2 strategy-3)
  (lambda (player-hand up-card)
    (if (or (and (strategy-1 player-hand up-card) (strategy-2 player-hand up-card))
	    (and (strategy-1 player-hand up-card) (strategy-3 player-hand up-card))
	    (and (strategy-2 player-hand up-card) (strategy-3 player-hand up-card)))
	#t
	#f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Question 6.  Code for "get-stats"

;; An extension of repeat-game
;; Gets final score of repeated games multiple times and returns list of scores

;; > (get-stats clever 100 10)
;; (-23 -10 -14 -4 -3 -1 -14 -6 -19 -7)

;; > (get-stats (stop-at 20) 50 8)
;; (-13 -11 -17 -23 -12 -17 -29 -21)

(define (get-stats strategy repeat-count data-points)
  (let list-adder ((dp data-points)
		   (data-list '()))
    (if (= dp 0)
	data-list
	(list-adder (- dp 1) (append data-list (list (let repeater ((n repeat-count)
								    (score 0))
						       (if (= n 0)
							   score 
							   (let ((the-deck (shuffled-pack)))
							     (let ((player-hand (list (car the-deck) (cadr the-deck)))
								   (dealer-up-card (caddr the-deck))
								   (rest-of-deck (cdddr the-deck)))
							       
							       (let ((outcome (play 
									       strategy 
									       player-hand 
									       dealer-up-card 
									       rest-of-deck)))	
								 (if (> (best-hand (car outcome)) 21)
								     (repeater (- n 1) (- score 1))                  ; Player bust (loses)
								     (let ((dealer-hand (play-dealer
											 (list dealer-up-card) 
											 (cdr outcome))))
								       (cond ((> (best-hand dealer-hand) 21)
									      (repeater (- n 1) (+ score 1)))	    ; Dealer Bust, player wins
									     ((< (best-hand (car outcome)) 
										 (best-hand dealer-hand)) (repeater (- n 1) (- score 1))) 	; Dealer wins, player loses
									     ((> (best-hand (car outcome)) 
										 (best-hand dealer-hand)) (repeater (- n 1) (+ score 1))) 	; Player wins	
									     (else (repeater (- n 1) score))))))))))))))))                      ; Draw

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Question 8.  Code for "hit?"

;; Interactive version of the game
;; Asks user to hit or not

;; > (black-jack hit?)
;; Player hand: ((8 s) (10 s))
;; Dealer face-up card: (K c)
;; Player total: 18
;; Would you like to take a hit? (type y for yes and n for no) n
;; -1

;; > (black-jack hit?)
;; Player hand: ((6 d) (6 s))
;; Dealer face-up card: (K d)
;; Player total: 12
;; Would you like to take a hit? (type y for yes and n for no) y
;; Player hand: ((8 s) (6 d) (6 s))
;; Dealer face-up card: (K d)
;; Player total: 20
;; Would you like to take a hit? (type y for yes and n for no) n
;; 1

(define (hit? player-hand up-card)
  (begin
    (display "Player hand: ")
    (display player-hand)
    (newline)
    (display "Dealer face-up card: ")
    (display up-card)
    (newline)
    (display "Player total: ")
    (display (best-hand player-hand))
    (newline)
    (display "Would you like to take a hit? (type y for yes and n for no) ")
    (if (eqv? (read) 'y)
	#t
	#f)))
    
