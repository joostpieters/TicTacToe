; Tic tac toe
; The following scheme code implements a command line tic tac toe game
; Ron Mackenzie

(define board '((1 2 3) (4 5 6) (7 8 9) (1 4 7) (2 5 8) (3 6 9) (1 5 9) (3 5 7)))

(define start
	(lambda (p)
		(if (equal? p 1)
			(pprint-board board)
			)	
		(if 
			(> p 9)
			(and (display "Game over, nobody wins!")(newline))
			(and
				(if (equal? (modulo p 2) 0)
					(and (display "Player 2, place an O from 1-9")(newline))
					(and (display "Player 1, place an X from 1-9")(newline))
				)
				(let ((move (read)))		
					(if
						(number? move)
						(if
							(valid? move)
							; if the player enters a number 1-9, then make the move and print the board
							(and 
								(if (equal? (modulo p 2) 0)
									(and 
										(set! board (subst 'O move board))
										(pprint-board board)
										(if (check-win? board)
											(and
												(display "Congratulations!! Player 2 is the winner!")(newline)
												)
											(start (+ p 1))
											)
										)
									(and
										(set! board (subst 'X move board))
										(pprint-board board)
										(if (check-win? board)
											(and
												(display "Congratulations!! Player 1 is the winner!")(newline)
												)
											(start (+ p 1))
											)
										)
									)
								)
							; otherwise, he/she failed, start another turn for that player
							(and 
								(display "Please enter a number between 1 and 9, not already played")(newline)
								(start p)
								)
							)
						(and 
							(display "Please enter a number between 1 and 9, not already played")(newline)
							(start p)
							)
						)
					)
				)
			)
		)
	)

(define check-win?
	(lambda (board)
		(cond 
			[(member? '(X X X) board) #t]
			[(member? '(O O O) board) #t]
			[else #f]
			)	
		)
	)

(define subst 
  (lambda (new old l) 
    (cond ((null? l) (quote ())) 
              ((atom? (car l)) 
                 (cond ((eq? (car l) old) 
                              (cons new (subst new old (cdr l)))) 
                           (else (cons (car l) (subst new old (cdr l)))))) 
               (else (cons (subst new old (car l)) 
                                 (subst new old (cdr l)))))))


(define (atom? x) (not (or (pair? x) (null? x))))

(define valid?
	(lambda (move)
		(if
			(and 
				(> move 0)
				(< move 10)
				(member? move board)
				)
			#t
			#f
			)	
		)	
	)

(define member?
	(lambda (e set)
		(if (null? e) #f
			(if (null? set) #f
				(if (equal? e (car set))
					#t
					(if (list? (car set))
						(if (member? e (car set))
							#t
							(member? e (cdr set))	
							)
						(member? e (cdr set))
						)
					)
				)
			)
		)
	)

; didn't have time to implement a prettier solution :(
(define pprint-board
	(lambda b
		(display (car (car board)))
		(display "|")
		(display (cadr (car board)))
		(display "|")
		(display (caddr (car board)))
		(newline)
		(display "------")
		(newline)
		(display (car (cadr board)))
		(display "|")
		(display (cadr (cadr board)))
		(display "|")
		(display (caddr (cadr board)))
		(newline)
		(display "------")
		(newline)
		(display (car (caddr board)))
		(display "|")
		(display (cadr (caddr board)))
		(display "|")
		(display (caddr (caddr board)))
		(newline)
		(newline)
		)
	)
; program operation

(display "Let's play tic-tac-toe!")(newline)
(newline)
(start 1)

