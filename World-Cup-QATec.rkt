#lang racket


;; ########################################################
;; Auxiliar
;; It generate a list that represents a random binary number of 4 bits
;; arguments:
;; numberBits -> the amount of bits of each number. Normally four
;; return: list (bit bit bit bit)
;; ########################################################
(define (BinaryGenerator numberBits)
  (cond 
  ((zero? numberBits)
    '())
  (else
    (cons (random 2) (BinaryGenerator (- numberBits 1))))))


;; ###########################
;; #### Genetic Algorithm ####
;; ###########################

(define (Fitness population)
  (cond 
  ()))

;; ########################################################
;; Mutation
;; It takes a random bit and exchange it to its contrary
;; arguments:
;; player -> a member of the population who will mutate
;; return: ((velocity) (force) (hability) id)
;; ########################################################
(define (Mutation player)
  (cond 
  ((not (list? (car player)))
    (list (car player)))
  (else
    (cons (Mutation-aux (car player) (random 4) 0 #f) (Mutation (cdr player))))))

;; ########################################################
;; Mutation-Auxiliar
;; It looks a particular bit and change it 
;; arguments:
;; attribute -> attribute of each player, velocity, force or hability
;; mutationBit -> a number selected random
;; position ->  it starts from zero to keep count of position
;; flag -> a boolean to identify if position was found
;; return: (1 0 1 0) *example
;; ########################################################
(define (Mutation-aux attribute mutationBit position flag)
  (cond
  ((null? attribute) '())
  ((equal? flag #t)
  (cdr attribute))
  ((and (equal? mutationBit position) (equal? (car attribute) 1))
  (cons 0 (Mutation-aux attribute mutationBit position #t)))
  ((and (equal? mutationBit position) (equal? (car attribute) 0))
  (cons 1 (Mutation-aux attribute mutationBit position #t)))
  (else(cons (car attribute) (Mutation-aux (cdr attribute) mutationBit (+ position 1) flag)))))

;; ########################################################
;; Population
;; It creates the entire population based on team line up
;; arguments:
;; teams -> the amount of teams, it has to be two
;; defenders1 -> defenders of team 1
;; midFilders1 -> midFilders of team 1
;; forwards1 -> forwards of team 1
;; defenders2 -> defenders of team 2
;; midFilders2 -> midFilders of team 2
;; forwards2 -> forwards of team 2
;; return: ((team 1) (team2))
;; ########################################################
(define (Population teams defenders1 midFilders1 forwards1 defenders2 midFilders2 forwards2  )
  (cond 
    ((zero? teams)
    '())
    ((equal? teams 2)
    (cons (Team defenders1 midFilders1 forwards1) (Population (- teams 1) defenders1 midFilders1 forwards1 defenders2 midFilders2 forwards2)))
    ((equal? teams 1)
    (cons (Team defenders2 midFilders2 forwards2) (Population (- teams 1) defenders1 midFilders1 forwards1 defenders2 midFilders2 forwards2)))
    ))

;; ########################################################
;; Team
;; It creates an individual team with its line up
;; arguments:
;; defenders
;; midFilders
;; forwards
;; return: ((player1)(player2)(player3)(player4)(player5)(player6)(player7)(player8)(player9)(player10)(player11))
;; ########################################################
(define (Team defenders midFilders forwards)
  (cond
    ((and (zero? defenders) (zero? midFilders) (zero? forwards))
    (list (Player 1 3))
    )
    ((> defenders 0)
    (cons (Player 2 3) (Team (- defenders 1) midFilders forwards))
    )
    ((> midFilders 0)
    (cons (Player 3 3) (Team defenders (- midFilders 1) forwards))
    )
    ((> forwards 0)
    (cons (Player 4 3) (Team  defenders midFilders (- forwards 1)))
    )
    ))

;; ########################################################
;; Player
;; It created a player with its attributes 
;; arguments:
;; playerId -> it indicates which type of player is
;;     goalKeeper: 1
;;     defenders: 2
;;     midFilders: 3
;;     forwards: 4
;; cycles -> the amount of attibutes each player is going to have
;; cycles = 3
;; return: ((velocity)(force)(hability) playerId)
;; ########################################################
(define (Player playerId cycles gen)
  (cond
  ((zero? cycles)
  (list playerId))
  ((equal? gen 1)
  (cons (BinaryGenerator 4) (Player playerId (- cycles 1))))
  (else(

  )
  ))
  )





(Population 2 4 4 2 5 3 2)