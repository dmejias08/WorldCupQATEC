#lang racket

;; ########################################################
;; BinaryGenerator - auxiliar
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

;; ########################################################
;; CreatePlayersAttributes - auxiliar
;; it creates a list of attributes assign to each player
;; arguments:
;; numberOfAttributes -> the attributes each player is going to have
;;    - normally 3 attributes (velocity, force, hability)
;; return: list (velocity, force, hability)
;; ########################################################
(define (CreatePlayersAttributes numberOfAttributes)
  (cond
    ((zero? numberOfAttributes)
     '())
    (else
     (cons (BinaryGenerator 4) (CreatePlayersAttributes (- numberOfAttributes 1))))))

;; ########################################################
;; GenerateTeamsAttributes - auxiliar
;; it creates a list that has lists of three attributes each, and 
;; it represents a team
;; arguments:
;; numberOfPlayers -> the attributes each player is going to have
;;    - normally 11 players 
;; return: eleven lists of  (velocity, force, hability)
;; ########################################################
(define (GenerateTeamsAttributes numberOfPlayers)
  (cond
    ((zero? numberOfPlayers)
       '())
    (else
     (cons (CreatePlayersAttributes 3) (GenerateTeamsAttributes (- numberOfPlayers 1))))))

;; ########################################################
;; GeneratePopulationAttributes - auxiliar
;; it creates a list that has two sub lists, in which we have eleven
;; attributes lists
;; arguments:
;; teams -> the amount of teams
;;    - must be 2 
;; return: ((team 1) (team 2))
;; ########################################################
(define (GeneratePopulationAttributes teams)
  (cond
    ((zero? teams)
     '())
    (else
     (cons (GenerateTeamsAttributes 11) (GeneratePopulationAttributes (- teams 1))))))


;; ###########################
;; #### Genetic Algorithm ####
;; ###########################


;; ########################################################
;; QuickSort
;; It sorts a list using quicksort algorithm
;; arguments:
;; list: list of elements in disorder
;; Returns: a sorted list
;; ########################################################
(define (quicksort list)
  (cond ( (null? list)
          '())
        ( else
          (quicksort-aux (caar list) (car list) (cdr list)))))



;; ########################################################
;; QuickSort - Auxiliar
;; It is a quicksort auxiliar, that gets the pivot to compare
;; and a sublist
;; arguments:
;; list: list of elements in disorder
;; pivot: an interger to compare each elemento of the list 
;; Returns: a sorted list
;; ########################################################
(define (quicksort-aux pivot pivotList list1)
  (cond ( (null? list1)
          (list pivotList))
        ( else
          (append  (quicksort (larger-list pivot list1)) (list pivotList)  (quicksort (smaller-list pivot list1))) )))



;; ########################################################
;; Smaller list analizer
;; It a quicksort auxiliar that analize numbers less than the pivot
;; arguments:
;; list: list of elements in disorder
;; pivot: an interger to compare each elemento of the list 
;; Returns: a sorted list 
;; ########################################################
(define (smaller-list pivot list)
  (cond ( (null? list)
          '())
        ( (< (caar list) pivot)
          (cons (car list) (smaller-list pivot (cdr list))))
        ( else
          (smaller-list pivot  (cdr list)))))



;; ########################################################
;; Larger list analizer
;; It a quicksort auxiliar that analize numbers greater than the pivot
;; arguments:
;; list: list of elements in disorder
;; pivot: an interger to compare each elemento of the list 
;; Returns: a sorted list 
;; ########################################################
(define (larger-list pivot list)
  (cond ( (null? list)
          '())
        ( (>= (caar list) pivot)
          (cons (car list) (larger-list pivot (cdr list))))
        ( else
          (larger-list pivot (cdr list)))))



;; ########################################################
;; QuickSort
;; It sorts a list using quicksort algorithm
;; arguments:
;; population: list of two teams
;; Returns: a sorted team
;; ########################################################
(define (SortList population)
  (cond
    ((null? population)
     '())
    (else
     (cons (quicksort (car population)) (SortList (cdr population))))))


;; ########################################################
;; Fitness
;; It takes a random bit and exchange it to its contrary
;; arguments:
;; player -> a member of the population who will mutate
;; return: ((velocity) (force) (hability))
;; ########################################################

(define (Fitness population)
  ;;REFRESH
  (cond
    ((null? population)
     '())
    (else
     (cons (FitnessTeam (car population)) (Fitness (cdr population))))))

(define (FitnessTeam team)
  (cond
    ((null? team)
     '())
    (else
     (cons (FitnessPlayer (car team) '()) (FitnessTeam (cdr team))))))

(define (FitnessPlayer player resultPlayer)
  (cond
    ((not (list? (car player)))
     (cons (FitnessPlayerAux resultPlayer) resultPlayer))
    (else
     (FitnessPlayer (cdr player) (cons (car player) resultPlayer)))))



(define (FitnessPlayerAux player)
  (cond
    ((null? player)
     0)
    (else ( + (CountOnes (car player)) (FitnessPlayerAux (cdr player))))))


(define (CountOnes attribute)
  (cond
    ((null? attribute)
     0)
    ((equal? (car attribute) 1)
     (+ 1 (CountOnes (cdr attribute))))
    (else
     (CountOnes (cdr attribute)))))




(define (Selection population selectedNum )
  (cond
    ((null? population)
     '())
    (else
     (cons (SelectionAux (car population) selectedNum ) (Selection (cdr population) selectedNum) ))))



 (define (SelectionAux team selectedNum)
   (cond
     ((zero? selectedNum)
      '())
     (else
      (cons (car team) (SelectionAux (cdr team) (- selectedNum 1))))))


;; ########################################################
;; Reproduction
;; It takes the selected population of 6 individuals and creates five more individuals
;; arguments:
;; population -> 6 indiviiduals
;; returns -> List with two teams with all their members
;; ########################################################
(define (Reproduction population)
  (cond
    ((null? population)
     '())
    (else
     (cons (ReproductionTeams (car population) (car population)) (Reproduction (cdr population)) ))))



;; ########################################################
;; Reproduction Teams
;; It creates the final team with the beset 6 players and then reproduce it
;; arguments:
;; team -> initial team to iterate over it
;; teamCopy -> a copy of the selected players
;; returns -> List with one team that has all members
;; ########################################################
(define (ReproductionTeams team teamCopy)
  (cond
  ((equal? (cdr team) '())
   (ReproductionTeamsAux teamCopy))
  (else
   (cons (Mutation (ReproductionPlayers (cdr (car team)) (cdr (car (cdr team))))) (ReproductionTeams (cdr team) teamCopy)))))

(define (ReproductionTeamsAux team)
  (cond
    ((null? team)
     '())
    (else
     (cons (Mutation (cdr (car team))) (ReproductionTeamsAux (cdr team))))))

;; ########################################################
;; Reproduction Players
;; It takes two players and combine them 
;; arguments:
;; player1, player2 -> two players 
;; returns -> A list of one player 
;; ########################################################
(define (ReproductionPlayers player1 player2)
  (cond
    ((and (null? player1) (null? player2))
     '())
    (else
     (cons (Combination (car player1) (car player2) 1) (ReproductionPlayers (cdr player1) (cdr player2))))))



;; ########################################################
;; Combination
;; It takes attributes of two players and combines it 
;; arguments:
;; attribute1, attribute2 -> two attribute's players
;; returns -> a list of binary
;; ########################################################
(define (Combination attribute1 attribute2 times)
  (cond
    ((zero? times)
     '())
    (else
     ( append (list (car attribute1) (car(cdr attribute1)) (car attribute2) (car(cdr attribute2))) (Combination attribute1 attribute2 (- times 1)) ))))


;;(ReproductionPlayers '((1 0 1 1) (1 1 0 0) (1 1 1 0)) '((0 1 0 ) (1 0 0 0) (0 0 0 0)))

;; ########################################################
;; Mutation
;; It takes a random bit and exchange it to its contrary
;; arguments:
;; player -> a member of the population who will mutate
;; return: ((velocity) (force) (hability))
;; ########################################################
(define (Mutation player)
  (cond
  ((null? player)
   '())
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
;; attributes -> attributes for each team 
;; return: ((team 1) (team2))
;; ########################################################
(define (Population teams defenders1 midFilders1 forwards1 defenders2 midFilders2 forwards2 attributes)
  (cond 
    ((null? attributes)
    '())
    ((equal? teams 2)
    (cons (Team defenders1 midFilders1 forwards1 (car attributes)) (Population (- teams 1) defenders1 midFilders1 forwards1 defenders2 midFilders2 forwards2 (cdr attributes))))
    ((equal? teams 1)
    (cons (Team defenders2 midFilders2 forwards2 (car attributes)) (Population (- teams 1) defenders1 midFilders1 forwards1 defenders2 midFilders2 forwards2 (cdr attributes))))
    ))
;; ########################################################
;; Team
;; It creates an individual team with its line up
;; arguments:
;; defenders
;; midFilders
;; forwards
;; attributes -> attributes for each player
;; return: ((player1)(player2)(player3)(player4)(player5)(player6)(player7)(player8)(player9)(player10)(player11))
;; ########################################################
(define (Team defenders midFilders forwards attributes)
  (cond
    ((and (zero? defenders) (zero? midFilders) (zero? forwards))
    (list (Player 1 (car attributes)))
    )
    ((> defenders 0)
    (cons (Player 2 (car attributes)) (Team (- defenders 1) midFilders forwards (cdr attributes)))
    )
    ((> midFilders 0)
    (cons (Player 3 (car attributes)) (Team defenders (- midFilders 1) forwards (cdr attributes)))
    )
    ((> forwards 0)
    (cons (Player 4 (car attributes)) (Team  defenders midFilders (- forwards 1) (cdr attributes)))
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
;; attributes -> attributes for each player
;; return: ((velocity)(force)(hability) playerId)
;; ########################################################
(define (Player playerId attributes)
  (cond
  ((null? attributes)
  (list playerId))
  (else
   (cons (car attributes) (Player playerId (cdr attributes))))))

;;(Reproduction '(((10 (0 1 1 1) (1 1 1 1) (0 1 1 1))
  ;; (8 (1 1 1 0) (1 0 1 1) (1 1 0 0))
  ;;(7 (1 1 0 0) (1 1 0 1) (0 1 1 0))
  ;;(7 (1 1 1 1) (1 1 0 1) (0 0 0 0))
  ;;(7 (1 0 1 1) (1 1 1 0) (0 0 1 0))
  ;;(7 (1 0 1 1) (1 1 0 1) (0 1 0 0)))
  ;;((9 (1 1 1 0) (1 0 1 1) (1 1 0 1))
  ;;(8 (0 1 1 1) (1 0 1 0) (1 1 1 0))
  ;;(8 (1 0 0 1) (1 1 1 1) (0 0 1 1))
  ;;(8 (1 1 0 1) (1 0 1 1) (0 0 1 1))
  ;;(6 (1 1 0 0) (1 1 0 0) (0 1 1 0)) 
  ;;(6 (1 0 0 0) (1 0 0 1) (0 1 1 1)))))

(Population 2 4 4 2 5 3 2(Reproduction (Selection (SortList (Fitness(Population 2 4 4 2 5 3 2 (Reproduction (Selection (SortList (Fitness (Population 2 4 4 2 5 3 2 (GeneratePopulationAttributes 2)))) 6))))) 6)))
;;(Reproduction (Selection (SortList (Fitness (Population 2 4 4 2 5 3 2 (GeneratePopulationAttributes 2)))) 6))
;;(Selection (SortList (Fitness (Population 2 4 4 2 5 3 2 (GeneratePopulationAttributes 2)))) 6)
;; (Fitness (Population 2 4 4 2 5 3 2 (GeneratePopulationAttributes 2)))
;; (Fitness '((((1 1 1 1) (0 0 0 0) (1 0 1 0) 1) ( (1 1 1 0) (1 1 1 0) (1 1 1 0) 2)) (((0 0 0 0) (0 0 0 0) (0 0 0 0) 1 ) ((1 1 1 1) (1 1 1 1) (1 1 1 1) 2))))
;;(Population 2 4 4 2 5 3 2 (Reproduction (Selection (SortList (Fitness (Population 2 4 4 2 5 3 2 (GeneratePopulationAttributes 2)))) 6)))