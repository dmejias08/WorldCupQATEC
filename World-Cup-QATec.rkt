#lang racket
;; First Gen

(define (Population teams defenders1 midFilders1 forwards1 defenders2 midFilders2 forwards2  )
  (cond 
    ((zero? teams)
    '())
    ((equal? teams 2)
    (cons (Team defenders2 midFilders2 forwards2) (Population (- teams 1) defenders1 midFilders1 forwards1 defenders2 midFilders2 forwards2)))
    ((equal? teams 1)
    (cons (Team defenders1 midFilders1 forwards1) (Population (- teams 1) defenders1 midFilders1 forwards1 defenders2 midFilders2 forwards2)))
    ))

(define (Team defenders midFilders forwards )
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


(define (BinaryGenerator numberBits)
  (cond 
  ((zero? numberBits)
    '())
  (else
    (cons (random 2) (BinaryGenerator (- numberBits 1))))))


(define (Player playerId cycles)
  (cond
  ((zero? cycles)
  (list playerId))
  (else
  (cons (BinaryGenerator 4) (Player playerId (- cycles 1)))))
  )

(Population 2 4 4 2 5 3 2)