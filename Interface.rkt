#lang racket
(require (lib "graphics.ss" "graphics"))
(require plot racket/class racket/gui/base)
(require 2htdp/universe)
(open-graphics)

(provide (all-defined-out))

(define mainFrame (open-viewport "QATEC" 1200 700))
(define mainWindow (open-pixmap "QATEC" 1200 700))

((draw-pixmap mainWindow) "assets/futcourt.png" (make-posn 0 0))

; Function that draw players on screen
(define (Jugadores list color num)
  (cond ((>= (length list) 1)
             ((draw-solid-ellipse mainWindow) (make-posn (caar list) (last(car list))) 20 20 color)
             (Jugadores (cdr list) color (+ num 1))
             ((draw-string mainWindow) (make-posn (+ (caar list) 3) (+ (last(car list)) 15)) (number->string num)))))

; Function that draw ball on canvas
(define (Ball position)
  ((draw-solid-ellipse mainWindow) (make-posn (car position) (last position)) 20 20 "white"))

; Refresh function paints canvas in every iteration
(define (Refresh team1 team2 ball marker)
  ((draw-pixmap mainWindow) "assets/futcourt.png" (make-posn 0 0))
  ((draw-solid-rectangle mainWindow) (make-posn 555 0) 95 35 "white")
  ((draw-string mainWindow) (make-posn 590 20) (number->string (car marker)))
  ((draw-string mainWindow) (make-posn 610 20) (number->string (last marker)))
  (Jugadores team1 "blue" 1)
  (Jugadores team2 "red" 12)
  (Ball ball)
  (copy-viewport mainWindow mainFrame)
  ((clear-viewport mainWindow)))

; Auxiliar function
(define (len lista)
  (cond(
        (null? lista)
        0)
       (else
        (+ 1 (len (cdr lista))))))


;; It converts binary to an interget 
(define (BinaryToInterger binary lenOfNumber)
  (cond
    ((<= lenOfNumber 0)
     0)
    (else
     ( + ( * (car binary) (expt 2 (- lenOfNumber 1))) (BinaryToInterger (cdr binary) (- lenOfNumber 1))))))

; It modify coordenates in a certain index of list 
(define (ModifyList list index pair)
  (ModifyListAux list 1 index pair))

; Auxiliar function
(define (ModifyListAux list currentIndex finalIndex pair)
  (cond ((equal? currentIndex finalIndex)
        (cons pair (cdr list)))
        (else
         (cons (car list) (ModifyListAux (cdr list) (+ currentIndex 1) finalIndex pair ) ))))

; It returns true false is an individual is not moving 
(define (Moving? team1 team2)
        (cond
          ((and (null? team1) (null? team2))
           #f)
          ((and (< (abs (- (caar team1) (caar team2))) 5)  (< (abs (- (cadar team1) (cadar team2) 5)))
           (Moving? (cdr team1) (cdr team2))))
          (else
           #t)))

; It takes a list of positions and an position and determines if position is in positions
(define (Collisions positions pair)
  (cond ((null? positions)
         #f)
        ((and (< (abs (- (caar positions)(car pair))) 5 ) (< (abs (- (cadar positions)(cadr pair))) 5 ))
         #t)
        (else
         (Collisions (cdr positions) pair))))

; It takes a position and returns a list that will has the position 11 times
(define (FollowBall pair)
  (FollowBallAux pair '() 1))

; Auxiliar function for follow ball
(define (FollowBallAux pair list counter)
  (cond ((equal? counter 12)
         list)
        (else
         (FollowBallAux pair (cons pair list)(+ counter 1)))))

; Get player ids in a list 
(define (GetPlayersIds population teamNumber)
  (cond
    ((equal? teamNumber 1)
     (GetPlayersIdsAux (car population) '()))
    ((equal? teamNumber 2)
    (GetPlayersIdsAux (cadr  population) '()))))

; Auxiliar of get player ids
(define (GetPlayersIdsAux team result)
  (cond
    ((equal? (len team) 1)
     (cons (last (car team)) (reverse result)))
    (else
     (GetPlayersIdsAux (cdr team) (cons (last (car team)) result)))))

; get an specific aattribute of all players in each one team
(define (GetAttributes population team attribute)
  (cond
    ((equal? team 1)
     (GetAttributesAux (car population) attribute))
    (else
     (GetAttributesAux (cadr population) attribute))))

; Auxiliar of get attributes
(define (GetAttributesAux team attribute)
  (cond
    ((null? team)
     '())
    ((equal? attribute "velocity")
     (cons (GetPlayerAttribute (car team) 1 1) (GetAttributesAux (cdr team) attribute)))
    ((equal? attribute "strength")
     (cons (GetPlayerAttribute (car team) 2 1) (GetAttributesAux (cdr team) attribute)))
    ((equal? attribute "ability")
     (cons (GetPlayerAttribute (car team) 3 1) (GetAttributesAux (cdr team) attribute)))))

; Auxuliar of get attributes 
(define (GetPlayerAttribute player attributePos cont)
  (cond
    ((> cont 3)
     '())
    ((equal? attributePos cont)
      (BinaryToInterger (car player) (len (car player))))
    (else
     (GetPlayerAttribute (cdr player) attributePos (+ cont 1)))))

; Generate position of all players to allign them randomly
(define (GeneratePositions defenders1 midFielders1 forwards1 defenders2 midFielders2 forwards2)
  (cond
    ((and ( > (+ defenders1  midFielders1  forwards1) 11) ( > (+ defenders2 midFielders2 forwards2) 11))
     #f)
    ((and (zero? defenders1) (zero? midFielders1) (zero? forwards1) (zero? defenders2) (zero? midFielders2) (zero? forwards2))
     '())
    ((and (zero? defenders1) (zero? midFielders1) (zero? forwards1))
     (cons (cons (list 1105 340) (GeneratePosAuxTeam2 defenders2 midFielders2 forwards2)) (GeneratePositions  defenders1 midFielders1 forwards1 0 0 0) ))
    (else
     (cons (cons (list 70 340) (GeneratePosAuxTeam1 defenders1 midFielders1 forwards1)) (GeneratePositions 0 0 0  defenders2 midFielders2 forwards2) ))))

; Auxiliar of generate positions 
(define (GeneratePosAuxTeam1 defenders1 midFielders1 forwards1)
  (cond
    ((and (zero? defenders1) (zero? midFielders1) (zero? forwards1))
     '())
    ((not (zero? defenders1))
     (append (GeneratePosAuxDef1Mid2 150 defenders1 0) (GeneratePosAuxTeam1 0 midFielders1 forwards1)))
    ((not (zero? midFielders1))
     (append (GeneratePosAuxDef2Mid1 400 midFielders1 0) (GeneratePosAuxTeam1 defenders1 0 forwards1)))
    ((not (zero? forwards1))
     (append (GeneratePosAuxForwards 500 forwards1 0) (GeneratePosAuxTeam1 defenders1 midFielders1 0)))
  ))

; Auxiliar of generate positions
(define (GeneratePosAuxTeam2 defenders2 midFielders2 forwards2)
  (cond
    ((and (zero? defenders2) (zero? midFielders2) (zero? forwards2))
     '())
    ((not (zero? defenders2))
     (append (GeneratePosAuxDef2Mid1 1025 defenders2 0) (GeneratePosAuxTeam2 0 midFielders2 forwards2)))
    ((not (zero? midFielders2))
     (append (GeneratePosAuxDef1Mid2 775 midFielders2 0) (GeneratePosAuxTeam2 defenders2 0 forwards2)))
    ((not (zero? forwards2))
     (append (GeneratePosAuxForwards 675 forwards2 0) (GeneratePosAuxTeam2 defenders2 midFielders2 0)))
  ))

; Auxiliar of generate positions
(define (GeneratePosAuxDef1Mid2 initialPosition player counter)
  (cond
    ((equal? player counter)
     '())
    ((or (zero? counter) (equal? player (+ counter 1)))
     (cons (list initialPosition (+ 150 ( * ( / 500 player) counter))) (GeneratePosAuxDef1Mid2 initialPosition player (+ counter 1))))
    (else
     (cons (list (+ initialPosition 50) ( + 150 ( * (/ 500 player) counter))) (GeneratePosAuxDef1Mid2 initialPosition player (+ counter 1))))))

; Auxiliar of generate positions
(define (GeneratePosAuxDef2Mid1 initialPosition player counter)
  (cond
    ((equal? player counter)
     '())
    ((or (zero? counter) (equal?  player (+ counter 1)))
     (cons (list initialPosition (+ 150 (* (/ 500 player) counter))) (GeneratePosAuxDef2Mid1 initialPosition player (+ counter 1))))
    (else
     (cons (list (- initialPosition 50) (+ 150 (* (/ 500 player) counter))) (GeneratePosAuxDef2Mid1 initialPosition player (+ counter 1))))))

; Auxiliar of generate positions
(define (GeneratePosAuxForwards initialPosition player counter)
  (cond
    ((> counter player)
     '())
    (else
     (cons (list initialPosition (+ 275 (* (/ 200 player) counter))) (GeneratePosAuxDef2Mid1 initialPosition player (+ counter 1))))))

; Auxiliar of calling factor 
(define (CallMovingFactorAux x1 y1 x2 y2 v h ID type team delta_x delta_y)
  (cond ((or (> (-(abs (- x2 x1))5) 5) (> (-(abs (- y2 y1))5) 5))
         (cond ((and (< x2 1200) (> x2 0) (< y2 700) (> y2 0))
                (cond ((or
                        (and (equal? ID 1) (and(or (< x2 200) (> x2 975))(and (< y2 500) (> y2 175))))
                        (and (equal? ID 2) (or (< x2 400) (> x2 800)))
                        (and (equal? ID 3) (and (> x2 200) (< x2 1000)))
                        (and (equal? ID 4) (and (> x2 400) (< x2 1100)) (equal? team 2))
                        (and (equal? ID 4) (and (> x2 100) (< x2 800))  (equal? team 1))
                        )
                       (DeltaXY x1 y1 x2 y2 v h ID type team delta_x delta_y)
                       )
                      (else 0)))
               (else 0))
         )
        (else 0)
        )
  )

; It defines the limits of each player, and it does not allow to move forward
(define (CallMovingFactor x1 y1 x2 y2 v h ID type team)
  (cond ((or(equal? type "x"))
         (CallMovingFactorAux x1 y1 (- x2 (* 3 (MovingFactor x1 y1 x2 y2 v h))) y2 v h ID type team (MovingFactor x1 y1 x2 y2 v h) 0))
        (else
         (CallMovingFactorAux x1 y1 x2 (- y2 (* 3 (MovingFactor y1 x1 y2 x2 v h))) v h ID type team 0 (MovingFactor y1 x1 y2 x2 v h))
         ))
  )

; Auxiliar of moving factot
(define (DeltaXY x1 y1 x2 y2 v h ID type team delta_x delta_y)
  (cond ((or(equal? type "x"))
         delta_x)
        (else
         delta_y
         ))
  )

; Auxiliar of moving factor 
(define (MovingFactor x1 y1 x2 y2 v h)
  (* 0.25 (sqrt (+ (expt (+ v 5) 2) (expt (+ h 5) 2))) (/ (- x2 x1) (sqrt(+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))))
  )

; Starting movement
(define (ModifyPosition static_list changing_list list_compare counter velocity ability ID team)
  (cond ((not(null? static_list))
         (ModifyList static_list counter (list (- (caar list_compare) (CallMovingFactor
                                                                       (caar changing_list)
                                                                       (last(car changing_list))
                                                                       (caar list_compare)
                                                                       (last(car list_compare))
                                                                       (car velocity)
                                                                       (car ability)
                                                                       (car ID)
                                                                       "x"
                                                                       team)
                                                  )
                                               (- (last(car list_compare)) (CallMovingFactor
                                                                            (caar changing_list)
                                                                            (last(car changing_list))
                                                                            (caar list_compare)
                                                                            (last(car list_compare))
                                                                            (car velocity)
                                                                            (car ability)
                                                                            (car ID)
                                                                            "y"
                                                                            team)
                                                  )
                                               )))
        
        (else
         '()
         )))

; Update positions of each list
(define (UpdatePositions
         static_list1
         list_compare1
         static_list2
         list_compare2
         velocity1
         ability1
         velocity2
         ability2
         ID1
         ID2
         ball_position
         marker
         hasit
         )
  
  (UpdatePositionsAux
   static_list1
   static_list1
   list_compare1
   static_list2
   static_list2
   list_compare2
   velocity1
   ability1
   velocity2
   ability2
   ID1
   ID2
   1
   1
   (ball_pos static_list1 static_list2 ball_position 10 10 marker hasit)
   marker
   (cadr(ball_pos static_list1 static_list2 ball_position 10 10 marker hasit))
   )
  )

; Auxiliar of define positions
(define (UpdatePositionsAux
         static_list1
         changing_list1
         list_compare1
         static_list2
         changing_list2
         list_compare2
         velocity1
         ability1
         velocity2
         ability2
         ID1
         ID2
         counter1
         counter2
         ball_position
         marker
         hasit)
  (cond ((not (null? changing_list1))
         (UpdatePositionsAux
          (ModifyPosition static_list1 changing_list1 list_compare1 counter1 velocity1 ability1 ID1 1)
          (cdr changing_list1)
          (cdr list_compare1)
          (ModifyPosition static_list2 changing_list2 list_compare2 counter2 velocity2 ability2 ID2 2)
          (cdr changing_list2)
          (cdr list_compare2)
          (cdr velocity1)
          (cdr ability1)
          (cdr velocity2)
          (cdr ability2)
          (cdr ID1)
          (cdr ID2)
          (+ counter1 1)
          (+ counter2 1)
          (car(ball_pos static_list1 static_list2 ball_position 10 10 marker hasit))
          marker
          (cadr(ball_pos static_list1 static_list2 ball_position 10 10 marker hasit)))
         )
        (else
         (list(list static_list2 static_list1) (list ball_position hasit))))
  )


; Starting position, call generate positions to allign players
(define (starting_pos defenders1 midFielders1 forwards1 defenders2 midFielders2 forwards2)
  (GeneratePositions defenders1 midFielders1 forwards1 defenders2 midFielders2 forwards2)
  )

; returns which team has the ball
(define (WhoHasTheBall? team1 team2 last_pos)
  (cond ((not(Collisions team1 last_pos))
         "b"
         )
        ((not(Collisions team2 last_pos))
         "r"
         )
        ))

; ball positions determines if there is colision and change direction of the ball
(define (ball_pos team1 team2 last_pos f h marker hasit)
(cond((and (< (car last_pos) 1200) (> (car last_pos) 0) (< (cadr last_pos) 700) (> (cadr last_pos) 0))
  (cond
    ((and (< (car last_pos) 1200) (> (car last_pos) 1150) (< (cadr last_pos) 415) (> (cadr last_pos) 260))
     (list '(590 340) "m")
     )
    ((and (< (car last_pos) 25) (> (car last_pos) 0) (< (cadr last_pos) 415) (> (cadr last_pos) 260))
     (list '(590 340) "n")
     )
    (else
     (cond ((and(Collisions team1 last_pos) (Collisions team2 last_pos))
            (cond ((equal? hasit "b")
                   (list(ContinuosMovement last_pos f h 1150 340) hasit)
                   )
                  (else
                   (list (ContinuosMovement last_pos f h 50 340) hasit)))    
            )
           (else
            (cond ((equal? hasit "b")
                   (list (ContinuosMovement last_pos f h (+ 100(random 1000)) (+ 100(random 500))) "r")
                   )
                  (else
                   (list (ContinuosMovement last_pos f h (+ 100(random 1000)) (+ 100(random 500))) "b")))
 
            ))
     )))(else
         (list '(590 340) hasit)
         )))
         

(define (ContinuosMovement last_pos f h x y)
  (list (+ (car last_pos) (* 0.2(MovingFactor x y (car last_pos) (cadr last_pos) f h)))
               (- (cadr last_pos) (* 0.2(MovingFactor y x (cadr last_pos) (car last_pos) f h))))
  )

(define (RandomMove last_pos random_x random_y f h)
  (list (- (car last_pos) (MovingFactor random_x random_y (car last_pos) (cadr last_pos) 1 1))
        (- (cadr last_pos) (MovingFactor random_y random_x (cadr last_pos) (car last_pos) 1 1))))

; This calls refresh fucntion and creates de animation on canvas 
(define (RepaintAll positions ball_position marker)
  (Refresh
   (car positions)
   (last (cdr positions))
   ball_position
   marker)
   )

; This function gives a new list of new positions to repaint canvas and create animation
(define (CallToUpdate positions ball_position population marker hasit)
  (UpdatePositions
           (FollowBall ball_position)
           (cadr positions)
           (FollowBall ball_position)
           (car positions)
           (GetAttributes population 2 "velocity")
           (GetAttributes population 2 "ability")
           (GetAttributes population 1 "velocity")
           (GetAttributes population 1 "ability")
           (GetPlayersIds population 2)
           (GetPlayersIds population 1)
           ball_position
           marker
           hasit
           )
  )

; Auxiliar of active moving
(define (ActiveMovingAux positions population marker)
  (RepaintAll  (car positions) (caadr positions) marker)
  (cond(
        (or(equal?(last (cadr positions)) "n")(equal?(last (cadr positions)) "m"))
        (cond((equal? (last (cadr positions)) "m")
              (list positions ( ModifyList (list marker) 1 (list (+ (car marker) 1) (cadr marker)) ))
              )
             (else
              (list positions ( ModifyList (list marker) 1 (list (car marker) (+ (cadr marker) 1)) ))
              ))

        ) (else
           (cond ((not(equal? (car positions) (car(CallToUpdate (car positions) (caadr positions) population marker (last (cadr positions))))))
                  (ActiveMovingAux (CallToUpdate (car positions) (caadr positions) population marker (cddr positions)) population marker)
        
                  )))
          )
  )

; This function is responsable of all movement of players, takes info form CallToUpdate
(define (ActiveMoving positions population marker)
  (ActiveMovingAux positions population marker)
  )

(ActiveMoving (list(starting_pos 4 4 2 4 4 2) '((590 340) "b")) '(
  (((0 1 1 1) (1 0 1 1) (1 1 0 1) 2)
   ((0 1 1 0) (0 1 0 1) (0 0 1 1) 2)
   ((0 0 1 1) (0 1 0 1) (1 0 1 0) 2)
   ((1 0 0 1) (1 0 1 0) (1 0 1 1) 2)
   ((0 1 1 1) (0 1 0 0) (1 1 0 1) 2)
   ((0 0 1 1) (1 0 1 0) (0 0 1 1) 3)
   ((0 0 1 1) (0 0 0 0) (1 1 0 1) 3)
   ((0 1 0 0) (0 1 0 1) (0 0 0 1) 3)
   ((0 1 1 1) (1 1 0 0) (0 1 1 0) 4)
   ((1 1 1 0) (0 0 1 1) (0 1 0 1) 4)
   ((1 0 1 0) (1 1 0 1) (0 1 1 0) 1))
  (((1 1 1 0) (1 0 1 0) (1 0 1 1) 2)
   ((0 1 0 1) (0 1 1 1) (1 1 1 1) 2)
   ((0 1 0 1) (0 0 1 0) (0 1 0 0) 2)
   ((0 1 0 0) (0 0 0 1) (0 0 0 0) 2)
   ((0 1 1 1) (0 1 1 0) (0 0 1 1) 3)
   ((0 1 1 0) (1 0 0 0) (0 0 0 1) 3)
   ((0 1 0 1) (0 0 1 0) (0 1 0 0) 3)
   ((0 0 1 1) (0 0 0 0) (1 1 0 0) 3)
   ((0 0 0 1) (0 0 1 0) (0 0 0 1) 4)
   ((0 0 0 0) (0 0 1 1) (0 0 0 0) 4)
   ((0 0 0 1) (0 0 0 1) (0 0 1 1) 1))) '(0 0))
  

;;Meter en generation2

#|(CallToUpdate(CallToUpdate (UpdatePositions
           (car (starting_pos))
           (cadr(starting_pos))
           (cadr(starting_pos))
           (car (starting_pos))
           '(15 14 13 11 8 7 2 7 8 10 15)
           '(12 13 11 7 5 4 2 1 1 15 15)
           '(15 14 15 11 10 7 2 7 8 10 15)
           '(11 14 15 11 10 8 2 3 5 10 15)
           )))|#


#|
(Refresh
            (ModifyPosition static_list1 list_compare1 counter1 velocity1 ability1)
            (ModifyPosition static_list2 list_compare2 counter2 velocity2 ability2)
            (list 590 340)
            (list 0 0))
           (sleep/yield 1)
(sleep/yield 0.1)
(Refresh (list (list 75 340) (list 150 600) (list 200 450) (list 200 250) (list 150 100) (list 300 600) (list 350 450) (list 350 250) (list 300 100) (list 500 500) (list 500 200)) (list (list 1100 340) (list 900 340)) (list 590 340) (list 1 0))
(sleep/yield 0.1)
(Refresh (list (list 80 340) (list 625 115) (list 755 322) (list 1145 545) (list 655 125) (list 500 340)) (list (list 1095 340) (list 900 340)) (list 590 340) (list 1 1))
(sleep/yield 0.1)
(Refresh (list (list 85 340) (list 625 115) (list 755 322) (list 1145 545) (list 655 125)(list 505 340)) (list (list 1090 340) (list 900 340)) (list 590 340) (list 2 1))
|#
;;(define (nave posx posy lad)
;;(cond 
;;((equal? lad 'u)
;;    ((draw-solid-rectangle ventana) (make-posn posx posy) 10 10 "black"))
;;((equal? lad 'd)
;;        ((draw-solid-rectangle ventana) (make-posn posx posy) 10 10 "black"))
;;((equal? lad 'l)
;;        ((draw-solid-rectangle ventana) (make-posn posx posy) 10 10 "black"))
;;((equal? lad 'r)
;;        ((draw-solid-rectangle ventana) (make-posn posx posy) 10 10 "black"))
;;(else
;;(void))
;;)
;;)

;; 442