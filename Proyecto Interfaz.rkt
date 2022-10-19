#lang racket
(require (lib "graphics.ss" "graphics"))
(require plot racket/class racket/gui/base)
(require 2htdp/universe)
(open-graphics)

(define mainFrame (open-viewport "QATEC" 1200 700))
(define mainWindow (open-pixmap "QATEC" 1200 700))

((draw-pixmap mainWindow) "assets/futcourt.png" (make-posn 0 0))

; FUNCION PARA DIBUJAR JUGADORES
(define (Jugadores list color num)
  (cond ((>= (length list) 1)
             ((draw-solid-ellipse mainWindow) (make-posn (caar list) (last(car list))) 20 20 color)
             (Jugadores (cdr list) color (+ num 1))
             ((draw-string mainWindow) (make-posn (+ (caar list) 3) (+ (last(car list)) 15)) (number->string num)))))

(define (Ball position)
  ((draw-solid-ellipse mainWindow) (make-posn (car position) (last position)) 20 20 "white"))

(define (Refresh team1 team2 ball score)
  ((draw-pixmap mainWindow) "assets/futcourt.png" (make-posn 0 0))
  (Jugadores team1 "blue" 1)
  (Jugadores team2 "red" 12)
  (Ball ball)
  
  (copy-viewport mainWindow mainFrame)
  ((clear-viewport mainWindow)))

(define (ModifyList list index pair)
  (ModifyListAux list 1 index pair))

(define (ModifyListAux list currentIndex finalIndex pair)
  (cond ((equal? currentIndex finalIndex)
        (cons pair (cdr list)))
        (else
         (cons (car list) (ModifyListAux (cdr list) (+ currentIndex 1) finalIndex pair ) ))))

(define (Moving? team1 team2)
        (cond
          ((and (null? team1) (null? team2))
           #f)
          ((and (< (abs (- (caar team1) (caar team2))) 5)  (< (abs (- (cadar team1) (cadar team2) 5)))
           (Moving? (cdr team1) (cdr team2))))
          (else
           #t)))

(define (Collisions positions pair)
  (cond ((null? positions)
         #t)
        ((and (< (abs (- (caar positions)(car pair))) 5 ) (< (abs (- (cadar positions)(cadr pair))) 5 ))
         #f)
        (else
         (Collisions (cdr positions) pair))))

(define (FollowBall pair)
  (FollowBallAux pair '() 1))

(define (FollowBallAux pair list counter)
  (cond ((equal? counter 12)
         list)
        (else
         (FollowBallAux pair (cons pair list)(+ counter 1)))))

(define (CallMovingFactor x1 y1 x2 y2 v h ID type team positions1 positions2)
  (cond ((or (> (-(abs (- x2 x1))7) 10) (> (-(abs (- y2 y1))7) 10))
         (cond ((and (< x2 1180) (> x2 20) (< y2 680) (> y2 20))
                (cond ((or
                        (and (equal? ID 1) (and(or (< x2 200) (> x2 975))(and (< y2 500) (> y2 175))))
                        (and (equal? ID 2) (or (< x2 400) (> x2 800)))
                        (and (equal? ID 3) (and (> x2 200) (< x2 1000)) (equal? team 1))
                        (and (equal? ID 3) (and (> x2 200) (< x2 1000)) (equal? team 2))
                        (and (equal? ID 4) (and (> x2 400) (< x2 1100)) (equal? team 2))
                        (and (equal? ID 4) (and (> x2 100) (< x2 800))  (equal? team 1))
                        )
                       (DeltaXY x1 y1 x2 y2 v h ID type team positions1 positions2)
                       )
                      (else 0)))
               (else 0))
         )
        (else 0)
        )
  )
(define (DeltaXY x1 y1 x2 y2 v h ID type team positions1 positions2)
  (cond ((or(equal? type "x"))
         (cond ((and (Collisions positions1 (list (+ x2 (MovingFactor x1 y1 x2 y2 v h)) y2)) (Collisions positions2 (list (+ x2 (MovingFactor x1 y1 x2 y2 v h)) y2)))
                (MovingFactor x1 y1 x2 y2 v h))
               (else
                (* -1 (MovingFactor x1 y1 x2 y2 v h))
                )
               ))
        (else
         (cond ((and (Collisions positions1 (list x2 (+ y2 (MovingFactor y1 x1 y2 x2 v h)))) (Collisions positions2 (list x2 (+ y2 (MovingFactor y1 x1 y2 x2 v h)))))
                (MovingFactor y1 x1 y2 x2 v h))
               (else
                (* -1 (MovingFactor y1 x1 y2 x2 v h))
                )
               )
         ))
  )

(define (MovingFactor x1 y1 x2 y2 v h)
  (* 0.25 (sqrt (+ (expt (+ v 5) 2) (expt (+ h 5) 2))) (/ (- x2 x1) (sqrt(+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))))
  )

(define (ModifyPosition static_list changing_list list_compare counter velocity ability ID team other_team)
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
                                                                       team
                                                                       static_list
                                                                       other_team)
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
                                                                            team
                                                                            static_list
                                                                            other_team)
                                                  )
                                               )))
        
        (else
         '()
         )))
  
(define (UpdatePositions static_list1 list_compare1 static_list2 list_compare2 velocity1 ability1 velocity2 ability2 ID1 ID2)
  
  (UpdatePositionsAux static_list1 static_list1 list_compare1 static_list2 static_list2 list_compare2 velocity1 ability1 velocity2 ability2 ID1 ID2 1 1)
  )

(define (UpdatePositionsAux static_list1 changing_list1 list_compare1 static_list2 changing_list2 list_compare2 velocity1 ability1 velocity2 ability2 ID1 ID2 counter1 counter2)
  ;;añadir una condición para defensas portero etc
  
   
  (cond ((not (null? changing_list1))
         (UpdatePositionsAux
          (ModifyPosition static_list1 changing_list1 list_compare1 counter1 velocity1 ability1 ID1 1 static_list2)
          (cdr changing_list1)
          (cdr list_compare1)
          (ModifyPosition static_list2 changing_list2 list_compare2 counter2 velocity2 ability2 ID2 2 static_list1)
          (cdr changing_list2)
          (cdr list_compare2)
          (cdr velocity1)
          (cdr ability1)
          (cdr velocity2)
          (cdr ability2)
          (cdr ID1)
          (cdr ID2)
          (+ counter1 1)
          (+ counter2 1))
         )
        (else
         (list static_list1 static_list2)))
  )

(define (starting_pos)
  (list (list
         (list 70 340)
         (list 150 600)
         (list 200 450)
         (list 200 250)
         (list 150 100)
         (list 400 600)
         (list 350 450)
         (list 350 250)
         (list 400 100)
         (list 500 450)
         (list 500 250))

        (list
         (list 1105 340)
         (list 1025 600)
         (list 975 450)
         (list 975 250)
         (list 1025 100)
         (list 775 600)
         (list 825 450)
         (list 825 250)
         (list 775 100)
         (list 675 450)
         (list 675 250))
        )
  )

(define (togo_pos)
  (list
   (FollowBall (ball_pos))
   (FollowBall (ball_pos))
   )
  )

(define (ball_pos)
  (list 1100 100)
  )

(define (RepaintAll positions)
  (Refresh
   (car positions)
   (last (cdr positions))
   (ball_pos)
   (list 0 0))
   )

(Refresh
 (car (starting_pos))
 (last (cdr (starting_pos)))
 (ball_pos)
 (list 0 0))

(define (CallToUpdate positions)
  (reverse (UpdatePositions
           (car (togo_pos))
           (cadr positions)
           (cadr (togo_pos))
           (car positions)
           '(15 14 13 11 8 7 2 7 8 10 15)
           '(12 13 11 7 5 4 2 1 1 15 15)
           '(15 14 15 11 10 7 2 7 8 10 15)
           '(11 14 15 11 10 8 2 3 5 10 15)
           '(1 2 2 2 2 3 3 3 3 4 4)
           '(1 2 2 2 2 3 3 3 3 4 4)
           ))
  )

(define (ActiveMoving positions)
  (RepaintAll  positions)
  (cond((and (Moving? (car (starting_pos)) (cadr positions)) (Moving? (cadr (starting_pos)) (car positions)))
        (ActiveMoving (CallToUpdate positions))
        )
       (else
        positions
        ))
  )

#|(displayln (reverse (UpdatePositions
           (car (starting_pos))
           (cadr (starting_pos))
           (cadr (starting_pos))
           (car (starting_pos))
           '(15 14 13 11 8 7 2 7 8 10 15)
           '(12 13 11 7 5 4 2 1 1 15 15)
           '(15 14 15 11 10 7 2 7 8 10 15)
           '(11 14 15 11 10 8 2 3 5 10 15)
           )))
(displayln "")
(displayln "Starting Point")
(displayln (starting_pos))|#

(ActiveMoving (starting_pos))

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