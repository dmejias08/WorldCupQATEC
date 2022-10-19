#lang racket
(require (lib "graphics.ss" "graphics"))
(require plot racket/class racket/gui/base)
(require 2htdp/universe)
(open-graphics)

(define mainFrame (open-viewport "QATEC" 1200 700))
(define mainWindow (open-pixmap "QATEC" 1200 700))

((draw-pixmap mainWindow) "assets/futcourt.png" (make-posn 0 0))

; FUNCION PARA DIBUJAR JUGADORES
(define (Jugadores lst color_ numb)
  (cond ((>= (length lst) 1)
             ((draw-solid-ellipse mainWindow) (make-posn (caar lst) (last(car lst))) 20 20 color_)
             (Jugadores (cdr lst) color_ (+ numb 1))
             ((draw-string mainWindow) (make-posn (+ (caar lst) 3) (+ (last(car lst)) 15)) (number->string numb)))))

(define (Ball position)
  ((draw-solid-ellipse mainWindow) (make-posn (car lst) (last position)) 20 20 "white"))

(define (Refresh team1 team2 ball score)
  ((draw-pixmap mainWindow) "assets/futcourt.png" (make-posn 0 0)) 
  (Jugadores team1 "blue" 1)
  (Jugadores team2 "red" 12)
  (ball ball)
  
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
         (onceParesAux pair (cons pair list)(+ counter 1)))))

(define (MovingFactor x1 y1 x2 y2 v h ID type)
  (cond ((or (> (abs (- x2 x1)) 5) (> (abs (- y2 y1)) 5))
         (cond ((and (< x1 1180) (> x1 20) (< y1 680) (> y1 20))
                (cond ((or
                        (and (equal? ID 1) (and(or (< x2 200) (> x2 975))(and (< y2 500) (> y2 175))))
                        (and (equal? ID 2) (or (< x2 400) (> x2 800)))
                        (and (equal? ID 3) (or (> x2 500) (< x2 700)))
                        (and (equal? ID 4) (or (and(< x2 595)(> x2 300)) (and(> x2 605)(< x2 900))))
                        )
                       (cond ((or(equal? type "x"))
                              (* 0.25 (sqrt (+ (expt (+ v 5) 2) (expt (+ h 5) 2))) (/ (- x2 x1) (sqrt(+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))))
                              )
                             (else (* 0.25 (sqrt (+ (expt (+ v 5) 2) (expt (+ h 5) 2))) (/ (- y2 y1) (sqrt(+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))))))
                       )
                      (else 0)))
               (else 0))
         )
        (else 0)
        )
  )

(define (ModifyPosition static_list changing_list list_compare counter velocity ability ID)
  (cond ((not(null? static_list))
        (ModifyList static_list counter (list (- (caar list_compare) (MovingFactor
                                                                     (caar changing_list)
                                                                     (last(car changing_list))
                                                                     (caar list_compare)
                                                                     (last(car list_compare))
                                                                     (car velocity)
                                                                     (car ability)
                                                                     (car ID)
                                                                     "x")
                                                 )
                                              (- (last(car list_compare)) (MovingFactor
                                                                           (caar changing_list)
                                                                           (last(car changing_list))
                                                                           (caar list_compare)
                                                                           (last(car list_compare))
                                                                           (car velocity)
                                                                           (car ability)
                                                                           (car ID)
                                                                           "y")
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
          (ModifyPosition static_list1 changing_list1 list_compare1 counter1 velocity1 ability1 ID1)
          (cdr changing_list1)
          (cdr list_compare1)
          (ModifyPosition static_list2 changing_list2 list_compare2 counter2 velocity2 ability2 ID2)
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
   (oncePares (ball_pos))
   (oncePares (ball_pos))
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