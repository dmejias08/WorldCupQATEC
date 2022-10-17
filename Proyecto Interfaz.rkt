#lang racket
(require (lib "graphics.ss" "graphics"))
(require plot racket/class racket/gui/base)
(require 2htdp/universe)
(open-graphics)

(define mainFrame (open-viewport "QATEC" 1200 700))
(define mainWindow (open-pixmap "QATEC" 1200 700))

((draw-pixmap mainWindow) "assets/futcourt.png" (make-posn 0 0))

; FUNCION PARA DIBUJAR JUGADORES
(define (jugadores lst color_ numb)
  (cond ((>= (length lst) 1)
             ((draw-solid-ellipse mainWindow) (make-posn (caar lst) (last(car lst))) 20 20 color_)
             (jugadores (cdr lst) color_ (+ numb 1))
             ((draw-string mainWindow) (make-posn (+ (caar lst) 3) (+ (last(car lst)) 15)) (number->string numb)))))

(define (ball lst)
  ((draw-solid-ellipse mainWindow) (make-posn (car lst) (last lst)) 20 20 "white"))

(define (refresh lst1 lst2 ball_ score)
  ((draw-pixmap mainWindow) "assets/futcourt.png" (make-posn 0 0))
  (jugadores lst1 "blue" 1)
  (jugadores lst2 "red" 12)
  (ball ball_)
  
  (copy-viewport mainWindow mainFrame)
  ((clear-viewport mainWindow)))
  
(refresh (list (list 70 340) (list 150 600) (list 200 450) (list 200 250) (list 150 100) (list 400 600) (list 350 450) (list 350 250) (list 400 100) (list 500 450) (list 500 250)) (list (list 1105 340) (list 1025 600) (list 975 450) (list 975 250) (list 1025 100) (list 775 600) (list 825 450) (list 825 250) (list 775 100) (list 675 450) (list 675 250)) (list 590 340) (list 0 0))
#|
(sleep/yield 0.1)
(refresh (list (list 75 340) (list 150 600) (list 200 450) (list 200 250) (list 150 100) (list 300 600) (list 350 450) (list 350 250) (list 300 100) (list 500 500) (list 500 200)) (list (list 1100 340) (list 900 340)) (list 590 340) (list 1 0))
(sleep/yield 0.1)
(refresh (list (list 80 340) (list 625 115) (list 755 322) (list 1145 545) (list 655 125) (list 500 340)) (list (list 1095 340) (list 900 340)) (list 590 340) (list 1 1))
(sleep/yield 0.1)
(refresh (list (list 85 340) (list 625 115) (list 755 322) (list 1145 545) (list 655 125)(list 505 340)) (list (list 1090 340) (list 900 340)) (list 590 340) (list 2 1))
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