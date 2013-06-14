;#lang racket/gui
;(require 2htdp/universe)
;(require 2htdp/image)
(define (double-program)
  
  ; INITIALIZATION OF THE VARIABLES USED
  (define hit 0)
  (define perfect-hit-player 0)
  (define perfect-hit-comp 0)
  (define health-1 12)
  (define health-2 12) 
  (define prev-position 0)
  (define a 0)
  (define image (bitmap "ws.jpg"))
  (define time-player 0)
  (define time-comp 0)
  (define b 1)
  (define wind 0)
  
  ;IMAGES USED IN THE PROJECT CREATED BY USING FUNCTIONS OF 2htdp/image MODULE
  
  ;HANDS OF THE MAN
  (define (hands dist)
    (add-line (add-line (add-line (add-curve (add-line (line 35 20 (make-pen "black" 3 "solid" "round" "round")) 0 0 35 -20 (make-pen "black" 3 "solid" "round" "round"))
                                             25 -10 10 1/3
                                             25 50 170 1/3
                                             (make-pen "black" 3 "solid" "round" "round"))
                                  (- 25 (/ dist 20)) 30 25 58 (make-pen "black" 3 "solid" "round" "round"))
                        (- 25 (/ dist 20)) 30 25 -2 (make-pen "black" 3 "solid" "round" "round"))
              (- 25 (/ dist 20)) 30 (- 65 (/ dist 20)) 30 (make-pen "black" 3 "solid" "projecting" "round")))
  
  ;AN ABSTRACTION TO CREATE A MIRROR IMAGE OF HANDS
  (define (invert-hands dist)
    (rotate 180 (hands dist)))
  
  ;IMAGE OF THE MAN
  (define man
    (add-line 
     (add-line
      (add-line
       (circle 15 "outline" (make-pen "black" 3 "solid" "round" "round"))
       15 30 15 90 (make-pen "black" 3 "solid" "round" "round"))
      15 90 35 110 (make-pen "black" 3 "solid" "round" "round"))
     15 90 -5 110 (make-pen "black" 3 "solid" "round" "round")))
  
  ;THE MAIN "BIG-BANG" FUNCTION WHICH IS THE FUNDAMENTAL PART OF 2htdp/universe LIBRARY
  (define (rotate-man ang)
    (big-bang ang
              (on-mouse add)
              (on-tick throw)
              (to-draw change-angle-scene 1300 700)))
  
  ;THE MOUSE-EVENT FUNCTION USED TO CONTROL ALL THE MOUSE FUNCTIONS IN THE GAME
  (define (add ang x y mouse-event)
    (cond [(and (or (equal? mouse-event "drag") (equal? mouse-event "button-down"))  (= hit 0)) (if (= a 0) 
                                                                                                    (let([dist (sqrt (+ (* (- x 65) (- x 65)) (* (- y 560) (- y 560))))])
                                                                                                      (if(= (- x 65) 0) (if (< dist 550) (list (cons 0 dist) (- 65 (/  dist 20)) 560 0)
                                                                                                                            (list (cons 0 550) (- 65 (/ 550 20)) 560 0))
                                                                                                         (if(< (/ (* 180 (atan (/ (- y 560) (- x 65)))) pi) -20)
                                                                                                            (if(< dist 550) (list (cons (/ (* 180 (atan (/ (- y 560) (- x 65)))) pi) dist)
                                                                                                                                  (- 65 (/  dist 20)) 560
                                                                                                                                  (atan (/ (- y 560) (- x 65))))
                                                                                                               (list (cons (/ (* 180 (atan (/ (- y 560) (- x 65)))) pi) 550) (- 65 (/ 550 20)) 560 (atan (/ (- y 560) (- x 65)))))
                                                                                                            (if(< dist 550) (list (cons -20 dist) (- 65 (/ dist 20)) 560 (/ (* -20 pi) 180))
                                                                                                               (list (cons -20 550) (- 65 (/ 550 20)) 560 (/ (* -20 pi) 180)))))) 
                                                                                                    
                                                                                                    ang)]
          
          
          [(and (equal? mouse-event "button-up") (= a 0) (= hit 0)) (begin (set! a 1) (play-sound "abcde.wav" #t) ang)]
          [(and (or (equal? mouse-event "drag") (equal? mouse-event "button-down")) (= a 1) (= hit 1)) (if (= a 1)
                                                                                                           (let([dist (sqrt (+ (* (- x 1180) (- x 1180)) (* (- y 560) (- y 560))))])
                                                                                                             (if (= (- x 1180) 0) (if(< dist 550) (list (cons 0 dist) (+ 1180 (/ dist 20)) 560 0)
                                                                                                                                     (list (cons 0 550) (+ 1180 (/ 550 20)) 560 0))
                                                                                                                 (if(> (/ (* 180 (atan (/ (- y 560) (- x 1180)))) pi) 20)
                                                                                                                    (if(< dist 550) (list (cons (/ (* 180 (atan (/ (- y 560) (- x 1180)))) pi) dist)
                                                                                                                                          (+ 1180 (/  dist 20))
                                                                                                                                          560
                                                                                                                                          (atan (/ (- y 560) (- x 1180))))
                                                                                                                       (list (cons (/ (* 180 (atan (/ (- y 560) (- x 1180)))) pi) 550)
                                                                                                                             (+ 1180 (/ 550 20))
                                                                                                                             560
                                                                                                                             (atan (/ (- y 560) (- x 1180)))))
                                                                                                                    (if(< dist 550) (list (cons 20 dist) (+ 1180 (/ dist 20)) 560 (/ (* 20 pi) 180))
                                                                                                                       (list (cons 20 550) (+ 1180 (/ 550 20)) 560 (/ (* 20 pi) 180))))))
                                                                                                           ang)]
          [(and (equal? mouse-event "button-up") (= a 1) (= hit 1)) (begin (set! a 0) (play-sound "abcde.wav" #t) ang)]    
          [else ang]))
  
  ;IMAGE OF BLOOD OF THE PLAYER
  (define (blood-player m)
    (ellipse (* 30 m) (* 10 m) "solid" "red"))
  
  ;IMAGE OF THE BLOOD OF COMPUTER
  (define (blood-comp m)
    (ellipse (* 30 m) (* 10 m) "solid" "red"))
  
  ;BACKGROUND OF THE WORLD
  (define background image)
  
  ;SIMPLE ABSTRACTIONS TO MAKE CODE LOOK READABLE
  (define (angle-text angle)
    (text (string-append "angle: " (number->string angle)) 20 "black"))
  
  (define (tension-text tension)
    (text (string-append "tension: " (number->string tension)) 20 "black"))
  
  (define (wind-text-player wind)
    (if(> wind 0) (text (string-append (string-append "WIND: " (number->string wind)) "-->") 20 "black")
       (text (string-append (string-append "WIND: " (number->string (- wind))) "<--") 20 "black")))
  
  (define (wind-text-comp wind)
    (if(> wind 0) (text (string-append (string-append "WIND: " (number->string wind)) "<--") 20 "black")
       (text (string-append (string-append "WIND: " (number->string (- wind))) "-->") 20 "black")))
  
  ;THE CREATE-SCENE FUNCTION WHICH REDRAWS THE GIVEN SCENES AT EVERY TICK DURING THE EXECUTION OF THE PROGRAM
  (define (change-angle-scene ang)
    (cond[(= health-1 0) (underlay/xy (rectangle 1000 700 "solid" "white") 400 350 (text "PLAYER 1 WINS" 50 "black"))]
         [(= health-2 0) (underlay/xy (rectangle 1000 700 "solid" "white") 400 350 (text "PLAYER 2 WINS" 50 "black"))]
         [ (and (= a 0) (= hit 0)) (underlay/xy
                                    (underlay/xy
                                     (underlay/xy
                                      (underlay/xy
                                       (underlay/xy
                                        (add-line 
                                         (underlay/xy 
                                          (underlay/xy 
                                           (underlay/xy 
                                            (underlay/xy 
                                             background 50 500 (overlay/xy man 10 30 (rotate (- (caar ang)) (hands (cdar ang)))))
                                            1200 500 (overlay/xy man -35 30 (invert-hands 0)))
                                           300 100 (angle-text (caar ang)))
                                          800 100 (tension-text (cdar ang)))
                                         0 600 1300 600 (make-pen "black" 3 "solid" "round" "round"))
                                        200 620 (rectangle (* 20 health-2) 10 "solid" "red"))
                                       900 620 (rectangle (* 20 health-1) 10 "solid" "red"))
                                      600 0 (wind-text-player wind))
                                     100 600 (blood-player perfect-hit-comp))
                                    1200 600 (blood-comp perfect-hit-player))]
         
         [ (and (= a 1) (= hit 0)) (underlay/xy
                                    (underlay/xy
                                     (underlay/xy 
                                      (underlay/xy 
                                       (add-line 
                                        (add-line 
                                         (underlay/xy 
                                          (underlay/xy background 50 500 (overlay/xy man 10 30 (hands 0)))
                                          1200 500 (overlay/xy man -35 30 (invert-hands 0)))
                                         (cadr ang) (caddr ang) (+ (cadr ang) (* 40 (cos (cadddr ang)))) (- (caddr ang) (* 40 (sin (cadddr ang)))) (make-pen "black" 3 "solid" "round" "round"))
                                        0 600 1300 600 (make-pen "black" 3 "solid" "round" "round"))
                                       200 620 (rectangle (* 20 health-2) 10 "solid" "red"))
                                      900 620 (rectangle (* 20 health-1) 10 "solid" "red"))
                                     100 600 (blood-player perfect-hit-comp)) 
                                    1200 600 (blood-comp perfect-hit-player))]
         
         [ (and (= a 1) (= hit 1)) (underlay/xy 
                                    (underlay/xy 
                                     (underlay/xy 
                                      (underlay/xy 
                                       (underlay/xy 
                                        (add-line 
                                         (underlay/xy 
                                          (underlay/xy 
                                           (underlay/xy 
                                            (underlay/xy 
                                             background 50 500 (overlay/xy man 10 30 (hands 0)))
                                            1200 500 (overlay/xy man -20 30 (rotate (- (caar ang)) (invert-hands (cdar ang)))))
                                           300 100 (angle-text (caar ang)))
                                          800 100 (tension-text (cdar ang)))
                                         0 600 1300 600 (make-pen "black" 3 "solid" "round" "round"))
                                        200 620 (rectangle (* 20 health-2) 10 "solid" "red"))
                                       900 620 (rectangle (* 20 health-1) 10 "solid" "red"))
                                      600 0 (wind-text-comp wind))
                                     100 600 (blood-player perfect-hit-comp))
                                    1200 600 (blood-comp perfect-hit-player))]
         
         [ (and (= a 0) (= hit 1)) (underlay/xy 
                                    (underlay/xy 
                                     (underlay/xy 
                                      (underlay/xy 
                                       (add-line 
                                        (add-line 
                                         (underlay/xy 
                                          (underlay/xy 
                                           background 50 500 (overlay/xy man 10 30 (hands 0)))
                                          1200 500 (overlay/xy man -40 30 (invert-hands 0))) (cadr ang) (caddr ang) (- (cadr ang) (* 40 (cos (cadddr ang)))) (- (caddr ang) (* 40 (sin (cadddr ang)))) (make-pen "black" 3 "solid" "round" "round"))
                                        0 600 1300 600 (make-pen "black" 3 "solid" "round" "round"))
                                       200 620 (rectangle (* 20 health-2) 10 "solid" "red"))
                                      900 620 (rectangle (* 20 health-1) 10 "solid" "red"))
                                     100 600 (blood-player perfect-hit-comp))
                                    1200 600 (blood-comp perfect-hit-player))]))
  
  
  
  
  ;THE ON-TICK FUNCTION WHICH CONSISTS OF ALL THE FUNCTIONS THAT SHOULD BE EXECUTED AT EACH TICK
  (define (throw ang)
    (let([instant-y (- (caddr ang) (* 40 (sin (cadddr ang))))]
         [instant-x (+ (cadr ang) (* 40 (cos (cadddr ang))))]
         [instant-x1 (- (cadr ang) (* 40 (cos (cadddr ang))))])
      (cond[(and (>= instant-y 600) (< instant-x 1250) (= a 1)) (begin (set! hit 1) (set! time-player 0) ang)]
           [(and (>= instant-x 1220) (< instant-x 1300) (= a 1) (>= instant-y 500) (< instant-y 600) (= hit 0)) (begin (set! hit 1)
                                                                                                                       (set! perfect-hit-player (+ perfect-hit-player 1))
                                                                                                                       (set! health-1 (- health-1 4))
                                                                                                                       (set! time-player 0)
                                                                                                                       ang)]
           [(and (>= instant-x 1250) (< instant-x 1300) (= a 1) (>= instant-y 500) (< instant-y 600)) ang]
           [(and (> instant-x 1300) (= a 1)) (begin 
                                               (set! hit 1)
                                               (set! time-player 0)
                                               ang)]
           
           
           [(and (>= instant-y 600) (= a 0) (> instant-x1 100)) (begin (set! hit 0) (set! time-comp 0) (set! prev-position (cadr ang)) ang)]
           [(and (< instant-x1 100) (> instant-x1 50) (= a 0) (>= instant-y 500) (< instant-y 600) (= hit 1)) (begin (set! hit 0)
                                                                                                                     (set! perfect-hit-comp (+ perfect-hit-comp 1))
                                                                                                                     (set! prev-position (cadr ang))
                                                                                                                     (set! health-2 (- health-2 4))
                                                                                                                     (set! time-comp 0)
                                                                                                                     ang)]
           [(and (< instant-x1 100) (> instant-x1 50) (= a 0) (>= instant-y 500) (< instant-y 600)) ang]
           [(and (>= instant-y 600) (< instant-x1 50) (= a 0)) (begin (set! hit 0) (set! prev-position (cadr ang)) (set! time-comp 0) ang)]
           [(and (< instant-x1 0) (= a 0)) (begin (set! hit 0) (set! time-comp 0) ang)]
           
           [(= hit 0)
            (begin (if (and (= a 0) (= b 1)) (begin (set! wind (- (random 11) 6)) (set! b 0) wind)
                       wind)
                   (cond[(= a 1) (let*(
                                       [init-speed (/ (cdar ang) 15)]
                                       [init-angle (caar ang)]
                                       [init-speed-x (+ (* init-speed (cos (/ (* init-angle pi) 180))) wind)]
                                       
                                       [new-x (+ (cadr ang) init-speed-x)]        
                                       [new-y (- (caddr ang) (- init-speed (* 10 (begin (set! time-player (+ time-player 0.1)) time-player))))])
                                   (list (car ang) new-x new-y (atan (- 1 (* (/ 10 init-speed) time-player)))))]
                        
                        [else ang]))]
           [(= hit 1)
            (begin (if (and (= a 1) (= b 0)) (begin (set! wind (- (random 11) 6)) (set! b 1) wind)
                       wind)
                   (cond [(= a 0) (let*(
                                        [init-speed (/ (cdar ang) 15)]
                                        [init-angle (caar ang)]
                                        [init-speed-x (+ wind (* init-speed (cos (/ (* init-angle pi) 180))))]                             
                                        [new-x (- (cadr ang) init-speed-x)]
                                        [new-y (- (caddr ang) (- init-speed (* 10 (begin (set! time-comp (+ time-comp 0.1)) time-comp))))])
                                    (list (car ang) new-x new-y (atan (- 1 (* (/ 10 init-speed) time-comp)))))]
                         [else ang]))])))
  
  ;THE FINAL EXPRESSION EVALUATED
  (rotate-man (list (cons -20 10) 760 560 0)))









