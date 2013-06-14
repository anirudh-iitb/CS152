#lang racket/gui
(require 2htdp/universe)
(require 2htdp/image)
(include "doubleplayer.rkt")
(include "singleplayer.rkt")

(define main-menu (new frame% [label "main menu"]
                       [width 700]
                       [height 700]))

(define abitmap (make-object bitmap% "bowman_b.jpg" 'jpeg))
(define atext (make-object bitmap% "text.png" 'png))

(new canvas% [parent main-menu]
     [paint-callback (lambda(canvas dc)
                       (send dc set-scale 1 0.8)
                       (send dc draw-bitmap abitmap 190 0))])

(define play (new button% [parent main-menu]
                  [label "PLAY"]
                  (callback (lambda(button event)
                              (begin 
                                (send play-menu show #t)
                                (send main-menu show #f)
                                )))
                  [vert-margin 100]
                  [min-width 200]
                  ))

(define how-to (new button% [parent main-menu]
                    [label "HOW-TO"]
                    (callback (lambda(button event)
                                (begin 
                                  (send how-to-menu show #t)
                                  (send main-menu show #f))))
                    [min-width 200]
                    ))

(define exit (new button% [parent main-menu]
                  [label "EXIT"]
                  (callback (lambda(button event)       
                              (send main-menu show #f)))
                  [vert-margin 100]
                  [min-width 200]
                  ))


(define play-menu (new frame% [label "play menu"]
                       [width 700]
                       [height 700]))

(new canvas% [parent play-menu]
     [paint-callback (lambda(canvas dc)
                       (send dc set-scale 1 0.8)
                       (send dc draw-bitmap abitmap 190 0))])                      

(define how-to-menu (new frame% [label "how-to-menu"]
                         [width 800]
                         [height 800]))

(new canvas% [parent how-to-menu]
     [paint-callback (lambda(canvas dc)
                       (send dc set-scale 1 0.8)
                       (send dc draw-bitmap abitmap 190 0)
                       (send dc set-scale 1 1)
                       (send dc draw-bitmap atext 0 300))])

(define back2 (new button% [parent how-to-menu]
                   [label "back to main menu"]
                   (callback (lambda(button event) 
                               (begin
                                 (send main-menu show #t)
                                 (send how-to-menu show #f))))
                   [vert-margin 50]
                   [min-width 200]))

(define single-player (new button% [parent play-menu]
                           [label "single player"]
                           (callback (lambda(button event)
                                       (begin 
                                         (send play-menu show #f)
                                         (single-program)
                                         )))
                           [vert-margin 100]
                           [min-width 200]))

(define double-player (new button% [parent play-menu]
                           [label "double player"]
                           (callback (lambda(button event)
                                       (begin 
                                         (send play-menu show #f)
                                         (double-program)
                                         )))
                           [min-width 200]))

(define back (new button% [parent play-menu]
                  [label "back to main menu"]
                  (callback (lambda(button event)
                              (begin 
                                (send main-menu show #t)
                                (send play-menu show #f))))
                  [vert-margin 100]
                  [min-width 200]))

(send main-menu show #t)




