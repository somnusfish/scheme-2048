#lang scheme/gui
(require scheme/gui/base)
(require "chessboard.scm")

(define (change-to-str cb)
  (apply 
   string-append 
   (map 
    (lambda (l) 
      (string-append 
       (apply 
        string-append 
        (map 
         (lambda (x) 
           (let ((y (format "~A" x)))
             (format "~A " (string-append (make-string (- 4 (string-length y)) #\ ) y)))) 
         l))))
    cb)))
(define frame (new frame%
                   [label "2048"]
                   [width 400]
                   [height 400]))


(define msg (new message%
                 [parent frame]
                 [label "S to start\nR to restart\nup down left right to move"]))

(define cb (make-chess-board))
(define text (change-to-str cb))

(define my-canvas%
  (class canvas% 
    (define/override (on-char event)
      (let ((keycode (send event get-key-release-code)))
        (cond ((eq? keycode 'up) (set! cb  (gen-cb (mv-up cb))))
              ((eq? keycode 'down) (set! cb (gen-cb (mv-down cb))))
              ((eq? keycode 'left) (set! cb (gen-cb (mv-left cb))))
              ((eq? keycode 'right) (set! cb (gen-cb (mv-right cb))))
              ((eq? keycode #\s ) (set! cb (gen-cb cb)))
              ((eq? keycode #\r) (set! cb (make-chess-board))))
        ;(set! cb  (mv-up (gen-cb cb)))
        (set! text (change-to-str cb))
        (send this refresh-now)
        (send this on-paint))
      )
    (super-new)))

(new my-canvas%
     [parent frame]
     [paint-callback
      (lambda (canvas dc)
        (send dc set-scale 3 3)
        (send dc set-text-foreground "blue")
        (send dc draw-text (substring text 0 19) 0 0 )
        (send dc draw-text (substring text 20 39)0 25 )
        (send dc draw-text (substring text 40 59) 0 50 )
        (send dc draw-text (substring text 60 79) 0 75)
        (send dc draw-text (substring text 80) 40 40))])

(send frame show #t)

(change-to-str (list (list 1 22 333 4444)
                 (list 4444 333 22 1)
                 (list 333 22 1 4444)
                 (list 22 4444 1 333)))
