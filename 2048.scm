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
                   [width 300]
                   [height 300]))

; Make a static text message in the frame
(define msg (new message%
                 [parent frame]
                 [label "S to start\nR to restart\nup down left right to move"]))
(define text (change-to-str (make-chess-board) ))
(define my-canvas%
  (class canvas% ; The base class is canvas%
    ; Define overriding method to handle mouse events
    ;(define/override (on-event event)
    ;(send msg set-label (format "Canvas mouse@(~A,~% ~A)" (send event get-x) (send event get-y))))
    ; Define overriding method to handle keyboard events
    (define/override (on-char event)
      ;(send msg set-label (format "Canvas key, code = ~A." (send event get-key-code)))
      (let ((keycode (send event get-key-release-code))
            (cb (make-chess-board)))
        (cond ((eq? keycode 'up) (set! text "up"))
              ((eq? keycode 'down) (set! text "down"))
              ((eq? keycode 'left) (set! text "left"))
              ((eq? keycode 'right) (set! text "right"))
              ((eq? keycode #\s ) (set! text "start"))
              ((eq? keycode #\r) (set! text "restart")))
        ;(set! text (format "~A" keycode))
        (send this refresh-now)
        (send this on-paint)))
    ; Call the superclass init, passing on all init args
    (super-new)))

(new my-canvas%
     [parent frame]
     [paint-callback
      (lambda (canvas dc)
        (send dc set-scale 3 3)
        (send dc set-text-foreground "blue")
        (send dc draw-text (substring text 0 19) 0 0 )
        (send dc draw-text (substring text 20 39)0 20 )
        (send dc draw-text (substring text 40 59) 0 40 )
        (send dc draw-text (substring text 60 79) 0 60))])

(send frame show #t)
(change-to-str (make-chess-board) )


