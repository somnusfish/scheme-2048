#lang scheme
(define (make-chess-board)
  (define (make-element element num)
    (if (= num 0) 
        null
        (cons element (make-element element (- num 1)))))
  (make-element
   (make-element '0 4)
   4)
)
