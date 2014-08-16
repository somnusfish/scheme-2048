#lang scheme
(require scheme/match)
(define test (list '(1 2 3 4 ) '(5 6 7 8 ) '(9 10 11 12 ) '(13 14 15 16 )))
(define (make-chess-board)
  (define (make-element element num)
    (if (= num 0) 
        null
        (cons element (make-element element (- num 1)))))
  (make-element
   (make-element 0 4)
   4)
  )

(define (move-left-lst lst)
  (define (left lst)
    (cond ((> (length lst) 2)
           (if (= (car lst) (cadr lst)) 
               (append (list (* 2 (car lst))) 
                       (append (left (cddr lst)) (list 0)))
               (cons (car lst) (left (cdr lst)))))
          ((= (length lst) 2)
           (if (= (car lst) (cadr lst))
               (list (* 2 (car lst)) 0)
               lst))
          (else lst)))
  (define (zero-list? lst)
    (andmap zero? lst))
  (define (shift lst)
    (if (zero-list? lst)
        lst
        (if (zero? (car lst))
            (append (shift (cdr lst))
                    (list 0))
            lst)))
  (shift (left lst)))

(define (anticlc-rotate cb)
  (if (= 1 (length (car cb)))
      (cons (map car cb) null)
      (append (anticlc-rotate (map cdr cb)) 
            (cons (map car cb) null))))



(anticlc-rotate test)
