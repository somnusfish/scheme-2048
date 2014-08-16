#lang scheme
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
  (define (mv0 lst)
    (if (null? lst)
        null
        (if (zero? (car lst))
            (append (mv0 (cdr lst)) (list 0))
            (cons (car lst) (mv0 (cdr lst))))))
  (define (zero-list? lst)
    (andmap zero? lst))
  (define (shift lst)
    (if (zero-list? lst)
        lst
        (if (zero? (car lst))
            (append (shift (cdr lst))
                    (list 0))
            lst)))
  (shift (left (mv0 lst))))

(define (anticlc-rotate cb)
  (if (= 1 (length (car cb)))
      (cons (map car cb) null)
      (append (anticlc-rotate (map cdr cb)) 
              (cons (map car cb) null))))

(define (mv-left cb)
  (map move-left-lst cb))
(define (mv-right cb)
  (anticlc-rotate
   (anticlc-rotate
    (mv-left 
     (anticlc-rotate
      (anticlc-rotate
       cb))))))
(define (mv-up cb)
  (anticlc-rotate
   (anticlc-rotate
    (anticlc-rotate
     (mv-left
      (anticlc-rotate
       cb))))))
(define (mv-down cb)
  (anticlc-rotate
   (mv-left
    (anticlc-rotate
     (anticlc-rotate
      (anticlc-rotate
       cb))))))

(define (win? cb)
  (define (win-lst? lst)
    (and (not (null? lst))
         (or (= 2048 (car lst))
             (win-lst? (cdr lst)))))
  (and (not (null? cb))
       (or (win-lst? (car cb))
           (win? (cdr cb)))))

