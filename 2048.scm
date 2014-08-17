#lang scheme
(define (make-chess-board)
  (make-list 4 (make-list 4 0)))

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
    (cond ((null? lst) null)
          ((zero? (car lst))
           (append (mv0 (cdr lst)) (list 0)))
          (else (cons (car lst) (mv0 (cdr lst))))))
  (define (zero-list? lst)
    (andmap zero? lst))
  (define (shift lst)
    (cond ((zero-list? lst) lst)
          ((zero? (car lst))
           (append (shift (cdr lst))
                   (list 0)))
          (else lst)))
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
  (ormap 
   (lambda (line) (ormap (lambda (x) (= x 2048)) line)) 
   cb))

(define (fail? cb)
  (define (zero-in? cb)
    (ormap
     (lambda (line) (ormap (lambda (x) (= x 0)) line))
     cb))
  (not (ormap zero-in? (list (mv-left cb)
                             (mv-right cb)
                             (mv-up cb)
                             (mv-down cb)))))

(define (gen-num)
  (if (zero? (remainder (random 10) 10))
      4
      2))
(define (gen-cb cb)
  (define (count-zero cb)
    (apply +
           (map
            (lambda (line) (length (filter zero? line)))
            cb)))
  (define (change-lst lst posi)
    (cond ((not (zero? (car lst)))
           (cons (car lst) (change-lst (cdr lst) posi)))
          ((not (= posi 1))
           (cons (car lst) (change-lst (cdr lst) (- posi 1))))
          (else
           (cons (gen-num) (cdr lst)))))
  (define (change cb posi)
    (let ((x (length (filter zero? (car cb)))))
      (if (< x posi)
          (cons (car cb) 
                  (change (cdr cb) (- posi x)))
          (cons (change-lst (car cb) posi)
                (cdr cb)))))
  (change cb (+ 1(random (count-zero cb)))))

