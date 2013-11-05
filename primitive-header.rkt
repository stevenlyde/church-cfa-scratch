
(define (hist samples name)
  (void))

(define (density samples)
  (void))

(define (runPhysics n w)
  (void))

(define first car)
(define second cdr)
(define (third lst)
  (cdr (cdr (car lst))))

(define (repeat n thunk)
  (if (> n 0)
      (pair (thunk) (repeat (- n 1) thunk))
      (list)))

(define (map proc lst)
  (if (null? lst) '() (cons (proc (car lst)) (map proc (cdr lst)))))

(define (sum lst)
  (if (null? lst) 0 (+ (car lst) (sum (cdr lst)))))