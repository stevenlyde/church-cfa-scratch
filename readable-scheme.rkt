#lang racket

(define rest cdr)
(define pair cons)
         
(define true #t)
(define false #f)
         
(define (true? x) (not (eq? x false)))
         
(define (false? x) (eq? x false))
         
(define (repeat n thunk)
  (if (> n 0)
      (pair (thunk) (repeat (- n 1) thunk))
      (list)))
         
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(provide pair rest true false true? false? repeat tagged-list?)

