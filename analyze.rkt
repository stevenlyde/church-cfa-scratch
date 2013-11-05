#lang racket

(require "desugar.rkt"
         "simplify.rkt"
         "anf.rkt"
         "cfa.rkt")

(define (run-analysis exp)
  (let* ([exp (church-desugar exp)]
         [exp (desugar exp)]
         [exp (normalize exp)])
    (analyze exp)))

(let loop ([token (read)] [top-list empty])
  (if (eof-object? token)
      (run-analysis `(begin
                       (load "primitive-header.rkt")
                       ,@(reverse top-list)))
      (loop (read) (cons token top-list))))
