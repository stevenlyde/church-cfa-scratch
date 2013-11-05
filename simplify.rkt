#lang racket

(provide desugar)

;;; Additional desugarings desired before converting to ANF.

; desugar : exp -> exp
(define (desugar exp)
  (match exp
    [(? number?) exp]
    [(? string?) exp]
    [(? symbol?) exp]
    [(? boolean?) exp]
    
    ['(list)
     '(quote ())]
    
    [`(list ,v ,u ...)
     `(cons ,(desugar v) ,(desugar `(list ,@u)))]
        
    [`(lambda (,v ...) ,body)
     `(lambda ,v ,(desugar body))]
    
    [`(lambda (,v ...) . ,body)
     `(lambda ,v ,(desugar-body body))]
    
    [`(letrec ((,vs ,es) ...) . ,body)
     `(let ,(for/list ([v vs])
              (list v '(void)))
        (begin
          ,@(map (λ (v e)
                   `(set! ,v ,(desugar e)))
                 vs es)
          ,@(map desugar body)))]
    
    ; In the abstract interpration with the current domains used a memoized lambda is equivalent to the original.
    [`(mem ,lambda) (desugar lambda)]
    
    [`(,app ...) (map desugar app)]))

(define (desugar-body body)
  (let ([vars (reverse (for/fold ([vars empty])
                         ([exp body])
                         (match exp
                           [`(define ,id ,_) (cons id vars)]
                           [else vars])))])
    (if (zero? (length vars))
        `(begin ,@(desugar-body body))
        `(let ,(for/list ([v vars])
                 (list v '(void)))
           (begin ,@(map (lambda (exp)
                           (match exp
                             [`(define ,id ,exp) `(set! ,id ,(desugar exp))]
                             [else (desugar exp)]))
                         body))))))
