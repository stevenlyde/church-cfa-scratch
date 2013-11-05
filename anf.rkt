#lang racket

(provide (rename-out [normalize-term normalize]))

(define (atomic? exp)
  (match exp
    [(? symbol?)  #t]
    [(? boolean?) #t]
    [(? number?)  #t]
    [(? string?)  #t]
    [`(quote ,_)  #t]
    [else         #f]))

(define (normalize-term exp)
  (normalize exp (λ (x) x)))

(define (normalize exp k)
  (match exp
    [`(lambda ,params ,body)  
     (k `(lambda ,params ,(normalize-term body)))]
    
    [`(let () ,exp)
     (normalize exp k)]
    
    [`(let ([,x ,exp1] . ,clause) ,exp2) 
     (normalize exp1 (λ (aexp1) 
                       `(let ([,x ,aexp1])
                          ,(normalize `(let (,@clause) ,exp2) k))))]    
    
    [`(letrec ([,x ,exp1]) ,exp2)
     (normalize exp1 (λ (aexp1) 
                       `(letrec ([,x ,aexp1])
                          ,(normalize exp2 k))))]
    
    [`(begin ,exp)
     (normalize-name exp (λ (t) (k t)))]
    
    [`(begin ,exp . ,rest)
     (normalize-name exp (λ (t) 
                           `(let ([,(gensym '_) ,t])
                              ,(normalize `(begin ,@rest) k))))]
    
    [`(if ,exp1 ,exp2 ,exp3)
     (normalize-name exp1 (λ (t)
                            (k `(if ,t ,(normalize-term exp2)
                                    ,(normalize-term exp3)))))]    
    [`(set! ,v ,exp)
     (normalize-name exp (λ (t)
                           `(let ([,(gensym '_) (set! ,v ,t)])
                              ,(k v))))]
    
    [`(call/cc ,exp)
     (normalize-name exp (λ (c)
                           (k `(call/cc ,c))))]

    [(? atomic?)
     (k exp)]
    
    [`(,f . ,args)
     (normalize-name f (λ (f)
                         (normalize-name* args (λ (args)
                                                 (k `(,f . ,args))))))]
    
    ))

(define (normalize-name exp k)
  (normalize exp (λ (exp)
                   (if (atomic? exp)
                       (k exp)
                       (let ([t (gensym)])
                         `(let ([,t ,exp]) ,(k t)))))))

(define (normalize-name* exps k)
  (if (empty? exps)
      (k empty)
      (normalize-name (car exps) (λ (t) 
                                   (normalize-name* (cdr exps) (λ (t*) 
                                                                 (k `(,t . ,t*))))))))
