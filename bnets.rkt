#lang racket


(require rackunit)


#|

(define samples
  (mh-query 200 100
    (define smokes (flip 0.2))

    (define lung-disease (or (flip 0.001) (and smokes (flip 0.1))))
    (define cold (flip 0.02))

    (define cough (or (and cold (flip 0.5)) (and lung-disease (flip 0.5)) (flip 0.01)))
    (define fever (or (and cold (flip 0.3)) (flip 0.01)))
    (define chest-pain (or (and lung-disease (flip 0.2)) (flip 0.01)))
    (define shortness-of-breath (or (and lung-disease (flip 0.2)) (flip 0.01)))

   (list cold lung-disease)

   cough
  )
)
(hist (map first samples) "cold")
(hist (map second samples) "lung-disease")
(hist samples "cold, lung-disease")

|#

#|

smokes
T   F
0.2 0.8

       lung-disease
smokes T       F
T      0.1009  0.8991
F      0.001   0.999


       lung-disease
smokes T       F
T      0.101   0.899
F      0.001   0.999



smokes -> lung-disease

lung-disease -> shortness-of-breath
lung-disease -> chest-pain
lung-disease -> cough

cold -> cough
cold -> fever

|#



#|


;;        lung-disease
;; smokes T       F
;; T      1       0                  
;; F      0.001   0.999            



(lung-disease (smokes)
  (true 0.101 0.899)
  (false 0.001 0.999))


- need a way to write the Bayesian network in s-expression form

- idenitfy the dependencies of an expression
 - if there are vars involved in in the expression then there is a conditional dependence (possibly)
- for each combination of values for dependent vars evaluate the precentage of it being true and it being false
- done

|#



(define (vars-of exp [vars (set)])
  (match exp
    [`(and ,es ...)
     (foldl (λ (e v) (set-union v (vars-of e))) vars es)]
    [`(or ,es ...)
     (foldl (λ (e v) (set-union v (vars-of e))) vars es)]
    [`(flip ,n)
     (set)]
    [(? symbol? v)
     (set v)]))


(define (fold-combination vars f acc [env (hash)])
  (match vars
    [(list-rest v others)
     (define new-acc (fold-combination others f acc (hash-set env v #t)))
     (fold-combination others f new-acc (hash-set env v #f))]
    [(? empty?) (f env acc)]))


(define (probability-of exp env)
  (match exp
    [`(and ,es ...) (foldl (λ (e p) (* p (probability-of e env))) 1 es)]
    [`(or ,es ...) (- 1.0 (foldl (λ (e p) (* p (- 1.0 (probability-of e env))))  1 es))]
    [`(flip ,n) n]
    [(? symbol? v) (if (hash-ref env v) 1.0 0.0)]))


(define (conditional-dependence-of s)
  (match s
    [`(define ,var ,exp)
     ; =>
     (define vars (set->list (vars-of exp)))
     (define prob 
       (fold-combination vars 
                         (λ (env acc)
                           (define tp (probability-of exp env))
                           (cons `(,@(map (curry hash-ref env) vars) ,tp ,(- 1.0 tp)) acc)) 
                         empty))     
     `(,var ,vars ,@(reverse prob))]))

#|
(check-equal? 
 (conditional-dependence-of '(define lung-disease (or (flip 0.001) (and smokes (flip 0.1)))))
 '(lung-disease (smokes)
                (#t 0.101 0.899)
                (#f 0.001 0.999)))
|#

(define program 
  '(
    (define smokes (flip 0.2))
    
    (define lung-disease (or (flip 0.001) (and smokes (flip 0.1))))
    (define cold (flip 0.02))
    
    (define cough (or (and cold (flip 0.5)) (and lung-disease (flip 0.5)) (flip 0.01)))
    (define fever (or (and cold (flip 0.3)) (flip 0.01)))
    (define chest-pain (or (and lung-disease (flip 0.2)) (flip 0.01)))
    (define shortness-of-breath (or (and lung-disease (flip 0.2)) (flip 0.01)))
    ))

(map conditional-dependence-of program)


; (conditional-dependence-of '(define cough (or (and cold (flip 0.5))
;                                               (and lung-disease (flip 0.5))
;                                               (flip 0.01))))

; T T T
; T T F
; T F T
; T F F
; F T T
; F T F
; F F T
; F F F    0.5 * 0.5 * 0.99

; what is the probability of getting false for each term

; 0.7525 0.2475

; factor graph
