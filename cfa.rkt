#lang racket

(require "unique.rkt")

(provide analyze)

;; Helpers
(define empty-set (set))

(define (exp->label exp) exp)

; map-set : (a -> b) (set a) -> (set b)
(define (map-set f s)
  (for/fold ([ns empty-set])
    ([e (in-set s)])
    (set-add ns (f e))))

; take* is like take but allows n to be larger than (length l)
(define (take* l n)
  (for/list ([e (in-list l)]
             [i (in-naturals)]
             #:when (i . < . n))
    e))

;; Abstract state-space.

;; state ::= (state exp env addr time)
(struct state (exp env kaddr time) #:transparent)

;; env = hash[var,addr]
;; A addr environment maps variables to addresses.
(define empty-env (make-immutable-hasheq empty))

; env-lookup : env var -> addr
(define env-lookup hash-ref)

; env-extend : env var addr -> env
(define env-extend hash-set)

; env-extend* : env list[var] list[addr] -> env
(define (env-extend* env vars addrs)
  (for/fold ([env env])
    ([v (in-list vars)]
     [a (in-list addrs)])
    (env-extend env v a)))  

;; store = hash[addr,d]
;; A store (or a heap/memory) maps address to denotable values.
(define empty-store (make-immutable-hash empty))

; store-lookup : store addr -> d
(define (store-lookup s a)
  (hash-ref s a empty-set))

; store-update : store addr d -> store
(define (store-update store addr value)
  (hash-update store addr 
               (lambda (d) (set-union d value))
               empty-set))

; store-update* : store list[addr] list[d] -> store
(define (store-update* store addrs values)
  (for/fold ([store store])
    ([a (in-list addrs)]
     [v (in-list values)])
    (store-update store a v)))

; store-join : store store -> store
(define (store-join store1 store2)
  (for/fold ([new-store store1])
    ([(k v) (in-hash store2)])
    (store-update new-store k v)))

;; value = clo
;; For pure CPS, closures are the only kind of value.

;; clo ::= (make-closure <lambda> <env>)
;; Closures pair a lambda term with a addr environment that
;; determinse the value of its free variables.
(struct closure (lam env) #:prefab)

;; addr = bind
;; Addresses can point to values in the store.
;; In pure CPS, the only kind of addresses are addrs.

;; bind ::= (make-addr <var> <time>)
;; A addr is minted each time a variable gets bound to a value.
(struct addr (var time) #:prefab)

;; time = (listof label)
;; In k-CFA, time is a bounded memory of program history.
;; In particular, it is the last k call sites through which
;; the program has traversed.
(define time-zero empty)

;; Continuations

(struct halt () #:prefab)
(struct letk (var body env kaddr) #:prefab)

(struct kontp (kaddr) #:prefab)

;; k-CFA parameters

;; Change these to alter the behavior of the analysis.

; k : natural
(define k 0)

; tick : exp time -> time
(define (tick exp time)
  (take* (list* (exp->label exp) time) k))

; alloc : var -> time -> addr
(define ((alloc time) var)
  (addr var time))

;; k-CFA abstract interpreter                 

; lambda? : exp -> boolean
(define (lambda? exp)
  (match exp
    [`(,(or 'lambda 'λ) . ,_) #t]
    [_ #f]))



(struct cons-lit (a d) #:prefab)
(struct empty-lit () #:prefab)
(struct prim-lit (e) #:prefab)
(struct quote-lit (datum) #:prefab)

(define number-primitive '(+ - * / gaussian uniform worldWidth worldHeight))
(define (number-primitive? p) (member p number-primitive))

(define boolean-primitive '(< > <= >= = flip equal? and or null?))
(define (boolean-primitive? p) (member p boolean-primitive))

(define pair-primitive '(cons car cdr))
(define (pair-primitive? p) (member p pair-primitive))

; atomic? exp -> boolean
(define (atomic? exp)
  (match exp
    [(? lambda?) #t]
    [(? symbol?) #t]
    [(? number?) #t]
    [(? boolean?) #t]
    [(? string?) #t]
    [`(quote ,_) #t]
    
    [`(,(? number-primitive?) . ,args) #t]
    [`(,(? boolean-primitive?) . ,args) #t]
    [`(,(? pair-primitive?) . ,args) #t]
    
    [`(uniform-draw ,_) #t]
    
    ['(void) #t]
    ['(list) #t]
    
    [else #f]))

; atomic-eval : env store -> exp -> d
(define ((atomic-eval env store) exp)
  (match exp
    [(? boolean?) (set exp)]
    [(? number?) (set 'number)]
    [(? string?) (set 'string)]
    [`(quote ,datum) (set (quote-lit datum))]
    
    ['(void) (set)]
    ['(list) (set (empty-lit))]
    
    [`(,(? number-primitive?) . ,args) (set 'number)]
    [`(,(? boolean-primitive?) . ,args) (set #t #f)]
    
    [`(cons ,a ,d) (set (cons-lit ((atomic-eval env store) a) ((atomic-eval env store) d)))]

    [`(car ,p)  (for/fold ([ds (set)])
                          ([d ((atomic-eval env store) p)])
                  (match d
                    [(cons-lit a d) (set-add ds a)]
                    [else ds]))]
    [`(cdr ,p)  (for/fold ([ds (set)])
                          ([d ((atomic-eval env store) p)])
                  (match d
                    [(cons-lit a d) (set-add ds d)]
                    [else ds]))]
    
    [`(uniform-draw ,lst) (letrec ([lsts ((atomic-eval env store) lst)]
                                   [alist? (λ (lst) (match lst
                                                   [(cons-lit a d)  (alist? d)]
                                                   [(quote-lit '()) #t]
                                                   [else            #f]))]
                                   [vals (λ (lst) (match lst
                                                   [(cons-lit a d)  (cons a (vals d))]
                                                   [(quote-lit '()) empty]))])
                            (for/fold ([ds (set)]) ([lst lsts])
                              (if (alist? lst) (vals lst) ds)))]
    
    [(or (? number-primitive?) (? boolean-primitive?) (? pair-primitive?)) (set (prim-lit exp))]
    
    [(? symbol? var)
     (store-lookup store (env-lookup env var))]
    [(? lambda? lam)
     (set (closure lam env))]))

; next : state store -> (values list[state] store)
(define (next s store)
  (match-define (state exp env kaddr time) s)
  (define time* (tick exp time))
    
  (match exp
    
    [`(let ([,var ,val-exp]) ,body)
     ; =>
     (define kaddr* ((alloc time*) exp))
     (define kont (letk var body env kaddr))
     (define store* (store-update store kaddr* (set kont)))
     (values (list (state val-exp env kaddr* time*))
             store*)]
    
    [`(letrec ([,var ,lam]) ,body)
     ; =>
     (define addr ((alloc time*) var))
     (define env* (env-extend env var addr))
     (define clo ((atomic-eval env* store) lam))
     (define store* (store-update store addr clo))
     (values (list (state body env* kaddr time*))
             store*)]
    
    [`(set! ,var ,aexp)
     ; =>     
     (define val ((atomic-eval env store) aexp))
     (define store* (store-update store (env-lookup env var) val))
     (values (list (state '(void) env kaddr time*))
             store*)]
    
    [`(if ,aexp ,true-exp ,false-exp)
     ; =>
     (values
      (for/list ([val ((atomic-eval env store) aexp)])
        (if val
            (state true-exp env kaddr time*)
            (state false-exp env kaddr time*)))
      store)]
    
    [`(call/cc ,f)
     (define procs ((atomic-eval env store) f))
     (define params (list (set (kontp kaddr))))
     (for/fold ([states empty] [store store]) ([proc procs])
       (match-define (closure (list (or 'λ 'lambda) formals call) env*) proc)
       (define addrs (map (alloc time*) formals))
       (define env** (env-extend* env* formals addrs))
       (define store* (store-update* store addrs params))
       (values (cons (state call env** kaddr time*) states) store*))]
    
    [(? atomic? aexp)
     ; =>
     (define val ((atomic-eval env store) aexp))
     (for/fold ([states empty] [store store]) ([kont (store-lookup store kaddr)] #:when (not (halt? kont)))
       (match kont
         [(struct letk (var body env* kaddr*))
          (define addr ((alloc time) var))
          (define store* (store-update store addr val))
          (define env** (env-extend env* var addr))
          (values (cons (state body env** kaddr* time*) states) store*)]))]
    
    [`(,f ,args ...)
     ; =>
     (define procs ((atomic-eval env store) f))
     (define params (map (atomic-eval env store) args))
     (for/fold ([states empty] [store store]) ([proc procs])
       (match proc
         [(closure (list (or 'λ 'lambda) formals call) env*)
          (define addrs (map (alloc time*) formals))
          (define env** (env-extend* env* formals addrs))
          (define store* (store-update* store addrs params))
          (values (cons (state call env** kaddr time*) states) store*)]
         [(prim-lit p)
          (next (state `(,p ,@args) env kaddr time) store)]
         [(kontp a)
          (define konts (store-lookup store a))
          (for/fold ([states empty] [store store]) ([kont konts])
            (match kont
              [(struct letk (var body env* kaddr*))
               (define addr ((alloc time) var))
               (define store* (store-update store addr (set))) ; TODO: this is assuming that there is nothing ever passed to the continuation
               (define env** (env-extend env* var addr))
               (values (cons (state body env** kaddr* time*) states) store*)]))]))]))


;; State-space exploration.

; exlore : set[state] list[state] store -> (values set[state] store regions)
(define (explore seen todo store)
  (match todo
    [(list)
     (values seen store)]
    [(list-rest (? (curry set-member? seen) st0) todo)
     (explore seen todo store)]
    [(list-rest st0 todo)
     (let-values ([(succs new-store) (next st0 store)])
       (for ([st1 succs])
         (define mark (mark-of-beast st0))
         ;(fprintf graph-out "~a [label=\"~a ~a\"];~n" mark mark (state-exp st0))
         ;(fprintf graph-out "~a [label=\"~a\"];~n" mark mark)
         (fprintf graph-out "~a -> ~a;~n" mark (mark-of-beast st1)))
       (explore (set-add seen st0) (append succs todo) new-store))]))

; analyze : exp -> mono-summary
(define (analyze exp)
  (fprintf graph-out "digraph states {~n")
  
  (define init-state (state exp empty-env (halt) time-zero))
  (define-values (states store) (explore empty-set (list init-state) empty-store))
  
  (fprintf graph-out "}~n")
  (flush-output graph-out)
  
  (for ([s states])
    (fprintf state-out "~a :: ~n~a~n" (mark-of-beast s) s))
  (flush-output state-out)
  
  (void))


(define graph-out (open-output-file "state-space.gv" #:exists 'truncate))
(define state-out (open-output-file "state-space.txt" #:exists 'truncate))

