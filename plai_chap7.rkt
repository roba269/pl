#lang plai-typed

(define (mycube2 x) (* x x))

(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  ; [fdC (fun : symbol) (arg : symbol) (body : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  )

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])

;(define-type FunDefC
;  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

;(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
;  (type-case ExprC in
;    [numC (n) in]
;    [idC (s) (cond
;              [(symbol=? s for) what]
;              [else in])]
;    [appC (f a) (appC f (subst what for a))]
;    [plusC (l r) (plusC (subst what for l) (subst what for r))]
;    [multC (l r) (multC (subst what for l) (subst what for r))]
;  )
;)
;
;(define (interp_subst [e : ExprC] [fds : (listof FunDefC)]) : number
;  (type-case ExprC e
;    [numC (n) n]
;    [plusC (l r) (+ (interp_subst l fds) (interp_subst r fds))]
;    [multC (l r) (* (interp_subst l fds) (interp_subst r fds))]
;    [idC (s) 0]
;    [appC (fun arg) 0]
;  )
;)

(define-type Binding
  [bind (name : symbol) (val : Value)])
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define (lookup [for : symbol] [env : Env]) : Value
  (cond
    [(empty? env) {error 'lookup "name not found"}]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))

;(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
;  (cond
;    [(empty? fds) (error 'get-fundef "reference to undefined function")]
;    [(cons? fds) (cond
;                   [(equal? n (fdC-name (first fds))) (first fds)]
;                   [else (get-fundef n (rest fds))])]))

;(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
;  (cond
;    [(empty? fds) (error 'get-fundef "reference to undefined function")]
;    [(cons? fds) (cond
;                   [(symbol=? n (fdC-name (first fds))) (first fds)]
;                   [else (get-fundef n (rest fds))])]))

(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-n l) (numV-n r)))]
    [else (error 'num+ "arguments should be number")]))

(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-n l) (numV-n r)))]
    [else (error 'num* "arguments should be number")]))

(define (interp [expr : ExprC] [env : Env]) : Value
  (type-case ExprC expr
    [numC (n) (numV n)]
    [idC (n) (lookup n env)]
    ; [fdC (n a b) (funV n a b)]
    [lamC (a b) (closV a b env)]
    [appC (f a) (local ([define f-value (interp f env)])
                  (interp (closV-body f-value)
                          (extend-env (bind (closV-arg f-value)
                                            (interp a env))
                                      env)))]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]))