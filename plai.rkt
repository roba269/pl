#lang plai-typed

(define (mycube2 x) (* x x))

(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)])

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

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
  [bind (name : symbol) (val : number)])
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define (lookup [for : symbol] [env : Env]) : number
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

(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(symbol=? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

(define (interp [expr : ExprC] [env : Env] [fds : (listof FunDefC)]) : number
  (type-case ExprC expr
    [numC (n) n]
    [idC (n) (lookup n env)]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (interp (fdC-body fd)
                          (extend-env (bind (fdC-arg fd)
                                            (interp a env fds))
                                      mt-env)
                          fds)
                  )]
    [plusC (l r) (+ (interp l env fds) (interp r env fds))]
    [multC (l r) (* (interp l env fds) (interp r env fds))]))