#lang racket
(provide interp)
(provide interp-env)
(provide interp-match-pat)
(require "ast.rkt")
(require "interp-prim.rkt")

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void
;; | '()
;; | (cons Value Value)
;; | (box Value)
;; | (string Character ...)
;; | (vector Value ...)
;; | (Value ... -> Answer)

;; type Answer = Value | 'err

;; type Env = (Listof (List Id Value))
;; Prog -> Answer
(define (interp p)
  (match p
    [(Prog ds e)
     (with-handlers ([exn? (λ (e) (raise e))]
                     [(λ (_) #t) (λ (_) 'err)])
       (interp-env e '() ds))]))

;; Expr Env Defns -> Answer
(define (interp-env e r ds)
  (match e
    [(Lit d) d]
    [(Eof)   eof]
    [(Var x) (interp-var x r ds)]
    [(Prim0 p) (interp-prim0 p)]
    [(Prim1 p e)
     (match (interp-env e r ds)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r ds)
       ['err 'err]
       [v1 (match (interp-env e2 r ds)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]
    [(Prim3 p e1 e2 e3)
     (match (interp-env e1 r ds)
       ['err 'err]
       [v1 (match (interp-env e2 r ds)
             ['err 'err]
             [v2 (match (interp-env e3 r ds)
                   ['err 'err]
                   [v3 (interp-prim3 p v1 v2 v3)])])])]
    [(PrimN p es)
     (match (interp-env* es r ds)
       ['err 'err]
       [vs (interp-primN p vs)])]
    [(If e0 e1 e2)
     (match (interp-env e0 r ds)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r ds)
            (interp-env e2 r ds))])]
    [(Begin e1 e2)
     (match (interp-env e1 r ds)
       ['err 'err]
       [v    (interp-env e2 r ds)])]
    [(Let x e1 e2)
     (match (interp-env e1 r ds)
       ['err 'err]
       [v (interp-env e2 (ext r x v) ds)])]
    [(App e es)
     (match (interp-env e r ds)
       ['err 'err]
       [f
        (match (interp-env* es r ds)
          ['err 'err]
          [vs
           (if (procedure? f)
               (apply f vs)
               'err)])])]
    [(Apply ef es e)
     (match (interp-env ef r ds)
       ['err 'err]
       [f
        (match (interp-env* es r ds)
          ['err 'err]
          [vs
           (match (interp-env e r ds)
             ['err 'err]
             [ws
              (if (and (procedure? f)
                       (list? ws))
                  (apply f (append vs ws))
                  'err)])])])]
    [(Match e ps es)
     (match (interp-env e r ds)
       ['err 'err]
       [v
        (interp-match v ps es r ds)])]
    [(LamPlain f xs e)
     (λ vs
       ; check arity matches - args and params must be same length
       (if (= (length vs) (length xs))
           (interp-env e (append (zip xs vs) r) ds)
           'err))]
    [(LamRest f xs x e)
     (λ vs
       ; check arity matches - must have at least enough args
       (if (>= (length vs) (length xs))
           ; separate the needed args (normal parameters) from the additional
           ; args (bound to the distinguished list/rest parameter)
           (let-values ([(required-args additional-args) (split-at vs (length xs))])
             (interp-env e (cons (list x additional-args) (append (zip xs required-args) r)) ds))
           'err))]
    [(Raise e) (raise (interp-env e r ds))]
    [(WithHandlers ps hs e) (interp-with-handlers ps hs e r ds)]))

;; (Listof Expr) REnv Defns -> (Listof Value) | 'err
(define (interp-env* es r ds)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r ds)
       ['err 'err]
       [v (match (interp-env* es r ds)
            ['err 'err]
            [vs (cons v vs)])])]))

;; Id Env [Listof Defn] -> Answer
(define (interp-var x r ds)
  (match (lookup r x)
    ['err (match (defns-lookup ds x)
            [(Defn f fun)
             (match fun
               [(FunPlain xs e) (interp-env (LamPlain f xs e) '() ds)]
               [(FunRest xs x e) (interp-env (LamRest f xs x e) '() ds)])]
            [#f 'err])]
    [v v]))

;; [Listof Expr] [Listof Expr] Expr Env [Listof Defn] -> Answer
(define (interp-with-handlers pes hes e r ds)
  (if (empty? pes)
      (interp-env e r ds)
      (match (interp-env* pes r ds)
        ['err 'err]
        [ps
         (match (interp-env* hes r ds)
           ['err 'err]
           [hs
            ;; The first clause allows Racket exceptions to bypass any potential
            ;; interference from within the interpreter.
            (with-handlers ([exn?     (λ (e) (raise e))]
                            [(λ _ #t) (λ (v) (handle-exceptions v ps hs))])
              (interp-env e r ds))])])))

;; Value [Listof Value] [Listof Value] -> Answer
(define (handle-exceptions v ps hs)
  (match* (ps hs)
    [('() '()) (raise v)]
    [((cons p ps) (cons h hs))
     (if (not (procedure? p))
         'err
         (match (p v)
           ['err 'err]
           [#f (handle-exceptions v ps hs)]
           [_
            (if (not (procedure? h))
                'err
                (h v))]))]))

;; Value [Listof Pat] [Listof Expr] Env Defns -> Answer
(define (interp-match v ps es r ds)
  (match* (ps es)
    [('() '()) 'err]
    [((cons p ps) (cons e es))
     (match (interp-match-pat p v r)
       [#f (interp-match v ps es r ds)]
       [r  (interp-env e r ds)])]))

;; Pat Value Env -> [Maybe Env]
(define (interp-match-pat p v r)
  (match p
    [(Var '_) r]
    [(Var x) (ext r x v)]
    [(Lit l) (and (eqv? l v) r)]
    [(Box p)
     (match v
       [(box v)
        (interp-match-pat p v r)]
       [_ #f])]
    [(Cons p1 p2)
     (match v
       [(cons v1 v2)
        (match (interp-match-pat p1 v1 r)
          [#f #f]
          [r1 (interp-match-pat p2 v2 r1)])]
       [_ #f])]
    [(Conj p1 p2)
     (match (interp-match-pat p1 v r)
       [#f #f]
       [r1 (interp-match-pat p2 v r1)])]))

;; Defns Symbol -> Defn
(define (defns-lookup ds f)
  (findf (match-lambda [(Defn g _) (eq? f g)])
         ds))

(define (zip xs ys)
  (match* (xs ys)
    [('() '()) '()]
    [((cons x xs) (cons y ys))
     (cons (list x y)
           (zip xs ys))]))

;; Env Id -> Answer
(define (lookup env x)
  (match env
    ['() 'err]
    [(cons (list y i) env)
     (match (symbol=? x y)
       [#t i]
       [#f (lookup env x)])]))

;; Env Id Value -> Env
(define (ext r x v)
  (cons (list x v) r))
