#lang racket
(provide Lit Prim0 Prim1 Prim2 Prim3 PrimN If Eof Begin
         Let Var Prog Defn FunPlain FunRest App Apply
         Match  Box Cons Conj
         Raise WithHandlers
         LamPlain LamRest)

;; type Prog = (Prog (Listof Defn) Expr)
(struct Prog (ds e) #:prefab)

;; type Defn = (Defn Id Fun)
(struct Defn (f fun) #:prefab)

;; type Fun = (FunPlain [Listof Id] Expr)
;;          | (FunRest [Listof Id] Id Expr)
(struct FunPlain (xs e)   #:prefab)
(struct FunRest  (xs x e) #:prefab)

;; type Expr = (Lit Datum)
;;           | (Eof)
;;           | (Prim0 Op0)
;;           | (Prim1 Op1 Expr)
;;           | (Prim2 Op2 Expr Expr)
;;           | (Prim3 Op3 Expr Expr Expr)
;;           | (PrimN OpN (Listof Expr))
;;           | (If Expr Expr Expr)
;;           | (Begin Expr Expr)
;;           | (Let Id Expr Expr)
;;           | (Var Id)
;;           | (App Expr (Listof Expr))
;;           | (Apply Expr (Listof Expr) Expr)
;;           | (Match Expr (Listof Pat) (Listof Expr))
;;           | (LamPlain Id (Listof Id) Expr)
;;           | (LamRest Id (Listof Id) Id Expr)
;;           | (Raise Expr)
;;           | (WithHandlers (Listof Expr) (Listof Expr) Expr)

;; type Id  = Symbol
;; type Datum = Integer
;;            | Boolean
;;            | Character
;;            | String
;; type Op0 = 'read-byte | 'peek-byte | 'void
;; type Op1 = 'add1 | 'sub1
;;          | 'zero?
;;          | 'char? | 'integer->char | 'char->integer
;;          | 'write-byte | 'eof-object?
;;          | 'box | 'car | 'cdr | 'unbox
;;          | 'empty? | 'cons? | 'box?
;;          | 'vector? | 'vector-length
;;          | 'string? | 'string-length
;; type Op2 = '+ | '- | '< | '=
;;          | 'eq? | 'cons
;;          | 'make-vector | 'vector-ref
;;          | 'make-string | 'string-ref
;; type Op3 = 'vector-set!
;; type OpN = 'list | 'vector
;; type Pat  = (Var Id)
;;           | (Lit Datum)
;;           | (Box Pat)
;;           | (Cons Pat Pat)
;;           | (Conj Pat Pat)

(struct Eof () #:prefab)
(struct Lit (d) #:prefab)
(struct Prim0 (p) #:prefab)
(struct Prim1 (p e) #:prefab)
(struct Prim2 (p e1 e2) #:prefab)
(struct Prim3 (p e1 e2 e3) #:prefab)
(struct PrimN (p es) #:prefab)
(struct If (e1 e2 e3) #:prefab)
(struct Begin (e1 e2) #:prefab)
(struct Let (x e1 e2) #:prefab)
(struct Var (x) #:prefab)
(struct App (f es) #:prefab)
(struct Apply (ef es e) #:prefab)
(struct LamPlain (f xs e) #:prefab)
(struct LamRest (f xs x e) #:prefab)
(struct Match (e ps es) #:prefab)
(struct Raise (e) #:prefab)
(struct WithHandlers (ps hs e) #:prefab)

(struct Box (p) #:prefab)
(struct Cons (p1 p2) #:prefab)
(struct Conj (p1 p2) #:prefab)
