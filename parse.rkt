#lang racket
(provide parse parse-e parse-define)
(require "ast.rkt")

;; S-Expr ... -> Prog
(define (parse . s)
  (match s
    [(cons (and (cons 'define _) d) s)
     (match (apply parse s)
       [(Prog ds e)
        (Prog (cons (parse-define d) ds) e)])]
    [(cons e '()) (Prog '() (parse-e e))]
    [_ (error "program parse error")]))

;; S-Expr -> Defn
(define (parse-define s)
  (match s
    [(list 'define (cons (? symbol? f) xs) e)
     (Defn f (parse-param-list FunRest FunPlain xs e))]
    [_ (error "Parse defn error" s)]))

;; S-Expr -> Expr
(define (parse-e s)
  (match s
    [(? datum?)               (Lit s)]
    ['eof                     (Eof)]
    [(? symbol?)              (Var s)]
    [(list 'quote (list))     (Lit '())]
    [(list (? op0? p0))       (Prim0 p0)]
    [(list (? op1? p1) e)     (Prim1 p1 (parse-e e))]
    [(list (? op2? p2) e1 e2) (Prim2 p2 (parse-e e1) (parse-e e2))]
    [(list (? op3? p3) e1 e2 e3)
     (Prim3 p3 (parse-e e1) (parse-e e2) (parse-e e3))]
    [(cons (? opN? p) es)
     (PrimN p (map parse-e es))]
    [(list 'begin e1 e2)
     (Begin (parse-e e1) (parse-e e2))]
    [(list 'if e1 e2 e3)
     (If (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list 'let (list (list (? symbol? x) e1)) e2)
     (Let x (parse-e e1) (parse-e e2))]
    [(list 'apply fe es ... e)
     (Apply (parse-e fe) (map parse-e es) (parse-e e))]
    [(cons 'match (cons e ms))
     (parse-match (parse-e e) ms)]
    [(list (or 'lambda 'λ) xs e)
     (parse-param-list (λ (xs r e) (LamRest  (gensym 'lamrest)  xs r e))
                       (λ (xs   e) (LamPlain (gensym 'lamplain) xs   e))
                       xs
                       e)]
    [(list 'raise e)
     (Raise (parse-e e))]
    [(list 'with-handlers (list (list ps hs) ...) e)
     (WithHandlers (map parse-e ps) (map parse-e hs) (parse-e e))]
    [(cons e es)
     (App (parse-e e) (map parse-e es))]
    [_ (error "Parse error" s)]))

;; ([Listof Id] Id Expr -> A) ([Listof Id] Expr -> B) S-Expr S-Expr -> A | B
;; This function takes constructors as its first two arguments. The first is
;; used when the parameter list ends in a symbol (the "rest" case) and the
;; second is used when the list ends in an empty list (the "plain" case).
(define (parse-param-list build-rest build-plain xs e)
  (match xs
    ;; Matches an improper list that ends in a symbol, e.g., '(a b c . xs).
    [(list-rest (? symbol? xs) ... (? symbol? r)) (build-rest  xs r (parse-e e))]
    ;; Matches a proper list that ends in an empty list, e.g., '(a b c).
    [(list-rest (? symbol? xs) ... '())           (build-plain xs   (parse-e e))]
    [_ (error "parse parameter list error")]))

;; Expr [Listof S-Expr]
(define (parse-match e ms)
  (match ms
    ['() (Match e '() '())]
    [(cons (list p r) ms)
     (match (parse-match e ms)
       [(Match e ps es)
        (Match e
               (cons (parse-pat p) ps)
               (cons (parse-e r) es))])]
    [_ (error "Parse match error" e ms)]))

;; S-Expr -> Pat
(define (parse-pat p)
  (match p
    [(? datum?) (Lit p)]
    [(? symbol?) (Var p)]
    [(list 'quote (list)) (Lit '())]
    [(list 'box p)
     (Box (parse-pat p))]
    [(list 'cons p1 p2)
     (Cons (parse-pat p1) (parse-pat p2))]
    [(list 'and p1 p2)
     (Conj (parse-pat p1) (parse-pat p2))]))


;; Any -> Boolean
(define (datum? x)
  (or (exact-integer? x)
      (boolean? x)
      (char? x)
      (string? x)))

;; Any -> Boolean
(define (op0? x)
  (memq x '(read-byte peek-byte void)))

(define (op1? x)
  (memq x '(add1 sub1 zero? char? integer->char char->integer
                 write-byte eof-object?
                 box unbox empty? cons? box? car cdr
                 vector? vector-length string? string-length)))

(define (op2? x)
  (memq x '(+ - < = eq? cons
              make-vector vector-ref make-string string-ref)))

(define (op3? x)
  (memq x '(vector-set!)))

(define (opN? x)
  (memq x '(list vector)))
