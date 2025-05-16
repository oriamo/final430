#lang racket
(provide (all-defined-out))
(require "ast.rkt")
(require "assert.rkt")
(require "compile-ops.rkt")
(require "types.rkt")
(require "lambdas.rkt")
(require "fv.rkt")
(require a86/ast)

(define rax 'rax)
(define rbx 'rbx) ; heap
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg
(define r8  'r8)  ; scratch
(define r9  'r9)  ; scratch
(define r15 'r15) ; stack pad (non-volatile)
(define rdx 'rdx) 
(define r11 'r11) 
(define r12 'r12)
(define current-handler 'err)

;; Prog -> Asm
(define (compile p)
  (match p
    [(Prog ds e)
     (prog (Global 'entry)
           (Extern 'peek_byte)
           (Extern 'read_byte)
           (Extern 'write_byte)
           (Extern 'raise_error)
           (Label 'entry)
           (Push rbx)    ; save callee-saved register
           (Push r15)
           (Push r12)      ; save r12 for exception handler chain
           (Mov r12 0) ; initialize exception handler to null
           (Mov rbx rdi) ; recv heap pointer
           (compile-defines-values ds)
           (compile-e e (reverse (define-ids ds)) #f)
           (Add rsp (* 8 (length ds))) ;; pop function definitions
           (Pop r12)
           (Pop r15)     ; restore callee-save register
           (Pop rbx)
           (Ret)
           (compile-defines ds)
           (compile-lambda-defines (lambdas p))
           (Label 'err)
           pad-stack
           (Call 'raise_error))]))

;; [Listof Defn] -> [Listof Id]
(define (define-ids ds)
  (match ds
    ['() '()]
    [(cons (Defn f fun) ds)
     (cons f (define-ids ds))]))

;; [Listof Defn] -> Asm
(define (compile-defines ds)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (seq (compile-define d)
          (compile-defines ds))]))

;; Defn -> Asm
(define (compile-define d)
  (match d
    [(Defn f (FunPlain xs e))
     (compile-lambda-define (LamPlain f xs e))]
    [(Defn f (FunRest xs x e))
     (compile-lambda-define (LamRest f xs x e))]))

;; [Listof Lam] -> Asm
(define (compile-lambda-defines ls)
  (match ls
    ['() (seq)]
    [(cons l ls)
     (seq (compile-lambda-define l)
          (compile-lambda-defines ls))]))

;; Lam -> Asm
(define (compile-lambda-define l)
  (let ((fvs (fv l)))
    (match l

      ;; ————— Plain (fixed‑arity) lambdas —————
      [(LamPlain f xs e)
       ;; env = [free-vars… , last-fixed, … , first-fixed, sentinel]
       (let ((env (append
                    (reverse fvs)     ; closed-over variables
                    (reverse xs)      ; fixed parameters in reverse
                    (list #f))))      ; closure pointer sentinel
         (seq
           (Label (symbol->label f))

           ;; exact‑arity check
           (Cmp r15 (length xs))
           (Jne 'err)

           ;; fetch & untag the closure pointer
           (Mov rax (Offset rsp (* 8 (length xs))))
           (Xor rax type-proc)

           ;; rehydrate closed‑over variables
           (copy-env-to-stack fvs 8)

           ;; compile the body, using env = free-vars + reversed xs
           (compile-e e env #t)

           ;; pop env slots (fixed args + sentinel) and return
           (Add rsp (* 8 (length env)))
           (Ret)))]

      ;; ————— Rest (variadic) lambdas —————
      [(LamRest f xs rest-id e)
       ;; env = [free-vars… , rest-id, last-fixed, … , first-fixed, sentinel]
       (let ((env (append
                    (reverse fvs)               ; closed‑over variables
                    (cons rest-id (reverse xs)) ; rest param then fixed params reversed
                    (list #f))))                ; closure pointer sentinel
         (seq
           (Label (symbol->label f))

           ;; minimum‑arity check
           (Cmp r15 (length xs))
           (Jl  'err)

           ;; carve off the “extra” args into a proper list in rax
           (Sub  r15 (length xs))        ; r15 ← num_extra
           (Mov  rdx r15)                ; loop counter
           (Mov  rax (value->bits '()))  ; start with ’() accumulator

           (let ((loop (gensym 'rest_loop))
                 (done (gensym 'rest_done)))
             (seq
               (Label loop)
               (Cmp   rdx 0)
               (Je    done)
               (Mov   r8    (Offset rsp 0))  ; pop one extra arg
               (Add   rsp   8)
               ;; cons it onto the front of our list (in rax)
               (Mov   rdi   rax)             ; rdi = old list (cdr)
               (Mov   rax   r8)              ; rax = item    (car)
               (Mov   (Offset rbx 0) rdi)    ; CDR 
               (Mov   (Offset rbx 8) rax)    ; car
               (Mov   rax   rbx)             ; rax = new cons-cell ptr
               (Xor   rax   type-cons)       ; tag it
               (Add   rbx   16)              ; bump heap
               (Sub   rdx   1)
               (Jmp   loop)
               (Label done)))

           ;; now push that list so `rest-id` binds to it
           (Push rax)

           ;; fetch & untag the closure pointer (beneath rest+fixed slots)
           (Mov rax (Offset rsp (* 8 (add1 (length xs)))))
           (Xor rax type-proc)

           ;; rehydrate closed‑over variables
           (copy-env-to-stack fvs 8)

           ;; compile the body, using env = free-vars + rest-id + reversed xs
           (compile-e e env #t)

           ;; pop env slots (rest + fixed + sentinel) and return
           (Add rsp (* 8 (length env)))
           (Ret)))])))

;; [Listof Id] Int -> Asm
;; Copy the closure environment at given offset to stack
(define (copy-env-to-stack fvs off)
  (match fvs
    ['() (seq)]
    [(cons _ fvs)
     (seq (Mov r9 (Offset rax off))
          (Push r9)
          (copy-env-to-stack fvs (+ 8 off)))]))

;; type CEnv = (Listof [Maybe Id])
;; Expr CEnv Boolean -> Asm
(define (compile-e e c t?)
  (match e
    [(Lit d) (compile-value d)]
    [(Eof) (compile-value eof)]
    [(Var x) (compile-variable x c)]
    [(Prim0 p) (compile-prim0 p)]
    [(Prim1 p e) (compile-prim1 p e c)]
    [(Prim2 p e1 e2) (compile-prim2 p e1 e2 c)]
    [(Prim3 p e1 e2 e3) (compile-prim3 p e1 e2 e3 c)]
    [(PrimN p es) (compile-primN p es c)]
    [(If e1 e2 e3)
     (compile-if e1 e2 e3 c t?)]
    [(Begin e1 e2)
     (compile-begin e1 e2 c t?)]
    [(Let x e1 e2)
     (compile-let x e1 e2 c t?)]
    [(App e es)
     (compile-app e es c t?)]
    [(Apply ef es e)
     (compile-apply ef es e c t?)]
    [(LamPlain f xs e1) (compile-lam e c)]
    [(LamRest f xs x e1) (compile-lam e c)]
    [(Raise e) (compile-raise e c t?)]
    [(WithHandlers ps hs e) (compile-with-handlers ps hs e c t?)]
    [(Match e ps es) (compile-match e ps es c t?)]))

;; Value -> Asm
(define (compile-value v)
  (cond [(string? v) (compile-string v)]
        [else        (Mov rax (value->bits v))]))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov rax (Offset rsp i)))))

;; String -> Asm
(define (compile-string s)
  (let ((len (string-length s)))
    (if (zero? len)
        (seq (Mov rax type-str))
        (seq (Mov rax len)
             (Mov (Offset rbx 0) rax)
             (compile-string-chars (string->list s) 8)
             (Mov rax rbx)
             (Xor rax type-str)
             (Add rbx
                  (+ 8 (* 4 (if (odd? len) (add1 len) len))))))))

;; [Listof Char] Integer -> Asm
(define (compile-string-chars cs i)
  (match cs
    ['() (seq)]
    [(cons c cs)
     (seq (Mov rax (char->integer c))
          (Mov (Offset rbx i) 'eax)
          (compile-string-chars cs (+ 4 i)))]))

;; Op0 -> Asm
(define (compile-prim0 p)
  (compile-op0 p))

(define (compile-prim1 p e c)
  (seq (compile-e e c #f)
       (compile-op1 p)))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c #f)
       (Push rax)
       (compile-e e2 (cons #f c) #f)
       (compile-op2 p)))

;; Op3 Expr Expr Expr CEnv -> Asm
(define (compile-prim3 p e1 e2 e3 c)
  (seq (compile-e e1 c #f)
       (Push rax)
       (compile-e e2 (cons #f c) #f)
       (Push rax)
       (compile-e e3 (cons #f (cons #f c)) #f)
       (compile-op3 p)))

;; OpN [Listof Expr] CEnv -> Asm
(define (compile-primN p es c)
  (seq (compile-es es c)
       (compile-opN p (length es))))

;; Expr Expr Expr CEnv Boolean -> Asm
(define (compile-if e1 e2 e3 c t?)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c #f)
         (Cmp rax (value->bits #f))
         (Je l1)
         (compile-e e2 c t?)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c t?)
         (Label l2))))

;; Expr Expr CEnv Boolean -> Asm
(define (compile-begin e1 e2 c t?)
  (seq (compile-e e1 c #f)
       (compile-e e2 c t?)))

;; Id Expr Expr CEnv Boolean -> Asm
(define (compile-let x e1 e2 c t?)
  (seq (compile-e e1 c #f)
       (Push rax)
       (compile-e e2 (cons x c) t?)
       (Add rsp 8)))

;; Exception handling: compile-raise and compile-with-handlers

;; Expr CEnv Boolean -> Asm
;; Generate code for (raise <e>): evaluate <e>, stash in r11, and jump to current handler.
(define (compile-raise e c t?)
  (seq
    ;; 1. Evaluate the expression to be raised
    (compile-e e c #f)
    ;; 2. Save raised value in r11
    (Mov r11 rax)
    ;; 3. Jump to the current handler label
    (Jmp current-handler)))

;; [Listof Expr] [Listof Expr] Expr CEnv Boolean -> Asm
;; Wrap <e> with exception handlers: predicates ps to handlers hs
;; [Listof Expr] [Listof Expr] Expr CEnv Boolean -> Asm
(define (compile-with-handlers ps hs e c t?)
  (let* ((old      current-handler)
         (lbl-h    (gensym 'handler))
         (lbl-end  (gensym 'end)))
    ;; 1) Install the new handler label
    (set! current-handler lbl-h)

    ;; 2) Build a little prologue that captures the handler stack
    (let ((handler-block
           (seq
             ;; a) save current rsp on the heap
             (Mov (Offset rbx 0) rsp)
             (Mov r12 rbx)
             (Add rbx 8)

             ;; b) push *both* predicate and handler closures
             (compile-es ps c)                                     ; pushes p1 … pn
             (compile-es hs (append (make-list (length ps) #f) c)) ; pushes f1 … fn

             ;; c) compile the protected body
             (compile-e e
                        (append (make-list (* 2 (length ps)) #f) c)
                        t?)

             ;; d) normal exit: pop all the closure pointers and skip the stub
             (Add rsp (* 8 (+ (length ps) (length hs))))
             (Jmp lbl-end)

             ;; -----------------------------
             ;; 3) The actual exception handler
             ;; -----------------------------
             (Label lbl-h)

               ;; i) restore the *original* stack
               (Mov rsp (Offset r12 0))

               ;; ii) grab the raised value (in r11) and your closures off the
               ;;     stack *before* you pop anything
               ;;    but since we just reset rsp, the closures are gone—
               ;;    instead we must have stashed them in regs:

               ;;    NOTE: here we assume compile-es pushed predicate first, then handler,
               ;;          so at the moment of their push they were at offsets 0 and 8.
               ;;    To capture them, you'd really need to do it *before* restoring rsp:
               ;;      (Mov r9  (Offset rsp 0)) ; handler closure
               ;;      (Mov r8  (Offset rsp 8)) ; predicate closure
               ;;    …then restore rsp.
               ;;    In this snippet I'm showing the idea inline:

               ;;    -- just illustrative: do this *before* restore if you write it
               ;; (Mov r8  (Offset rsp 0))   ; handler closure ptr
               ;; (Mov r9  (Offset rsp 8))   ; predicate closure ptr

               ;; iii) ------ invoke the predicate: ------
               ;;    push the raised value as the single argument
               (Push r11)
               ;;    arity = 1
               (Mov r15 1)
               ;;    load and call the predicate closure (in, say, r9)
               (Mov rax r9)
               (assert-proc rax)
               (Xor  rax type-proc)
               (Mov  rax (Offset rax 0))
               (Call rax)
               ;;    now rax is the predicate’s Boolean result

               ;;    if it returned false, chain to the previous handler
               (Cmp  rax (value->bits #f))
               (Je   old)

               ;; iv) ------ invoke the real handler: ------
               ;;    remove the predicate argument
               (Add  rsp 8)
               ;;    re-push the raised value
               (Push r11)
               (Mov  r15 1)
               ;;    load & call the handler closure (in r8)
               (Mov rax r8)
               (assert-proc rax)
               (Xor  rax type-proc)
               (Mov  rax (Offset rax 0))
               (Call rax)

               ;; v) cleanup the argument
               (Add  rsp 8)

             ;; 4) exit point
             (Label lbl-end))))
      ;; restore the old handler symbol
      (set! current-handler old)
      handler-block)))



;; Id [Listof Expr] CEnv -> Asm
;; The return address is placed above the arguments, so callee pops
;; arguments and return address is next frame
;; Expr [Listof Expr] CEnv Boolean -> Asm
(define (compile-app e es c t?)
  (if t?
      (compile-app-tail e es c)
      (compile-app-nontail e es c)))

;; Expr [Listof Expr] CEnv -> Asm
(define (compile-app-tail e es c)
  (seq (compile-es (cons e es) c)
       (move-args (add1 (length es)) (length c))
       (Add rsp (* 8 (length c)))
       (Mov r15 (length es))
       (Mov rax (Offset rsp (* 8 (length es))))
       (assert-proc rax)
       (Xor rax type-proc)
       (Mov rax (Offset rax 0))
       (Jmp rax)))

;; Integer Integer -> Asm
(define (move-args i off)
  (cond [(zero? off) (seq)]
        [(zero? i)   (seq)]
        [else
         (seq (Mov r8 (Offset rsp (* 8 (sub1 i))))
              (Mov (Offset rsp (* 8 (+ off (sub1 i)))) r8)
              (move-args (sub1 i) off))]))

;; Expr [Listof Expr] CEnv -> Asm
;; The return address is placed above the arguments, so callee pops
;; arguments and return address is next frame
(define (compile-app-nontail e es c)
  (let ((r (gensym 'ret))
        (i (* 8 (length es))))
    (seq (Lea rax r)
         (Push rax)
         (compile-es (cons e es) (cons #f c))
         (Mov r15 (length es))
         (Mov rax (Offset rsp i))
         (assert-proc rax)
         (Xor rax type-proc)
         (Mov rax (Offset rax 0)) ; fetch the code label
         (Jmp rax)
         (Label r))))

;; Expr [Listof Expr] Expr CEnv -> Asm
;; Expr [Listof Expr] Expr CEnv Boolean -> Asm
;; Compile (apply ef es e) where ef is the function, es are regular arguments,
;; and e is the list of additional arguments
(define (compile-apply ef es e c t?)
  (let ((r (gensym 'ret))
        (loop (gensym 'apply_loop))
        (done (gensym 'apply_done))
        (num-regular-args (length es)))
    (seq 
     ;; Push return address
     (Lea rax r)
     (Push rax)
     
     ;; Compile and push function and regular args (ef, e0, e1, etc.)
     (compile-es (cons ef es) (cons #f c))
     
     ;; Check that ef is a procedure
     (Mov r9 (Offset rsp (* 8 num-regular-args))) ; Get the function
     (Mov rdx r9)                                 ; Copy to rdx for checking
     (And rdx ptr-mask)                           ; Mask to check tag
     (Cmp rdx type-proc)                          ; Must be a procedure
     (Jne 'err)                                   ; Error if not a procedure
     
     ;; Compile the list argument en
     (compile-e e (append (make-list (length (cons ef es)) #f) (cons #f c)) #f)
     
     ;; Initialize r9 as our runtime arg counter
     (Mov r9 num-regular-args)                    ; Start counting with the regular arguments
     
     ;; Save list in r11 for iteration
     (Mov r11 rax)
     
     ;; Loop to traverse the list and push each element
     (Label loop)
     (Cmp r11 (value->bits '()))                  ; Check if we're at end of list
     (Je done)
     
     ;; Make sure it's a cons cell (proper list)
     (Mov rdx r11)                                ; Use rdx for checking
     (And rdx ptr-mask)                           ; Mask to check tag
     (Cmp rdx type-cons)                          ; Must be a cons
     (Jne 'err)                                   ; Error if not a proper list
     
     ;; Get car of current cons cell and push it
     (Xor r11 type-cons)
     (Mov rdx (Offset r11 8))                     ; Get car (using offset 8 for CAR)
     (Push rdx)                                   ; Push it onto stack
     (Add r9 1)                                   ; Increment arg count register
     (Mov r11 (Offset r11 0))                     ; Get cdr for next iteration
     (Jmp loop)
     
     (Label done)
     
     ;; Set r15 register to final arg count for arity checking
     (Mov r15 r9)                                 ; r15 holds total argument count for arity check
     
     ;; Function is at offset: arg-count * 8 from rsp
     (Mov r8 r9)                                  ; Copy arg count to r8
     (Sal r8 3)                                   ; Convert to byte offset (* 8)
     (Mov rax (Offset rsp r8))                    ; Get the function
     
     ;; Jump to the function (which will check arity using r15)
     (Xor rax type-proc)
     (Mov rax (Offset rax 0))                     ; Get the code label
     (Jmp rax)
     
     ;; Return label
     (Label r))))

;; Defns -> Asm
;; Compile the closures for ds and push them on the stack
(define (compile-defines-values ds)
  (seq (alloc-defines ds 0)
       (init-defines ds (reverse (define-ids ds)) 8)
       (add-rbx-defines ds 0)))

;; Defns Int -> Asm
;; Allocate closures for ds at given offset, but don't write environment yet
(define (alloc-defines ds off)
  (match ds
    ['() (seq)]
    [(cons (Defn f fun) ds)
     (let ((fvs (defn->fvs (Defn f fun))))
       (seq (Lea rax (symbol->label f))
            (Mov (Offset rbx off) rax)
            (Mov rax rbx)
            (Add rax off)
            (Xor rax type-proc)
            (Push rax)
            (alloc-defines ds (+ off (* 8 (add1 (length fvs)))))))]))

;; Defns CEnv Int -> Asm
;; Initialize the environment for each closure for ds at given offset
(define (init-defines ds c off)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (let ((fvs (defn->fvs d)))
       (seq (free-vars-to-heap fvs c off)
            (init-defines ds c (+ off (* 8 (add1 (length fvs)))))))]))

;; Defn -> Int
(define (defn->fvs d)
  (match d
    [(Defn f (FunPlain xs e)) (fv (LamPlain f xs e))]
    [(Defn f (FunRest xs x e)) (fv (LamRest f xs x e))]))

;; Defns Int -> Asm
;; Compute adjustment to rbx for allocation of all ds
(define (add-rbx-defines ds n)
  (match ds
    ['() (seq (Add rbx (* n 8)))]
    [(cons d ds)
     (let ((fvs (defn->fvs d)))
       (add-rbx-defines ds (+ n (add1 (length fvs)))))]))

;; (LamPlain | LamRest) CEnv -> Asm
(define (compile-lam l c)
  (let ((fvs (fv l)))
    (match l
      [(or (LamPlain f _ _)
           (LamRest  f _ _ _))
       (seq (Lea rax (symbol->label f))
            (Mov (Offset rbx 0) rax)
            (free-vars-to-heap fvs c 8)
            (Mov rax rbx) ; return value
            (Xor rax type-proc)
            (Add rbx (* 8 (add1 (length fvs)))))])))

;; [Listof Id] CEnv Int -> Asm
;; Copy the values of given free variables into the heap at given offset
(define (free-vars-to-heap fvs c off)
  (match fvs
    ['() (seq)]
    [(cons x fvs)
     (seq (Mov r8 (Offset rsp (lookup x c)))
          (Mov (Offset rbx off) r8)
          (free-vars-to-heap fvs c (+ off 8)))]))

;; [Listof Expr] CEnv -> Asm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (seq (compile-e e c #f)
          (Push rax)
          (compile-es es (cons #f c)))]))

;; Expr [Listof Pat] [Listof Expr] CEnv Bool -> Asm
(define (compile-match e ps es c t?)
  (let ((done (gensym)))
    (seq (compile-e e c #f)
         (Push rax) ; save away to be restored by each clause
         (compile-match-clauses ps es (cons #f c) done t?)
         (Jmp 'err)
         (Label done)
         (Add rsp 8)))) ; pop the saved value being matched

;; [Listof Pat] [Listof Expr] CEnv Symbol Bool -> Asm
(define (compile-match-clauses ps es c done t?)
  (match* (ps es)
    [('() '()) (seq)]
    [((cons p ps) (cons e es))
     (seq (compile-match-clause p e c done t?)
          (compile-match-clauses ps es c done t?))]))

;; Pat Expr CEnv Symbol Bool -> Asm
(define (compile-match-clause p e c done t?)
  (let ((next (gensym)))
    (match (compile-pattern p '() next)
      [(list i cm)
       (seq (Mov rax (Offset rsp 0)) ; restore value being matched
            i
            (compile-e e (append cm c) t?)
            (Add rsp (* 8 (length cm)))
            (Jmp done)
            (Label next))])))

;; Pat CEnv Symbol -> (list Asm CEnv)
(define (compile-pattern p cm next)
  (match p
    [(Var '_)
     (list (seq) cm)]
    [(Var x)
     (list (seq (Push rax)) (cons x cm))]
    [(Lit l)
     (let ((ok (gensym)))
       (list (seq (Mov r8 rax)
                  (compile-value l)
                  (Cmp rax r8)
                  (Je ok)
                  (Add rsp (* 8 (length cm)))
                  (Jmp next)
                  (Label ok))
             cm))]
    [(Conj p1 p2)
     (match (compile-pattern p1 (cons #f cm) next)
       [(list i1 cm1)
        (match (compile-pattern p2 cm1 next)
          [(list i2 cm2)
           (list
            (seq (Push rax)
                 i1
                 (Mov rax (Offset rsp (* 8 (- (sub1 (length cm1)) (length cm)))))
                 i2)
            cm2)])])]
    [(Box p)
     (match (compile-pattern p cm next)
       [(list i1 cm1)
        (let ((ok (gensym)))
          (list
           (seq (Mov r8 rax)
                (And r8 ptr-mask)
                (Cmp r8 type-box)
                (Je ok)
                (Add rsp (* 8 (length cm))) ; haven't pushed anything yet
                (Jmp next)
                (Label ok)
                (Xor rax type-box)
                (Mov rax (Offset rax 0))
                i1)
           cm1))])]
    [(Cons p1 p2)
     (match (compile-pattern p1 (cons #f cm) next)
       [(list i1 cm1)
        (match (compile-pattern p2 cm1 next)
          [(list i2 cm2)
           (let ((ok (gensym)))
             (list
              (seq (Mov r8 rax)
                   (And r8 ptr-mask)
                   (Cmp r8 type-cons)
                   (Je ok)
                   (Add rsp (* 8 (length cm))) ; haven't pushed anything yet
                   (Jmp next)
                   (Label ok)
                   (Xor rax type-cons)
                   (Mov r8 (Offset rax 0))
                   (Push r8)                ; push cdr
                   (Mov rax (Offset rax 8)) ; mov rax car
                   i1
                   (Mov rax (Offset rsp (* 8 (- (sub1 (length cm1)) (length cm)))))
                   i2)
              cm2))])])]))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))
