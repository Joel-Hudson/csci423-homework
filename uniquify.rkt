#lang racket
(require "utilities.rkt")


(trace-define (uniquify-exp symtab)
  (lambda (e)
    (match e
      [(Var x) (Var (dict-ref symtab x))]
      [(Int n) (Int n)]
      [(Let x e body) (define new-x (gensym x))
                      (define new-env (dict-set symtab x new-x))
                      (define new-e ((uniquify-exp new-env) e))
                      (define new-body ((uniquify-exp new-env) body)      
         (Let new-x new-e new-body)]
      [(Prim op es)
       (Prim op (for/list ([e es]) ((uniquify-exp symtab) e)))])))

(define (uniquify p)
(match p
[(Program info e)
(Program info ((uniquify-exp '()) e))]))
                      
