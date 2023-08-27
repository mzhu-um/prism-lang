#lang racket
(require
 (for-label racket/base)
 scribble/bettergrammar)
(provide (all-defined-out))

;;; Top Level

;;; Mode Specification
(define-grammar prism-grammar
  #:literals (mdp dtmc ctmc pta pomdp popta real? integer?)
  #:datum-literals
  (mode module -> init for range int double bool true false
        := & && and or + - * / <=> ! not != ? if list define _
        rewards)
  
  (model-def
   (code:line
    mode-def
    toplevel-def ...))

  (mode-def (mode mode-tag))

  (toplevel-def const-def module-def sconst-def rewards-def)

  (sconst-def
   (define id integer?))

  (const-def
   (const binding ...))
  (binding
   (id type value))

  (module-def
   (module id
     conf-def ...))
  (conf-def
   inits-def
   cmd-def)

  (inits-def
   (init binding ...))

  (cmds-def
   (-> cmd-def ...) )

  (cmd-def
   (actions c-expr steps))

  (steps
   (step ...)
   (for [id (range integer? integer?)] step)
   (for [id (list integer? ...)] step))


  (step
   [prob c-expr ...]
   [_ c-expr ...])

  (c-expr
    (op1 c-expr)   
    (op2 c-expr c-expr)
    (op3 c-expr c-expr c-expr))

  (op1 - ! not)
  (op2 := & && and or - * / + - != <=> =>)
  (op3 ? if)

  (mode-tag mdp dtmc ctmc pta pomdp popta)


  (prob
   real?)

  (actions
   (id ...))

  (type
   (range integer? integer?)
   bool
   int
   double)

  (bval
   true
   false
   #t
   #f)

  (rewards-def
   (rewards id
            [actions c-expr c-expr])))


;;; Module Specification
