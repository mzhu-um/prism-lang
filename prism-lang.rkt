#lang racket
(require (for-syntax racket racket/pretty)
         "prism-compile.rkt")
(provide (rename-out [module-begin #%module-begin])
         #%top-interaction)

(define-namespace-anchor lang)
(define ns-lang (namespace-anchor->namespace lang))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ e ...)
     #`(#%module-begin
        (for-each (match-lambda
                    [(and `(define ,_ ,_) s)
                     (eval s ns-lang)]
                    [_ (void)])
                  `(e ...))
        (define data (eval `(quasiquote (e ...)) ns-lang))
        ;; (displayln data)
        ;; (displayln data (current-error-port))
        (print-prog data)
        )]))

