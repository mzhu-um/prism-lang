#lang racket
(require (for-syntax racket syntax/parse) racket/trace)
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(provide print-prog)

(define INDENT-SIZE 2)
(define (indent-decr i) (- i INDENT-SIZE))
(define (indent-incr i) (+ i INDENT-SIZE))

(define (println/indent i)
  (lambda (s) (printf "~a" (make-string i #\space)) (printf "~a~n" s)))

(define (close v)
  (match v
    ['module 'endmodule]))

(define (print-prog prog)
  (define (print/indent prog indent store)
    (define p (println/indent indent))
    (define p/s (λ (s) (p (escape-id s))))
    (match prog
      [`(,fst . ,prog)
       (let ([store
              (match fst
                [`(define ,name ,num)
                 (eval fst ns)
                 (cons (cons name (eval num ns)) store)]
                [`(module ,name (rename ,name2 (,id1 ,id2) ...))
                 (p (format "module ~a = ~n  ~a [~a]~nendmodule~n"
                            (escape-id name)
                            (escape-id name2)
                            (string-join
                             (map (λ (id1 id2)
                                    (format "~a = ~a"
                                            (escape-id id1)
                                            (escape-id id2)))
                                  id1 id2)
                             ", ")))
                 store]
                [`(,(? (λ (v) (memq v '(module))) v) ,name . ,in-module)
                 (p (string-append (escape-id v) " " (escape-id name)))
                 (print/indent in-module (indent-incr indent) store)
                 (p/s (close v))
                 (p "")
                 store]
                [`(init . ,in-clause)
                 (print-init/indent store in-clause indent)
                 store]
                [`(const . ,in-clause)
                 (print-const/indent store in-clause indent)
                 (p "")
                 store]
                [`(-> . ,in-clause)
                 (print-tran/indent store in-clause indent)
                 store]
                [`(rewards ,name . ,in-clause)
                 (p (string-append "rewards" " " (format "\"~a\""(escape-id name))))
                 (print-rewards/indent store in-clause (indent-incr indent))
                 (p/s 'endrewards)
                 (p "")
                 store]
                [_ (p "")])])
         (print/indent prog indent store))]
      [`()
       (void)]))
  (match prog
    [`((mode ,mode) . ,prog)
     (printf "~a~n~n" (escape-id mode))
     (print/indent prog 0 '())]
    [prog
     (print/indent prog 0 '())]))


(define (show-id id)
  (if (symbol? id) (escape-id id)
      (error 'prism-lang "Expecting an identifier, given [~a]" id)))

(define (lookup/ρ/any x ρ)
  (if (symbol? x)
      (lookup/ρ x ρ x)
      x))

(define (lookup/ρ x ρ default)
  (cond [(empty? ρ) default]
        [else (if (eq? x (caar ρ)) (cdar ρ) (lookup/ρ x (cdr ρ) default))]))

(define (show-val/pure val)
  (show-val val '()))

(define (escape-id id)
  (string-replace (symbol->string id) "-" "_"))

(define (show-val val ρ)
  (match val
    [`(eval ,k) (number->string (eval k ns))]
    [(? number? i) (number->string i)]
    [(? symbol? id)
     (lookup/ρ id ρ (escape-id id))]
    [(or 'true #t) "true"]
    [(or 'false #f) "false"]
    [`(add1 ,v1) (show-val `(+ ,v1 1) ρ)]
    [`(sub1 ,v1) (show-val `(- ,v1 1) ρ)]
    [`(,op1 ,v1)
     (format "~a ~a" (show-op1 op1) (show-val v1 ρ))]
    [`(:= ,v1 ,v2)
     (format "(~a' = ~a)" (show-id v1) (show-val v2 ρ))]
    [`(,(? symbol? op) ,v1 ,v2)
     (format "~a ~a ~a" (show-val v1 ρ) (show-op op) (show-val v2 ρ))]
    [`(,(or 'if '?) ,v1 ,v2 ,v3)
     (format "~a ? ~a : ~a" (show-val v1 ρ) (show-val v2 ρ) (show-val v3 ρ))]
    [`(,(? symbol? op) ,vs ...)
     (let ([sep (string-append " " (show-op op) " ")])
      (apply (curry format (string-join (make-list (length vs) "~a") sep))
             (map (λ (v) (show-val v ρ)) vs)))]
    [_ (error 'prism-lang "Expecting a value, given [~a]" val)]))

(define (show-op1 op)
  (match op
    [(or '-) (symbol->string op)]
    [(or '! 'not) "!"]
    [_ (error 'prism-lang "unsupported monop: [~a]" op)]))

(define (show-op op)
  (match op
    [(or 'and '&& '&) "&"]
    [(or 'or) "|"]
    [(or '- '* '/ '+ '= '!= '< '<= '> '>= '<=> '=>) (symbol->string op)]
    ['iff ("<=>")]
    [_ (error 'prism-lang "unsupported binop: [~a]" op)]))

(define (show-ty ρ ty)
  (match ty
    ['int "int"]
    ['bool "bool"]
    ['double "double"]
    [`(range ,n1 ,n2)
     (format "[~a..~a]" (show-val n1 ρ) (show-val n2 ρ))]
    [_ (error 'prism-lang "Expecting a type expression, given [~a]" ty)]))


(define (show-init1 clause ρ)
  (match clause
    [`(,id ,ty ,val)
     (format "~a : ~a init ~a;" (escape-id id) (show-ty ρ ty) (show-val val ρ))]
    [`(,id clock)
     (format "~a : clock;" id)]
    [_ (error 'prism-lang "Expecting a init clause, given [~a]" clause)]))

(define (show-labels labels)
  (string-join (map escape-id labels) ","))

(define (show-dests dests ρ indent)
  (let* ([indent (make-string indent #\space)]
         [pretty+ (string-append "\n" indent "   + ")])
   (match dests
     [`(for [,(? symbol? n) ,binder] ,dest)
      (let* ([lst
              (match binder
                [(or `(range ,_ ,_) `(list ,_ ...))
                 (eval binder ns)]
                [_ (error 'prism-lang "unsupported for binder form [~a]"
                          binder)])]
             [_ (if (not (and (list? lst) (andmap integer? lst)))
                    (error 'prism-lang
                           "[~a] doesn't evaluates to a valid integer list"
                           binder)
                    '())])
        (string-join
         (map
          (lambda (times)
            (let ([ρ (cons (cons n times) ρ)])
              (match dest
                [`(_ ,action ...) (format "~a" (show-actions action ρ))]
                [`(,prob ,action ...)
                 (format "~a : ~a"(show-val prob ρ) (show-actions action ρ))])))
          lst)
         pretty+))]

     [_ (string-join
         (map
          (match-lambda
            [`(_ ,action ...)
             (format "~a"
                     (show-actions action ρ))]
            [`(,prob ,action ...)
             (format "~a : ~a"
                     (show-val prob ρ)
                     (show-actions action ρ))])
          dests)
         pretty+)])))

(define (show-actions action ρ)
  (string-join
   (map (λ (action) (show-val action ρ))
        action)
   " & "))

(define (show-tran1 clause ρ)
  (match clause
    [`(,labels ,from ,dests)
     (let ([pretext
            (format "[~a] ~a "
                    (show-labels labels)
                    (show-val from ρ))])
       (format "~a-> ~a;"
               pretext
               (show-dests dests ρ (string-length pretext))))
]
    [_ (error 'prism-lang
              "Expecting a transition clause,\nGiven [~a]"
              clause)]))

(define (show-const1 clause ρ)
  (match clause
    [`(,id ,ty ,val)
     (format "const ~a  ~a = ~a;"
             (show-ty ρ ty)
             (escape-id id)
             (show-val val ρ))]))

(define (show-reward1 clause ρ)
  (match clause
    [`(,actions ,state ,reward)
     (format "[~a] ~a : ~a;"
             (show-labels actions)
             (show-val state ρ)
             (show-val reward ρ))]
    [_ (error 'prism-lang
              "Expecting a reward clause,\nGiven [~a]"
              clause)]))

(define (print-init/indent store clauses indent)
  (map (λ (clause) ((println/indent indent) (show-init1 clause store)))
       clauses))

(define (print-tran/indent store clauses indent)
  (map (λ (clause) ((println/indent indent) (show-tran1 clause store)))
       clauses))

(define (print-const/indent store clauses indent)
  (map (λ (clause) ((println/indent indent) (show-const1 clause store)))
       clauses))

(define (print-rewards/indent store clauses indent)
  (map (λ (clause) ((println/indent indent) (show-reward1 clause store)))
       clauses))


#|
[`(for [,(? symbol? n) (range ,from ,to)] ,dest)
(let* ([from (eval from)]
[to (eval to)]
[_ (if (> from to)
(error 'prism-lang
"[~a] [~a] doesn't evaluates to a valid range"
from to)
'())])
(string-join
(build-list
(- to from)
(lambda (times)
(let ([ρ (cons (cons n (from . + . times)) ρ)])
(match dest
[`(_ ,action) (format "(~a)" (show-val action ρ))]
[`(,prob ,action)
(format "~a : (~a)"(show-val prob ρ) (show-val action ρ))]))))
" + "))]
|#
