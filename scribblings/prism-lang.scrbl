#lang scribble/manual

@(require
  scribble/bettergrammar
  prism-lang/prism-spec)

@title{S-Exp PRISM}

@defmodule[prism-lang]

@section{Lexical Specification}
@subsection{Model Language}
@bettergrammar*[prism-grammar]

@subsection{TODO}
@(define papers
   `(("Macro and Evaluation")
     (("unquote form to be used to deal with evaluation"))
     (("Unfold the define form as let binding in syntax stage"))
     (,(linebreak))
     ))

@(define (papers->rows p)
   (map (lambda (r) (if (string? (car r)) `bottom-border `())) p))

@tabular[#:style 'boxed
         #:row-properties (papers->rows papers)
         papers]
