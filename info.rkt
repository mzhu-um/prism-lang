#lang info
(define collection "prism-lang")
(define deps '("base" "scribble-bettergrammar"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/prism-lang.scrbl" ())))
(define pkg-desc "Parenthesized Language for PRISM Model Checker")
(define version "0.0")
(define pkg-authors '(mzhu))
(define license '(Apache-2.0 OR MIT))
