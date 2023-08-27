#lang racket
(require "prism-lang.rkt")
(provide (all-from-out "prism-lang.rkt"))

(module reader syntax/module-reader
  prism-lang)
